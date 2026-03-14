import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net

open Lean (Json FromJson ToJson)
open Std.Net
open Std.Internal.UV.TCP

namespace Agent.Server

/-- Mutable state for the server, shared with request handlers. -/
structure State where
  upstream : String
  fork : String
  allowPR : Bool
  refreshToken : IO String

/-- Parse a minimal HTTP request and return (method, path, body). -/
private def parseHttpRequest (raw : String) : Option (String × String × String) := do
  let lines := raw.splitOn "\r\n"
  let requestLine ← lines[0]?
  let parts := requestLine.splitOn " "
  let method ← parts[0]?
  let path ← parts[1]?
  -- Find body after empty line
  let body := match raw.splitOn "\r\n\r\n" with
    | [_, b] => b
    | _ => ""
  return (method, path, body)

private def httpResponse (status : String) (body : String) : String :=
  s!"HTTP/1.1 {status}\r\nContent-Type: application/json\r\nContent-Length: {body.length}\r\nConnection: close\r\n\r\n{body}"

private def okResponse (data : Json := .mkObj []) : String :=
  let body := (Json.mkObj [("ok", true), ("result", data)]).compress
  httpResponse "200 OK" body

private def errorResponse (status : String) (msg : String) : String :=
  let body := (Json.mkObj [("ok", false), ("error", .str msg)]).compress
  httpResponse status body

/-- Handle a single HTTP request. Returns the response string. -/
private def handleRequest (state : State) (method path body : String) : IO String := do
  match method, path with
  | "GET", "/health" =>
    return okResponse
  | "POST", "/refresh-token" =>
    let token ← state.refreshToken
    return okResponse (.str token)
  | "POST", "/create-pr" =>
    if !state.allowPR then
      return errorResponse "403 Forbidden" "PR creation not allowed in fork-only mode"
    match Json.parse body with
    | .error e => return errorResponse "400 Bad Request" s!"invalid JSON: {e}"
    | .ok j =>
      let title := j.getObjValAs? String "title" |>.toOption |>.getD "Agent PR"
      let prBody := j.getObjValAs? String "body" |>.toOption |>.getD ""
      let head := j.getObjValAs? String "head" |>.toOption |>.getD ""
      let base := j.getObjValAs? String "base" |>.toOption |>.getD "main"
      if head == "" then
        return errorResponse "400 Bad Request" "missing 'head' (branch name)"
      let ghPat ← IO.getEnv "GITHUB_PAT"
      let ghPat := ghPat.getD ""
      if ghPat == "" then
        return errorResponse "500 Internal Server Error" "GITHUB_PAT not set"
      let child ← IO.Process.spawn {
        cmd := "gh"
        args := #["pr", "create",
          "--repo", state.upstream,
          "--head", s!"{(state.fork.splitOn "/")[0]!}:{head}",
          "--base", base,
          "--title", title,
          "--body", prBody]
        env := #[("GH_TOKEN", some ghPat)]
        stdout := .piped
        stderr := .piped
      }
      let stdout ← child.stdout.readToEnd
      let stderr ← child.stderr.readToEnd
      let code ← child.wait
      if code != 0 then
        return errorResponse "500 Internal Server Error" s!"gh pr create failed: {stderr.trimAscii.toString}"
      return okResponse (.str stdout.trimAscii.toString)
  | _, _ =>
    return errorResponse "404 Not Found" s!"unknown endpoint: {method} {path}"

/-- Await the result of a TCP IO.Promise. -/
private def awaitTcp (p : IO.Promise (Except IO.Error α)) : IO α := do
  let result ← IO.wait p.result!
  match result with
  | .error e => throw e
  | .ok v => return v

/-- Handle a single client connection. -/
private def handleClient (state : State) (client : Socket) : IO Unit := do
  -- Read the request (up to 64KB)
  let data? ← awaitTcp (← client.recv? 65536)
  match data? with
  | none => return  -- EOF
  | some data =>
    let raw := String.fromUTF8! data
    match parseHttpRequest raw with
    | none =>
      let resp := errorResponse "400 Bad Request" "malformed HTTP request"
      let _ ← awaitTcp (← client.send #[resp.toUTF8])
    | some (method, path, body) =>
      let resp ← handleRequest state method path body
      let _ ← awaitTcp (← client.send #[resp.toUTF8])
    let _ ← awaitTcp (← client.shutdown)

/-- Start the HTTP server. Returns (port, shutdown action). -/
def start (state : State) : IO (UInt16 × IO Unit) := do
  let server ← Socket.new
  -- Bind to localhost on port 0 (OS picks a free port)
  let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := 0 }
  server.bind addr
  server.listen 8
  -- Get the actual port
  let localAddr ← server.getSockName
  let port := match localAddr with
    | .v4 a => a.port
    | .v6 a => a.port
  let running ← IO.mkRef true
  -- Accept loop in background
  let _acceptTask ← IO.asTask (prio := .dedicated) do
    while ← running.get do
      match ← IO.wait (← server.accept).result! with
      | .error _ => break
      | .ok client =>
        if !(← running.get) then break  -- shutdown wakeup, discard
        let _ ← IO.asTask (prio := .dedicated) do
          try
            handleClient state client
          catch _ =>
            pure ()
  let shutdown : IO Unit := do
    running.set false
    -- Make a dummy connection to unblock the accept loop.
    -- (cancelAccept drops the promise, causing result! to panic)
    try
      let dummy ← Socket.new
      let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := port }
      let _ ← dummy.connect addr
    catch _ => pure ()
  return (port, shutdown)

end Agent.Server
