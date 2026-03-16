import Agent.StreamFormat
import Lean.Data.Json

open Lean (Json)

namespace Agent.Sandbox

/-- System paths that need read+execute (contain binaries/libraries). -/
private def roxPaths : List String :=
  [ "/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix" ]

/-- System paths that need read-only access (config, etc). -/
private def roPaths : List String :=
  [ "/etc", "/run", "/dev", "/proc", "/sys" ]

/-- System paths that need read-write access. -/
private def rwPaths : List String :=
  [ "/dev/null" ]

/-- Get additional read-only-execute paths (home-relative). -/
private def homeRoxPaths : IO (List System.FilePath) := do
  match ← IO.getEnv "HOME" with
  | some h =>
    let home := System.FilePath.mk h
    return [ home / ".elan"
           , home / ".cache"
           , home / ".local" ]
  | none => return []

/-- Home paths that need read-write access. -/
private def homeRwPaths : IO (List System.FilePath) := do
  match ← IO.getEnv "HOME" with
  | some h =>
    let home := System.FilePath.mk h
    return [ home / ".claude"
           , home / ".claude.json"
           , home / ".gitconfig"
           , home / ".config" / "claude"
           , home / ".config" / "gh"
           , home / ".config" / "git" ]
  | none => return []

/--
Launch the coding agent inside a landrun sandbox.
The MCP server (running in the parent process) is exposed to the agent via an MCP config
file written to /tmp. Returns the exit code of the agent process.
-/
private def shellEscape (s : String) : String :=
  if s.any (fun c => c == ' ' || c == '"' || c == '\'' || c == '\\' || c == '$' || c == '`'
                   || c == '(' || c == ')' || c == '!' || c == '&' || c == '|'
                   || c == ';' || c == '\n' || c == '\t') then
    "'" ++ s.replace "'" "'\\''" ++ "'"
  else s

def launchAgent (repoPath : System.FilePath) (prompt : String)
    (serverPort : UInt16)
    (ghToken : String)
    (debug : Bool := false)
    (extraEnv : Array (String × Option String) := #[])
    (pluginDirs : Array String := #[])
    (subAgent : Option String := none) : IO UInt32 := do
  -- Write MCP config: nc bridges claude's stdio to the JSON-RPC TCP server in the parent process.
  -- The parent process holds all secrets; the sandbox only gets a TCP connection to it.
  let mcpConfig := Json.mkObj [("mcpServers", Json.mkObj [
    ("agent", Json.mkObj [
      ("command", .str "nc"),
      ("args", .arr #[.str "127.0.0.1", .str (toString serverPort)])
    ])
  ])]
  let ts ← IO.monoNanosNow
  let mcpConfigPath := System.FilePath.mk s!"/tmp/agent-mcp-{ts}.json"
  IO.FS.writeFile mcpConfigPath mcpConfig.compress
  let mut args : Array String := #[]
  -- Read-write access to the repo and /tmp
  args := args.push "--rwx" |>.push repoPath.toString
  args := args.push "--rw" |>.push "/tmp"
  -- Read+execute system paths (binaries, libraries)
  for p in roxPaths do
    if ← System.FilePath.pathExists p then
      args := args.push "--rox" |>.push p
  -- Read-only system paths
  for p in roPaths do
    if ← System.FilePath.pathExists p then
      args := args.push "--ro" |>.push p
  -- Read-write system paths (e.g. /dev/null)
  for p in rwPaths do
    if ← System.FilePath.pathExists p then
      args := args.push "--rw" |>.push p
  -- Home paths with execute
  for p in ← homeRoxPaths do
    if ← p.pathExists then
      args := args.push "--rox" |>.push p.toString
  -- Home paths read-write (claude config/state)
  for p in ← homeRwPaths do
    if ← p.pathExists then
      args := args.push "--rw" |>.push p.toString
  -- Plugin directories (read+execute access)
  for p in pluginDirs do
    if ← System.FilePath.pathExists p then
      args := args.push "--rox" |>.push p
  -- Network: allow connecting to the local MCP server and external HTTPS
  args := args.push "--connect-tcp" |>.push (toString serverPort)
  args := args.push "--connect-tcp" |>.push "443"
  -- Environment variables for the sandboxed command
  args := args.push "--env" |>.push s!"GH_TOKEN={ghToken}"
  -- Pass through inherited env vars by name
  for name in ["SHELL", "PATH", "HOME", "USER", "TERM"] do
    args := args.push "--env" |>.push name
  -- Extra env vars
  for (k, v) in extraEnv do
    match v with
    | some val => args := args.push "--env" |>.push s!"{k}={val}"
    | none => pure ()
  -- Separator and the actual command
  args := args.push "--"
  args := args.push "claude"
  args := args.push "--print"
  args := args.push "--output-format=stream-json"
  args := args.push "--verbose"
  args := args.push "--dangerously-skip-permissions"
  args := args.push "--mcp-config"
  args := args.push mcpConfigPath.toString
  for p in pluginDirs do
    args := args.push "--plugin-dir"
    args := args.push p
  if let some name := subAgent then
    args := args.push "--agent"
    args := args.push name
  args := args.push "-p"
  args := args.push prompt
  if debug then
    let argsStr := String.intercalate " " (args.toList.map shellEscape)
    IO.eprintln s!"[debug] cd {shellEscape repoPath.toString} && landrun {argsStr}"
  let child ← IO.Process.spawn {
    cmd := "landrun"
    args
    cwd := repoPath
    stdin := .null
    stdout := .piped
    stderr := .piped
  }
  -- Stream stdout, parse stream-json events and format for display
  let outTask ← IO.asTask (prio := .dedicated) do
    let out ← IO.getStdout
    repeat do
      let line ← child.stdout.getLine
      if line.isEmpty then return
      match StreamFormat.formatEvent line with
      | some formatted =>
        out.putStrLn formatted
        out.flush
      | none => pure ()
  -- Stream stderr to console in a background thread
  let errTask ← IO.asTask (prio := .dedicated) do
    let err ← IO.getStderr
    repeat do
      let line ← child.stderr.getLine
      if line.isEmpty then return
      err.putStr line
      err.flush
  -- Wait for streams to drain (EOF when child exits), then collect exit code
  let _ ← IO.wait outTask
  let _ ← IO.wait errTask
  let exitCode ← child.wait
  -- Clean up MCP config
  try IO.FS.removeFile mcpConfigPath catch _ => pure ()
  return exitCode

end Agent.Sandbox
