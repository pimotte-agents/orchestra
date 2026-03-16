import Lean.Data.Json

open Lean (Json)

/-
TODO: Refactor this file as follows: Define a type of log messages and implement parsing this type
of log messages from `JSON`. Then add a function that formats a message.
-/

namespace Agent.StreamFormat

private def truncate (s : String) (n : Nat := 150) : String :=
  let s := s.replace "\n" " "
  if s.length ≤ n then s
  else String.ofList (s.toList.take n) ++ "..."

private def jStr (j : Json) (key : String) : String :=
  j.getObjValAs? String key |>.toOption |>.getD ""

private def jVal (j : Json) (key : String) : Option Json :=
  j.getObjVal? key |>.toOption

private def jArr (j : Json) (key : String) : Option (Array Json) :=
  match j.getObjVal? key with
  | .ok (.arr a) => some a
  | _ => none

private def formatToolUse (item : Json) : String :=
  let name := jStr item "name"
  match jVal item "input" with
  | none => s!"[tool] {name}"
  | some inp =>
    let desc := jStr inp "description"
    let cmd := jStr inp "command"
    let fp := jStr inp "file_path"
    let pat := jStr inp "pattern"
    let detail :=
      if !cmd.isEmpty then
        let header := if !desc.isEmpty then s!"{desc}\n" else ""
        s!"{header}  > {truncate cmd}"
      else if !fp.isEmpty then fp
      else if !pat.isEmpty then s!"pattern: {pat}"
      else desc
    s!"[tool] {name}: {detail}"

private def formatContent (item : Json) : Option String :=
  match jStr item "type" with
  | "thinking" =>
    let t := jStr item "thinking"
    if t.isEmpty then none else some s!"[thinking] {truncate t}"
  | "tool_use" => some (formatToolUse item)
  | "text" =>
    let t := jStr item "text"
    if t.isEmpty then none else some s!"[text] {truncate t 300}"
  | _ => none

/-- Format a single stream-json event line for human-readable display.
    Returns `none` if the event should be suppressed. -/
def formatEvent (line : String) : Option String := do
  let json ← (Json.parse line.trimAscii.toString).toOption
  match jStr json "type" with
  | "system" =>
    if jStr json "subtype" == "init" then
      let model := jStr json "model"
      let sid := jStr json "session_id"
      let sidShort := if sid.length > 8 then String.ofList (sid.toList.take 8) ++ "..." else sid
      some s!"[init] session={sidShort} model={model}"
    else some s!"[system] {jStr json "subtype"}"
  | "assistant" =>
    let msg ← jVal json "message"
    let items ← jArr msg "content"
    let item ← items.back?
    formatContent item
  | "user" =>
    let tr ← jVal json "tool_use_result"
    let stdout := jStr tr "stdout"
    let stderr := jStr tr "stderr"
    if stdout.isEmpty && stderr.isEmpty then none
    else
      let outPart :=
        if stdout.isEmpty then ""
        else
          let lines := stdout.splitOn "\n"
          if lines.length > 5 then
            let preview := String.intercalate "\n  " (lines.take 3)
            s!"\n  {preview}\n  ... ({lines.length} lines)"
          else
            s!"\n  {String.intercalate "\n  " lines}"
      let errPart := if stderr.isEmpty then "" else s!"\n  stderr: {truncate stderr}"
      some s!"[output]{outPart}{errPart}"
  | "result" =>
    let sub := jStr json "subtype"
    let turns := match json.getObjValAs? Nat "num_turns" with
      | .ok n => s!" | {n} turns" | .error _ => ""
    let dur := match json.getObjValAs? Nat "duration_ms" with
      | .ok ms => s!" | {ms / 1000}s" | .error _ => ""
    let cost := match jVal json "total_cost_usd" with
      | some v => s!" | ${v.compress}" | none => ""
    let res := jStr json "result"
    let resPart := if res.isEmpty then "" else s!"\n{truncate res 300}"
    some s!"[done] {sub}{turns}{dur}{cost}{resPart}"
  | "rate_limit_event" => none
  | other => some s!"[{other}]"

end Agent.StreamFormat
