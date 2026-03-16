import Lean.Data.Json
import Agent.Config
import Init.Data.String.Basic

open Lean (Json FromJson ToJson)

namespace Agent.TaskStore

-- Types

inductive TaskStatus where
  | running
  | completed
  | failed
deriving Repr

instance : ToJson TaskStatus where
  toJson
    | .running   => "running"
    | .completed => "completed"
    | .failed    => "failed"

instance : FromJson TaskStatus where
  fromJson?
    | .str "running"   => .ok .running
    | .str "completed" => .ok .completed
    | .str "failed"    => .ok .failed
    | j => .error s!"expected task status string, got {j}"

structure TaskRecord where
  id            : String
  createdAt     : String
  upstream      : String
  fork          : String
  mode          : TaskMode
  prompt        : String
  sessionId     : Option String := none
  status        : TaskStatus    := .running
  continuesFrom : Option String := none
  series        : Option String := none
deriving Repr

instance : ToJson TaskRecord where
  toJson r :=
    let base : List (String × Json) := [
      ("id",         r.id),
      ("created_at", r.createdAt),
      ("upstream",   r.upstream),
      ("fork",       r.fork),
      ("mode",       ToJson.toJson r.mode),
      ("prompt",     r.prompt),
      ("status",     ToJson.toJson r.status)
    ]
    let fields := base
      |> fun acc => match r.sessionId with
          | none => acc | some s => acc ++ [("session_id", Json.str s)]
      |> fun acc => match r.continuesFrom with
          | none => acc | some s => acc ++ [("continues_from", Json.str s)]
      |> fun acc => match r.series with
          | none => acc | some s => acc ++ [("series", Json.str s)]
    Json.mkObj fields

instance : FromJson TaskRecord where
  fromJson? j := do
    let id           ← j.getObjValAs? String "id"
    let createdAt    ← j.getObjValAs? String "created_at"
    let upstream     ← j.getObjValAs? String "upstream"
    let fork         ← j.getObjValAs? String "fork"
    let mode         ← j.getObjValAs? TaskMode "mode"
    let prompt       ← j.getObjValAs? String "prompt"
    let status       ← j.getObjValAs? TaskStatus "status"
    let sessionId     := j.getObjValAs? String "session_id"     |>.toOption
    let continuesFrom := j.getObjValAs? String "continues_from" |>.toOption
    let series        := j.getObjValAs? String "series"         |>.toOption
    return { id, createdAt, upstream, fork, mode, prompt, status, sessionId, continuesFrom, series }

-- Directories

def tasksDir : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h / ".agent" / "tasks"
  | none   => throw (.userError "HOME not set")

def seriesDir : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h / ".agent" / "series"
  | none   => throw (.userError "HOME not set")

-- ID generation: 16-char lowercase hex from the nanosecond monotonic clock

private def hexChar (n : UInt64) : Char :=
  let d := (n % 16).toNat
  if d < 10 then Char.ofNat (d + '0'.toNat) else Char.ofNat (d - 10 + 'a'.toNat)

def generateId : IO String := do
  let nanos ← IO.monoNanosNow
  let s := toString nanos
  let padding := "0000000000000000"
  let padded := (padding ++ s).takeEnd 16
  return padded.toString

-- Timestamp

def currentIso8601 : IO String := do
  let child ← IO.Process.spawn {
    cmd    := "date"
    args   := #["-u", "+%Y-%m-%dT%H:%M:%SZ"]
    stdout := .piped
    stderr := .null
    stdin  := .null
  }
  let out ← child.stdout.readToEnd
  let _   ← child.wait
  return out.trimAscii.toString

-- Storage

def saveTask (record : TaskRecord) : IO Unit := do
  let dir ← tasksDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / s!"{record.id}.json") (Lean.Json.compress (ToJson.toJson record))

def loadTask (id : String) : IO (Option TaskRecord) := do
  let path := (← tasksDir) / s!"{id}.json"
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok r    => return some r

private def stripJsonExt (name : String) : Option String :=
  let ext := ".json"
  if name.endsWith ext then
    some (name.dropEnd ext.length).toString
  else
    none

/-- Load all task records, sorted by ID descending (newest first). -/
def loadAllTasks : IO (Array TaskRecord) := do
  let dir ← tasksDir
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut records : Array TaskRecord := #[]
  for entry in entries do
    if let some id := stripJsonExt entry.fileName then
      if let some r ← loadTask id then
        records := records.push r
  return records.qsort (fun a b => a.id > b.id)

-- Series pointers

def latestInSeries (seriesName : String) : IO (Option String) := do
  let path := (← seriesDir) / s!"{seriesName}.json"
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return none
  | .ok j    => return j.getObjValAs? String "latest_task_id" |>.toOption

def updateSeriesPointer (seriesName taskId : String) : IO Unit := do
  let dir ← seriesDir
  IO.FS.createDirAll dir
  let path := dir / s!"{seriesName}.json"
  IO.FS.writeFile path (Lean.Json.compress (Json.mkObj [("latest_task_id", taskId)]))

end Agent.TaskStore
