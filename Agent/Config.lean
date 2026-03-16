import Lean.Data.Json

open Lean (Json FromJson ToJson)

namespace Agent

inductive TaskMode where
  | fork
  | pr
deriving Repr, Inhabited

instance : FromJson TaskMode where
  fromJson?
    | .str "fork" => .ok .fork
    | .str "pr" => .ok .pr
    | j => .error s!"expected \"fork\" or \"pr\", got {j}"

instance : ToJson TaskMode where
  toJson
    | .fork => "fork"
    | .pr => "pr"

structure Task where
  upstream : String
  fork : String
  mode : TaskMode
  prompt : String
deriving Repr, Inhabited

instance : FromJson Task where
  fromJson? j := do
    let upstream ← j.getObjValAs? String "upstream"
    let fork ← j.getObjValAs? String "fork"
    let mode ← j.getObjValAs? TaskMode "mode"
    let prompt ← j.getObjValAs? String "prompt"
    return { upstream, fork, mode, prompt }

structure AppConfig where
  appId : Nat
  privateKeyPath : String
  installationId : Option Nat := none
  pat : String := ""
  pluginDirs : Array String := #[]
deriving Repr

instance : FromJson AppConfig where
  fromJson? j := do
    let ghApp ← j.getObjVal? "github_app"
    let appId ← ghApp.getObjValAs? Nat "app_id"
    let privateKeyPath ← ghApp.getObjValAs? String "private_key_path"
    let installationId := ghApp.getObjValAs? Nat "installation_id" |>.toOption
    let pat := (do
      let gh ← j.getObjVal? "github"
      gh.getObjValAs? String "pat"
    ) |>.toOption |>.getD ""
    let pluginDirs := j.getObjValAs? (Array String) "plugin_dirs" |>.toOption |>.getD #[]
    return { appId, privateKeyPath, installationId, pat, pluginDirs }

structure TaskFile where
  tasks : Array Task
deriving Repr

instance : FromJson TaskFile where
  fromJson? j := do
    let tasks ← j.getObjValAs? (Array Task) "tasks"
    return { tasks }

private def expandHome (path : String) : IO System.FilePath := do
  if path.startsWith "~/" then
    match ← IO.getEnv "HOME" with
    | some h => return System.FilePath.mk h / (path.drop 2).toString
    | none => throw (.userError "HOME not set")
  else return .mk path

def loadJsonFile (α : Type) [FromJson α] (path : System.FilePath) : IO α := do
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e => throw (.userError s!"{path}: JSON parse error: {e}")
  | .ok j =>
    match FromJson.fromJson? j with
    | .error e => throw (.userError s!"{path}: {e}")
    | .ok v => return v

def loadAppConfig (path : Option System.FilePath := none) : IO AppConfig := do
  let configPath ← expandHome (path.getD "~/.agent/config.json").toString
  loadJsonFile AppConfig configPath

def loadTaskFile (path : System.FilePath) : IO TaskFile :=
  loadJsonFile TaskFile path

end Agent
