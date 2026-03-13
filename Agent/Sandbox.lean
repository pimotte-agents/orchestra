namespace Agent.Sandbox

/-- Standard read-only paths the agent needs access to. -/
private def roPaths : List String :=
  [ "/usr", "/lib", "/lib64", "/bin", "/sbin", "/etc"
  , "/nix", "/run" ]

/-- Get additional read-only paths (home-relative). -/
private def homeRoPaths : IO (List System.FilePath) := do
  match ← IO.getEnv "HOME" with
  | some h =>
    let home := System.FilePath.mk h
    return [ home / ".elan"
           , home / ".local"
           , home / ".config" / "claude" ]
  | none => return []

/--
Launch the coding agent inside a landrun sandbox.
Returns the exit code of the agent process.
-/
def launchAgent (repoPath : System.FilePath) (prompt : String)
    (serverPort : UInt16)
    (ghToken : String)
    (extraEnv : Array (String × Option String) := #[]) : IO UInt32 := do
  let mut args : Array String := #[]
  -- Read-write access to the repo
  args := args.push "--rw" |>.push repoPath.toString
  -- Read-only system paths
  for p in roPaths do
    if ← System.FilePath.pathExists p then
      args := args.push "--ro" |>.push p
  for p in ← homeRoPaths do
    if ← p.pathExists then
      args := args.push "--ro" |>.push p.toString
  -- Separator and the actual command
  args := args.push "--"
  args := args.push "claude"
  args := args.push "--print"
  args := args.push "--dangerously-skip-permissions"
  args := args.push "-p"
  args := args.push prompt
  -- Build environment
  let mut env := extraEnv
  env := env.push ("GH_TOKEN", some ghToken)
  env := env.push ("AGENT_SERVER_PORT", some (toString serverPort))
  env := env.push ("AGENT_SERVER_URL", some s!"http://127.0.0.1:{serverPort}")
  let child ← IO.Process.spawn {
    cmd := "landrun"
    args
    cwd := repoPath
    env
    stdin := .null
    stdout := .inherit
    stderr := .inherit
    inheritEnv := true
  }
  child.wait

end Agent.Sandbox
