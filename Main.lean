import Agent
import Cli

open Cli
open Agent

/-- Run a single task: clone, branch, start server, launch agent, optionally create PR. -/
private def runTask (appConfig : AppConfig) (task : Task) (idx : Nat) : IO Unit := do
  IO.println s!"=== Task {idx}: {task.fork} ({repr task.mode}) ==="
  -- 1. Clone / update repo
  IO.println s!"Cloning/updating {task.fork}..."
  let repoPath ← Repo.ensureCloned task.fork task.upstream
  IO.println s!"  Repo at {repoPath}"
  -- 2. Prepare branch
  let branch := task.branch.getD (Repo.generateBranchName task.prompt)
  IO.println s!"  Branch: {branch}"
  Repo.prepareBranch repoPath branch
  -- 3. Create GitHub App token
  IO.println "  Creating GitHub App token..."
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let (forkOwner, _) ← Repo.splitRepo task.fork
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  IO.println "  Token ready"
  -- 4. Start HTTP server
  let serverState : Server.State := {
    upstream := task.upstream
    fork := task.fork
    allowPR := match task.mode with | .pr => true | .fork => false
    refreshToken := do
      let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
      let newToken ← GitHub.createInstallationToken jwt installationId
      GitHub.setupGhAuth newToken
      return newToken
  }
  let (port, shutdown) ← Server.start serverState
  IO.println s!"  Server on port {port}"
  -- 5. Launch agent in sandbox
  IO.println "  Launching agent..."
  let exitCode ← Sandbox.launchAgent repoPath task.prompt port token
  IO.println s!"  Agent exited with code {exitCode}"
  -- 6. Shutdown server
  shutdown
  -- 7. If PR mode, push and create PR
  if let .pr := task.mode then
    if exitCode == 0 then
      IO.println "  Pushing branch and creating PR..."
      let child ← IO.Process.spawn {
        cmd := "git"
        args := #["push", "--force-with-lease", "origin", s!"{branch}:{branch}"]
        cwd := repoPath
        stdout := .piped
        stderr := .piped
      }
      let stderr ← child.stderr.readToEnd
      let pushCode ← child.wait
      if pushCode != 0 then
        IO.eprintln s!"  Push failed: {stderr.trimAscii.toString}"
      else
        let pat ← IO.getEnv appConfig.patEnvVar
        let pat := pat.getD ""
        if pat == "" then
          IO.eprintln s!"  Warning: {appConfig.patEnvVar} not set, skipping PR creation"
        else
          let prUrl ← GitHub.createPullRequest pat task.upstream
            s!"{forkOwner}:{branch}" "main"
            s!"[Agent] {(task.prompt.take 60).toString}"
            task.prompt
          IO.println s!"  PR created: {prUrl}"
    else
      IO.eprintln "  Skipping PR (agent failed)"
  IO.println s!"=== Task {idx} done ===\n"

private def runHandler (p : Parsed) : IO UInt32 := do
  let taskFile := p.positionalArg! "task-file" |>.as! String
  let configPath := p.flag? "config" |>.map (·.as! String)
  let taskIdx := p.flag? "task" |>.map (·.as! Nat)
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let taskFileData ← loadTaskFile taskFile
  if taskFileData.tasks.isEmpty then
    IO.eprintln "No tasks found in task file"
    return 1
  let tasks := match taskIdx with
    | some idx =>
      if h : idx < taskFileData.tasks.size then
        #[taskFileData.tasks[idx]]
      else
        #[]
    | none => taskFileData.tasks
  if tasks.isEmpty then
    IO.eprintln "Task index out of range"
    return 1
  for i in [:tasks.size] do
    try
      runTask appConfig tasks[i]! i
    catch e =>
      IO.eprintln s!"Task {i} failed: {e}"
  return (0 : UInt32)

private def cleanupHandler (_ : Parsed) : IO UInt32 := do
  Repo.cleanup
  return 0

private def runCmd' : Cmd := `[Cli|
  run VIA runHandler; ["0.1.0"]
  "Run coding agent tasks from a task file."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    t, task : Nat; "Run only the task at this index (0-based)"

  ARGS:
    "task-file" : String; "Path to the JSON task file"
]

private def cleanupCmd : Cmd := `[Cli|
  cleanup VIA cleanupHandler; ["0.1.0"]
  "Remove all cloned repositories."
]

private def defaultHandler (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand. Try 'agent --help'."
  return 1

def agentCmd : Cmd := `[Cli|
  agent VIA defaultHandler; ["0.1.0"]
  "CLI tool for managing and sandboxing coding agents."

  SUBCOMMANDS:
    runCmd';
    cleanupCmd
]

def main (args : List String) : IO UInt32 :=
  agentCmd.validate args
