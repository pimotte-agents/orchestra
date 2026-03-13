# Implementation Plan

## Overview

A Lean 4 CLI tool that orchestrates coding agents (initially `claude` CLI) working on
GitHub fork-based workflows. The tool manages GitHub App tokens, sandboxes the agent
via `landrun`, and provides a local HTTP server for privileged operations (token refresh,
upstream PR creation) that the agent cannot perform directly.

## Architecture

```
┌─────────────┐
│  User       │  Provides: task file, GitHub App key, PAT
└──────┬──────┘
       │
┌──────▼──────┐
│  agent CLI  │  Parses config, manages repos, runs tasks sequentially
│  (this tool)│
├─────────────┤
│ HTTP Server │  localhost:PORT — serves token-refresh & create-pr endpoints
├─────────────┤
│ landrun     │  Sandboxes the coding agent (file access + network restrictions)
├─────────────┤
│ claude CLI  │  The coding agent, running inside the sandbox
└─────────────┘
```

## Working Directory Layout

```
~/.agent/
  repos/
    <owner>/<repo>/          # Cloned fork repositories
```

The tool creates `~/.agent/repos/` on first run. Each fork is cloned once and reused
across tasks. The upstream is added as a git remote named `upstream`.

## Configuration

### Global Config (`~/.agent/config.toml`)

```toml
[github_app]
app_id = 12345
private_key_path = "/path/to/private-key.pem"

[github]
pat_env = "GITHUB_PAT"       # env var name holding the PAT (default)
```

The PAT is read from an environment variable (never stored in config files).
The private key is referenced by path; the tool reads it but never exposes it
to the agent.

### Task File (passed as CLI argument)

```toml
[[task]]
upstream = "leanprover-community/mathlib4"
fork = "mybot/mathlib4"
mode = "pr"                          # "fork" or "pr"
prompt = "Fix the sorry in Mathlib.Algebra.Group.Defs"
branch = "fix-sorry-group-defs"      # optional, auto-generated if omitted

[[task]]
upstream = "leanprover-community/mathlib4"
fork = "mybot/mathlib4"
mode = "fork"
prompt = "Refactor Mathlib.Tactic.Ring"
```

Fields:
- `upstream`: `owner/repo` of the upstream repository
- `fork`: `owner/repo` of the fork (GitHub App has access)
- `mode`: `"fork"` (work on fork only) or `"pr"` (create PR to upstream when done)
- `prompt`: The prompt passed to the coding agent
- `branch`: Optional branch name; auto-generated from prompt if omitted

## Modules

### 1. `Agent.Config`

Parse global config and task files using `Lake.Toml`.

**Types:**
```
structure AppConfig where
  appId : Nat
  privateKeyPath : FilePath
  patEnvVar : String := "GITHUB_PAT"

structure Task where
  upstream : String        -- "owner/repo"
  fork : String            -- "owner/repo"
  mode : TaskMode          -- .fork | .pr
  prompt : String
  branch : Option String

inductive TaskMode where
  | fork | pr

structure TaskFile where
  tasks : Array Task
```

**Key functions:**
- `loadAppConfig : IO AppConfig` — reads `~/.agent/config.toml`
- `loadTaskFile : FilePath → IO TaskFile` — reads the task file

### 2. `Agent.GitHub`

GitHub App JWT creation and installation token management. All crypto
is done by shelling out to `openssl`. GitHub API calls use `gh` or `curl`.

**Key functions:**
- `createJWT (appId : Nat) (privateKeyPath : FilePath) : IO String`
  — Creates a JWT signed with RS256 via `openssl`
- `getInstallationToken (jwt : String) (installationId : Nat) : IO String`
  — Exchanges JWT for an installation access token
- `getInstallationId (jwt : String) (owner : String) : IO Nat`
  — Finds the installation ID for a given repository owner
- `setupGhAuth (token : String) (host : String := "github.com") : IO Unit`
  — Configures `gh` CLI authentication with the token
- `createPullRequest (pat : String) (upstream fork : String) (head base title body : String) : IO String`
  — Creates a PR on the upstream repo using the PAT, returns PR URL

### 3. `Agent.Repo`

Repository cloning and management.

**Key functions:**
- `ensureCloned (fork upstream : String) (workDir : FilePath) : IO FilePath`
  — Clones the fork if not already present, adds upstream remote, returns repo path
- `prepareBranch (repoPath : FilePath) (branch : String) (base : String := "main") : IO Unit`
  — Fetches upstream, creates a fresh branch from `upstream/<base>`
- `cleanupRepos (workDir : FilePath) : IO Unit`
  — Removes all cloned repositories

### 4. `Agent.Server`

A minimal HTTP server on localhost that the sandboxed agent can call.
Uses `Std.Internal.UV.TCP` for the socket.

**Protocol:** Simple line-based HTTP (enough for `curl` from inside the sandbox).

**Endpoints:**

`POST /refresh-token`
- No request body needed
- Regenerates the GitHub App installation token, reconfigures `gh` auth
- Response: `{"ok": true}`

`POST /create-pr`
- Request body: `{"title": "...", "body": "...", "head": "branch-name", "base": "main"}`
- Only available when task mode is `pr`
- Creates PR on upstream using PAT
- Response: `{"ok": true, "url": "https://github.com/..."}`

`GET /health`
- Response: `{"ok": true}`

**Key types:**
```
structure ServerState where
  appConfig : AppConfig
  currentTask : Task
  repoPath : FilePath
  installationId : Nat
  mut currentToken : String

structure ServerConfig where
  port : UInt16
  state : IO.Ref ServerState
```

**Key functions:**
- `startServer (config : ServerConfig) : IO (IO.Promise Unit)`
  — Starts the HTTP server in background, returns a promise for shutdown
- `stopServer : IO Unit`
  — Signals the server to stop

### 5. `Agent.Sandbox`

Launches the coding agent inside a `landrun` sandbox.

**Sandbox permissions:**
- Read-write access to the repository directory
- Read access to standard system paths (`/usr`, `/lib`, `/etc`, `/bin`, `/nix` etc.)
- Read access to `~/.elan` (for Lean toolchain)
- Network access to localhost only (for the HTTP server) and to github.com
  (for `gh` operations on the fork)
- No access to `~/.agent/config.toml`, the private key, or any PAT

**Key functions:**
- `launchAgent (repoPath : FilePath) (prompt : String) (serverPort : UInt16) (env : Array (String × String)) : IO UInt32`
  — Builds the `landrun` command with appropriate `--rw`, `--ro`, `--dns`
    flags, sets environment variables (including `AGENT_SERVER_PORT`),
    launches `claude` CLI, waits for exit, returns exit code

The `claude` CLI is invoked roughly as:
```
landrun \
  --rw <repoPath> \
  --ro /usr --ro /lib --ro /etc --ro /bin --ro /nix --ro ~/.elan \
  -- claude --print --prompt <prompt>
```

The exact `landrun` flags will be refined during implementation based on
what the `claude` CLI actually needs at runtime.

### 6. `Agent.CLI`

Command-line argument parsing.

**Usage:**
```
agent run <task-file.toml>          # Run all tasks in the file
agent run <task-file.toml> --task N # Run only task N (0-indexed)
agent cleanup                       # Remove all cloned repos
agent version                       # Print version
```

**Key functions:**
- `parseArgs (args : List String) : IO Command`
- Entry point dispatches to the appropriate handler

### 7. `Main`

Entry point. Parses CLI args, loads config, and runs the task loop.

## Task Execution Flow

For each task:

1. **Load config** — Read `~/.agent/config.toml` and the task file
2. **Clone/update repo** — `Agent.Repo.ensureCloned` gets the fork ready
3. **Prepare branch** — Fetch upstream, create working branch
4. **Generate GitHub App token** — Create JWT → get installation token → configure `gh`
5. **Start HTTP server** — Bind to a random available port on localhost
6. **Launch agent in sandbox** — `landrun` with the `claude` CLI, passing:
   - Working directory: the repo path
   - Env: `AGENT_SERVER_PORT=<port>`, `GH_TOKEN=<installation_token>`
   - The prompt from the task config
7. **Wait for agent exit** — Monitor the process
8. **If mode is `pr`** — Check if the agent pushed commits to the branch on the fork,
   then create a PR to the upstream repo using the PAT
9. **Stop HTTP server**
10. **Report result** — Log success/failure

## Dependencies

The tool needs to import `Lake.Toml` for TOML parsing. Add Lake as a dependency
in `lakefile.toml`, or vendor the TOML parser if importing Lake causes issues.

If importing Lake's TOML module proves problematic, fallback: write a minimal
TOML parser supporting only the subset we need (strings, integers, arrays of tables).

For JSON (server protocol), use `Lean.Data.Json` from the standard library.

For TCP, use `Std.Internal.UV.TCP` from the standard library.

## Implementation Order

1. **`Agent.Config`** — Config types and TOML parsing (validates that Lake.Toml import works)
2. **`Agent.GitHub`** — JWT + token management (can be tested standalone)
3. **`Agent.Repo`** — Repo cloning (can be tested standalone)
4. **`Agent.Server`** — HTTP server (can be tested with `curl`)
5. **`Agent.Sandbox`** — Landrun integration (needs all above)
6. **`Agent.CLI`** — CLI parsing
7. **`Main`** — Wire everything together
8. **Testing** — End-to-end test with a real fork

## Open Questions / Risks

1. **Lake.Toml importability** — Can we depend on Lake's TOML parser in a standalone
   project? If not, we need a fallback. First step of implementation should validate this.
2. **`Std.Internal.UV.TCP` stability** — The `Internal` namespace suggests this API may
   change. It's the only TCP option in stdlib though.
3. **`landrun` network policy** — Need to verify that landrun can allow localhost TCP
   while restricting other network access, and that it can allow connections to
   github.com for `gh` operations.
4. **`claude` CLI runtime needs** — The sandbox must provide everything `claude` needs
   (Node.js runtime, config files, etc.). The exact `--ro` paths will need iteration.
5. **Token expiration timing** — GitHub App installation tokens last 1 hour. For long
   tasks, the agent must call `/refresh-token`. We should document this in the agent's
   prompt or provide a wrapper script.
