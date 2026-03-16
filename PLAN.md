# Implementation Plan: Per-Repository `.agent` Hooks and Validation Loop

## Overview

Add support for a `.agent/` configuration directory inside each cloned repository.
This directory may contain shell scripts that are run at defined points in the task
lifecycle, and an optional `config.json` that controls the validation retry workflow.

## Per-Repository `.agent/` Directory

Scripts are run with the repository root as the working directory, outside the sandbox,
with full access to the host environment (Docker, build systems, etc.).

| File | When | Notes |
|------|------|-------|
| `init.sh` | Once, after first clone | Guarded by `.agent/.initialized` marker file |
| `before.sh` | Before every agent launch (initial + retries) | |
| `validation.sh` | After every agent launch | Exit 0 = pass, non-zero = fail |
| `after.sh` | Once, after the validation loop ends | Runs even if retries exhausted |
| `config.json` | Loaded at task start | Configures retry prompt and max retries |

### `.agent/config.json` Schema

```json
{
  "validation": {
    "retry_prompt": "Validation failed. Please review the issues and fix them.",
    "max_retries": 3
  }
}
```

Both fields are optional. Defaults: `max_retries = 3`, `retry_prompt = "..."`.

If `validation.sh` is absent, the agent runs exactly once (no retries possible) and
`after.sh` runs immediately after.

## Task Execution Flow (updated)

```
1.  ensureCloned            — clone repo if needed
2.  runInitIfNeeded         — run init.sh once (guarded by .agent/.initialized marker)
3.  createJWT / token       — GitHub App auth
4.  startMcpServer          — bind TCP server
5.  loadRepoConfig          — read .agent/config.json (defaults if absent)
6.  loadSystemPrompt        — read system prompt file

    Loop (attempt 0..maxRetries):
      a. runHook "before.sh"
      b. launchAgent         — initial prompt on attempt 0, retry_prompt + --resume on retries
      c. capture session_id  — from stream-json init event (for potential resume)
      d. runValidation       — run validation.sh; if absent → treated as passed
      e. if passed → break

7.  runHook "after.sh"
8.  shutdown MCP server
```

## Changes Required

### 1. New file: `Agent/RepoConfig.lean`

**Types:**

```lean
structure ValidationConfig where
  retryPrompt : String
  maxRetries  : Nat

structure RepoConfig where
  validation : ValidationConfig
```

**Functions:**

- `loadRepoConfig (repoPath : System.FilePath) : IO RepoConfig`
  — Reads `.agent/config.json`; returns defaults if absent or field missing.

- `runHook (repoPath : System.FilePath) (name : String) : IO Unit`
  — Runs `.agent/<name>` with `repoPath` as cwd if the file exists and is executable.
  — Streams stdout/stderr to the console. Throws if exit code is non-zero.

- `runInitIfNeeded (repoPath : System.FilePath) : IO Unit`
  — If `.agent/.initialized` does not exist: runs `init.sh` via `runHook`, then writes
    the marker file.

- `runValidation (repoPath : System.FilePath) : IO Bool`
  — Runs `.agent/validation.sh`. Returns `true` if it passes (exit 0) or is absent,
    `false` if it exits non-zero. Does not throw on failure.

### 2. Modified: `Agent/Sandbox.lean`

- Add `resume : Option String := none` parameter to `launchAgent`.
  When set, passes `--resume <session_id>` to claude.

- Change return type from `IO UInt32` to `IO (UInt32 × Option String)`.
  The second element is the session ID captured from the `Event.init` stream event.

- In the stdout processing loop, when `StreamFormat.parseEvent` returns `Event.init`,
  store the session ID in an `IO.Ref` and include it in the return value.

### 3. Modified: `Agent/Basic.lean` (or `Agent.lean`)

- Add `import Agent.RepoConfig` so it is re-exported from the `Agent` namespace.

### 4. Modified: `Main.lean`

Replace the single `launchAgent` call in `runTask` with the validation loop:

```
let repoConfig ← loadRepoConfig repoPath
runInitIfNeeded repoPath
let mut sessionId : Option String := none
let maxRetries := repoConfig.validation.maxRetries
for attempt in List.range (maxRetries + 1) do
  runHook repoPath "before.sh"
  let prompt := if attempt == 0 then task.prompt else repoConfig.validation.retryPrompt
  let resume := if attempt == 0 then none else sessionId
  let (exitCode, sid) ← Sandbox.launchAgent ... prompt ... (resume := resume) ...
  sessionId := sid
  let valid ← runValidation repoPath
  if valid then break
  if attempt == maxRetries then
    IO.eprintln s!"Task {idx}: validation still failing after {maxRetries} retries"
runHook repoPath "after.sh"
```

## Files Touched

| File | Change |
|------|--------|
| `Agent/RepoConfig.lean` | **New** |
| `Agent/Basic.lean` | Add import |
| `Agent/Sandbox.lean` | New `resume` param; return `(UInt32 × Option String)` |
| `Main.lean` | Replace single launch with validation loop |
| `Agent.lean` | Add export if needed |
