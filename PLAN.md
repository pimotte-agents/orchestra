# Implementation Plan: Listeners

## Overview

Extend the queue daemon with **listener** support. A listener periodically polls a
configurable **source** for new events and, for each new event, enqueues a task
based on a configurable **action template**.

Supported source types:
- `github-issues` — new open issues on a GitHub repository (built-in, implemented in Lean)
- `github-pr-reviews` — new review comments on open pull requests (built-in, implemented in Lean)
- `shell` — standard output of an arbitrary shell script (extensible)

Listeners are activated by passing `--listener-dir <path>` (default:
`~/.agent/listeners/`) to `orchestra queue start`. Each listener runs
synchronously in the existing daemon loop; no new threads are needed.

---

## File System Layout (additions)

```
~/.agent/
  listeners/
    <name>.json          ← one listener config file per listener
    state/
      <name>.json        ← polling state (processed IDs, last-checked timestamp)
```

---

## Data Model

### Listener Config (`~/.agent/listeners/<name>.json`)

```json
{
  "name": "mathlib-issues",
  "source": {
    "type": "github-issues",
    "repo": "leanprover-community/mathlib4",
    "labels": ["good first issue"]
  },
  "action": {
    "upstream": "leanprover-community/mathlib4",
    "fork":     "mybot/mathlib4",
    "mode":     "pr",
    "prompt_template": "Fix GitHub issue #{{issue_number}}: {{title}}\n\n{{body}}",
    "series":   null,
    "backend":  null,
    "model":    null,
    "agent":    null,
    "system_prompt": null
  },
  "interval_seconds": 120
}
```

**Source variants:**

| type | extra fields | event payload variables |
|------|-------------|------------------------|
| `github-issues` | `repo` (required), `labels` (optional filter list) | `{{issue_number}}`, `{{title}}`, `{{body}}`, `{{url}}` |
| `github-pr-reviews` | `repo` (required) | `{{pr_number}}`, `{{pr_title}}`, `{{reviewer}}`, `{{body}}`, `{{url}}` |
| `shell` | `command`, `args` (optional array) | `{{output}}` (full stdout) |

**Action template fields** mirror `QueueEntry`: `upstream`, `fork`, `mode`,
`prompt_template` (rendered with event variables → becomes `prompt`), plus
optional `series`, `backend`, `model`, `agent`, `system_prompt`.

### Listener State (`~/.agent/listeners/state/<name>.json`)

```json
{
  "last_checked": "2026-03-18T10:00:00Z",
  "processed_ids": ["42", "57", "103"]
}
```

`processed_ids` are string-serialised source-specific identifiers (GitHub issue
numbers, GitHub review IDs). For `shell` sources there are no IDs; the state
only records `last_checked`, and the script is re-run each interval — it is the
script's responsibility not to emit duplicate output.

---

## New Module: `Orchestra/Listener.lean`

### Types

```lean
inductive SourceConfig where
  | githubIssues  (repo : String) (labels : List String)
  | githubPrReviews (repo : String)
  | shell (command : String) (args : List String)

instance : ToJson SourceConfig
instance : FromJson SourceConfig

structure ActionConfig where
  upstream       : String
  fork           : String
  mode           : TaskMode
  promptTemplate : String
  series         : Option String := none
  backend        : Option String := none
  model          : Option String := none
  agent          : Option String := none
  systemPrompt   : Option String := none

instance : ToJson ActionConfig
instance : FromJson ActionConfig

structure ListenerConfig where
  name            : String
  source          : SourceConfig
  action          : ActionConfig
  intervalSeconds : Nat := 60

instance : ToJson ListenerConfig
instance : FromJson ListenerConfig

structure ListenerState where
  lastChecked  : String          -- ISO 8601 UTC; empty string = never
  processedIds : Array String    -- already-queued event IDs

instance : ToJson ListenerState
instance : FromJson ListenerState
```

### Functions

| Function | Signature | Purpose |
|----------|-----------|---------|
| `listenersDir` | `IO System.FilePath` | `~/.agent/listeners/` |
| `listenerStateDir` | `IO System.FilePath` | `~/.agent/listeners/state/` |
| `loadListenerConfig` | `String → IO (Option ListenerConfig)` | Load `<name>.json` |
| `loadAllListenerConfigs` | `IO (Array ListenerConfig)` | All configs in `listenersDir` |
| `loadListenerState` | `String → IO ListenerState` | Load state; default empty |
| `saveListenerState` | `String → ListenerState → IO Unit` | Persist state |
| `renderTemplate` | `String → List (String × String) → String` | Replace `{{key}}` with values |
| `pollSource` | `SourceConfig → ListenerState → String → IO (Array (String × List (String × String)))` | Poll a source; return `(eventId, vars)` pairs for new events only. `eventId` is `""` for shell sources. The `ghToken` argument is used for GitHub sources. |
| `buildQueueEntry` | `ActionConfig → List (String × String) → IO QueueEntry` | Render template and construct a `QueueEntry` |

### `pollSource` details

- **`githubIssues`**: call the GitHub REST API `GET /repos/{owner}/{repo}/issues?state=open&labels=...` using `IO.Process` with `gh api` (reuse the existing pattern in `Orchestra/GitHub.lean`). Filter out IDs already in `processedIds`. Return one `(issue_number_string, vars)` per new issue.
- **`githubPrReviews`**: call `GET /repos/{owner}/{repo}/pulls?state=open`, then for each PR call `GET /repos/{owner}/{repo}/pulls/{n}/reviews`. Filter by IDs in `processedIds`. Return one entry per new review.
- **`shell`**: spawn the command, capture stdout. If non-empty, return a single event with `eventId = ""` and `vars = [("output", stdout)]`. Shell sources ignore `processedIds` and rely on the script itself to be idempotent; `last_checked` is updated regardless.

---

## Modified: `Orchestra.lean`

Add:
```lean
import Orchestra.Listener
```

---

## Modified: `Main.lean` — `queueStartHandler`

Add `--listener-dir` flag to `queueStartCmd`:
```
l, listener-dir : String; "Directory of listener configs (default: ~/.agent/listeners/)"
```

In `queueStartHandler`:

1. Resolve listener directory (`--listener-dir` or `~/.agent/listeners/`).
2. Load all listener configs with `Listener.loadAllListenerConfigs`.
3. Maintain a `HashMap String Nat` mapping listener name → next-poll time (nanosecond
   monotonic clock). Initialise each to `0` so all listeners fire immediately on
   first iteration.
4. Extend the main `repeat` loop:

```
repeat do
  -- existing: run next pending queue entry (unchanged)
  ...

  -- new: check each listener
  let now ← IO.monoNanosNow
  for cfg in listenerConfigs do
    if now >= nextPollTime.getD cfg.name 0 then
      let state ← Listener.loadListenerState cfg.name
      let ghToken ← ...  -- read from env or appConfig
      let events ← Listener.pollSource cfg.source state ghToken
      for (eid, vars) in events do
        let entry ← Listener.buildQueueEntry cfg.action vars
        Queue.saveEntry entry
        IO.println s!"  Listener '{cfg.name}': queued entry {entry.id}"
      -- update state: add all new event IDs, update lastChecked
      let newIds := events.map (·.1) |>.filter (· != "")
      let newState : Listener.ListenerState := {
        lastChecked  := ← TaskStore.currentIso8601
        processedIds := state.processedIds ++ newIds.toArray
      }
      Listener.saveListenerState cfg.name newState
      -- schedule next poll
      nextPollTime := nextPollTime.insert cfg.name
        (now + cfg.intervalSeconds.toUInt64 * 1_000_000_000)

  IO.sleep 2000
```

The `HashMap` is a local mutable `IO.Ref` holding `Std.HashMap String UInt64`.

---

## New Subcommand: `orchestra queue listeners`

A read-only status command for listing configured listeners and their state.

```
orchestra queue listeners [--listener-dir <path>]
```

Output example:
```
LISTENER            INTERVAL  LAST CHECKED              PROCESSED
mathlib-issues      120s      2026-03-18T10:00:00Z      42 events
pr-reviews          60s       2026-03-18T09:58:00Z      7 events
custom-source       300s      never                     0 events
```

Implementation: `queueListenersHandler` loads all configs + states, prints table.
Add `queueListenersCmd` and register it as a subcommand of `queueCmd`.

---

## GitHub API Access

Listener polling for `github-issues` and `github-pr-reviews` uses `gh api`
(already available in the daemon's environment since `GH_TOKEN` is set from
`appConfig`). The pattern is the same as `Orchestra/GitHub.lean`:

```lean
let child ← IO.Process.spawn {
  cmd  := "gh"
  args := #["api", s!"/repos/{owner}/{repo}/issues?state=open&per_page=100"]
  stdout := .piped
  stderr := .null
  stdin  := .null
}
let out ← child.stdout.readToEnd
let _   ← child.wait
```

Parse the JSON array returned by `gh api`. For `github-pr-reviews`, two API
calls per listener invocation (list PRs, then list reviews for each open PR).

The `ghToken` available to the daemon is read from `appConfig.ghToken` — already
present in the `AppConfig` structure.

---

## Implementation Order

1. **`Orchestra/Listener.lean`** — all types with `ToJson`/`FromJson` instances;
   `listenersDir`, `listenerStateDir`, `loadListenerConfig`, `loadAllListenerConfigs`,
   `loadListenerState`, `saveListenerState`, `renderTemplate`, `buildQueueEntry`
2. **`Orchestra/Listener.lean`** — `pollSource` for all three source types (GitHub
   issues, GitHub PR reviews, shell)
3. **`Orchestra.lean`** — add `import Orchestra.Listener`
4. **`Main.lean`** — add `queueListenersHandler`, `queueListenersCmd`; register in `queueCmd`
5. **`Main.lean`** — extend `queueStartCmd` with `--listener-dir` flag; extend
   `queueStartHandler` with the listener polling loop
6. Build and verify

---

## Design Notes and Trade-offs

- **No new threads**: listeners run synchronously in the existing daemon loop.
  The 2 s sleep between queue checks means the effective listener resolution is
  ≥ 2 s, which is acceptable for GitHub polling intervals (typically ≥ 60 s).
- **Shell idempotency**: shell sources rely on the script being idempotent (or
  having its own deduplication). The daemon only tracks `last_checked` for shell
  sources, not individual event IDs.
- **`gh api` for GitHub**: reuses the authenticated `gh` CLI already present in
  the daemon environment, avoiding a separate HTTP client dependency.
- **Processed-ID growth**: `processedIds` arrays grow without bound. For typical
  usage (hundreds of issues) this is fine. A future improvement could cap the
  array by evicting the oldest entries.
- **Labels filter (github-issues)**: passed directly as a comma-separated query
  parameter to the GitHub API. If `labels` is empty, no filter is applied.
