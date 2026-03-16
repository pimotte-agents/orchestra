# Implementation Plan: Task Persistence and Session Continuity

## Overview

Add a persistent task store so every agent run is recorded with a unique ID and its
Claude session ID. This enables:
- Continuing a previous session by ID (`--continues`)
- Grouping related runs into a named series
- Resuming the latest run in a series with a new prompt (`agent resume`)
- Querying task history (`agent tasks`, `agent task <id>`, `agent series`)

The queue-mode extension described in DESIGN.md is out of scope for this plan.

---

## Data Model

### Task Record

Every call to `runTask` creates one record stored at
`~/.agent/tasks/<id>.json`:

```json
{
  "id":             "0123456789abcdef",
  "created_at":     "2024-01-15T14:30:00Z",
  "upstream":       "leanprover-community/mathlib4",
  "fork":           "mybot/mathlib4",
  "mode":           "pr",
  "prompt":         "Fix the sorry in Algebra.Group.Defs",
  "session_id":     "abc123...",
  "status":         "completed",
  "continues_from": "fedcba9876543210",
  "series":         "mathlib-fixes"
}
```

`continues_from` and `series` are omitted when not applicable.
`session_id` is omitted until the agent emits its `init` event and is updated
after the run completes.
`status` is one of `"running"`, `"completed"`, `"failed"`.

### Series Pointer

`~/.agent/series/<name>.json` holds a single pointer to the latest task in
the series:

```json
{ "latest_task_id": "0123456789abcdef" }
```

Updated atomically (write temp file, rename) after each run that belongs to
the series.

### Task ID Format

A 16-character lowercase hex string derived from `IO.monoNanosNow` (a
monotonically increasing `UInt64` nanosecond counter). Lexicographically
sortable, globally unique within a single machine.

---

## File System Layout (additions)

```
~/.agent/
  tasks/
    0123456789abcdef.json   ← one file per task run
    ...
  series/
    mathlib-fixes.json      ← series pointer files
    ...
```

---

## New Module: `Agent/TaskStore.lean`

### Types

```lean
inductive TaskStatus where
  | running | completed | failed

structure TaskRecord where
  id           : String
  createdAt    : String         -- ISO 8601, from `date -u +%Y-%m-%dT%H:%M:%SZ`
  upstream     : String
  fork         : String
  mode         : TaskMode
  prompt       : String
  sessionId    : Option String  := none
  status       : TaskStatus     := .running
  continuesFrom : Option String := none
  series       : Option String  := none
```

### Functions

| Function | Signature | Purpose |
|----------|-----------|---------|
| `tasksDir` | `IO System.FilePath` | `~/.agent/tasks/` |
| `seriesDir` | `IO System.FilePath` | `~/.agent/series/` |
| `generateId` | `IO String` | 16-char hex from `IO.monoNanosNow` |
| `saveTask` | `TaskRecord → IO Unit` | Write `tasks/<id>.json` |
| `loadTask` | `String → IO (Option TaskRecord)` | Load by ID; `none` if missing |
| `loadAllTasks` | `IO (Array TaskRecord)` | Load all records, sorted by ID descending |
| `latestInSeries` | `String → IO (Option String)` | Read series pointer → task ID |
| `updateSeriesPointer` | `String → String → IO Unit` | Write series pointer atomically |

`saveTask` and `updateSeriesPointer` write atomically: write to `<path>.tmp`,
then rename.

`ToJson`/`FromJson` instances are derived for `TaskStatus` and `TaskRecord`.

---

## Modified: `Agent/Sandbox.lean`

No changes needed — `resume : Option String` and the `(UInt32 × Option String)`
return type were already added in the previous feature.

---

## Modified: `Agent/Basic.lean` / `Agent.lean`

Add `import Agent.TaskStore` to `Agent.lean`.

---

## Modified: `Main.lean`

### `runTask` signature extension

```lean
private def runTask
    (appConfig    : AppConfig)
    (task         : Task)
    (idx          : Nat)
    (debug        : Bool)
    (continuesFrom : Option String := none)
    (series        : Option String := none) : IO Unit
```

### `runTask` body changes

**Before the validation loop**, insert:

1. Generate a task ID: `let taskId ← TaskStore.generateId`
2. Resolve initial resume ID: if `continuesFrom` is `some prevId`, load
   `TaskStore.loadTask prevId` and extract `sessionId` → `initialResume`.
   If the referenced task doesn't exist or has no session ID, warn and
   set `initialResume := none`.
3. Create and save the initial task record (status `running`).

**In the validation loop**, change the resume logic so that the first attempt
uses `initialResume` (from `continuesFrom`) rather than `none`:

```lean
let resume := if attempt == 0 then initialResume else sessionId
```

**After the validation loop**, update the record with the final session ID
and status (`completed` or `failed` — treat exhausted retries as
`completed` since the agent did run).

**After `after.sh`**, if `series` is `some name`:
- `TaskStore.updateSeriesPointer name taskId`

### `runHandler` changes

Read two new flags from `Parsed`:
- `--continues <id>` (optional `String`)
- `--series <name>` (optional `String`)

When `--continues` is set and `--task` is not set and the task file has more
than one task, emit an error: `--continues requires --task when the file has
multiple tasks`.

Pass `continuesFrom` and `series` through to each `runTask` call.

### New subcommand handlers

#### `tasksHandler`

```
agent tasks [--limit N]
```

Load all task records via `TaskStore.loadAllTasks`.
Print a table (most recent first):

```
ID               CREATED              FORK                        STATUS     SERIES
0123456789abcdef 2024-01-15T14:30:00Z mybot/mathlib4              completed  mathlib-fixes
```

`--limit N` (default 20) caps the number of rows.

#### `taskShowHandler`

```
agent task <id>
```

Load the record by ID. Print all fields. If not found, error.

#### `seriesHandler`

```
agent series
```

List all series files in `~/.agent/series/`, printing each series name and its
latest task ID and status.

#### `resumeHandler`

```
agent resume <series-name> --prompt "..."
```

1. `TaskStore.latestInSeries seriesName` → `prevId`
2. `TaskStore.loadTask prevId` → `prevRecord`
3. Construct a `Task` inheriting `fork`, `upstream`, `mode` from `prevRecord`,
   with `prompt` from the flag, and `agent`/`systemPrompt` defaults.
4. Load `AppConfig` (with optional `--config` flag).
5. Call `runTask appConfig task 0 debug (continuesFrom := prevId) (series := seriesName)`.

---

## New CLI Commands

```
agent tasks [--limit N]          List recent task runs
agent task <id>                  Show details of one task run
agent series                     List all task series
agent resume <series> -p <text>  Resume latest task in a series
```

### `run` command flag additions

```
--continues <id>   Resume the Claude session of a previous task
--series <name>    Add this run to a named task series
```

---

## Updated `agentCmd` SUBCOMMANDS

```lean
SUBCOMMANDS:
  runCmd';
  mcpServerCmd;
  prepareCmd;
  cleanupCmd;
  tasksCmd;
  taskCmd;
  seriesCmd;
  resumeCmd
```

---

## Implementation Order

1. `Agent/TaskStore.lean` — types, JSON instances, storage functions
2. `Agent.lean` — add import
3. `Main.lean` — extend `runTask` + `runHandler`
4. `Main.lean` — add `tasksCmd`, `taskCmd`, `seriesCmd`, `resumeCmd` handlers and commands
5. Build and verify
