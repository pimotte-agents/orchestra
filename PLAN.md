# Implementation Plan: Task Queue Mode

## Overview

Extend the agent CLI with a **queue mode**: a long-running daemon process that
executes tasks one at a time as they are submitted, plus a subcommand to add
tasks to the queue.

This builds on the existing `TaskStore`, `Task`, `AppConfig`, and series
mechanisms without changing them.

---

## New Subcommands

```
agent queue-start [--config <path>] [--debug]
    Start the queue daemon. Polls ~/.agent/queue/ for pending entries and
    runs them serially. Writes a PID file and exits if a daemon is already running.

agent enqueue <task-file> [--task N] [--series <name>] [--continues <id>] [--config <path>]
    Add one or more tasks from a task file to the queue.
    Prints the queue entry ID(s) on stdout.

agent queue [--limit N]
    List recent queue entries (pending, running, done, failed), most recent first.
```

---

## Data Model

### Queue Entry

One file per queued task at `~/.agent/queue/<entry-id>.json`:

```json
{
  "id":            "0123456789abcdef",
  "created_at":    "2026-01-15T14:30:00Z",
  "status":        "pending",
  "upstream":      "leanprover-community/mathlib4",
  "fork":          "mybot/mathlib4",
  "mode":          "pr",
  "prompt":        "Fix the sorry in Algebra.Group.Defs",
  "agent":         null,
  "system_prompt": null,
  "backend":       null,
  "model":         null,
  "continues_from": null,
  "series":        "mathlib-fixes",
  "task_id":       null,
  "config_path":   null
}
```

| Field | Type | Notes |
|-------|------|-------|
| `id` | String | 16-char hex, same format as `TaskStore.generateId` |
| `created_at` | String | ISO 8601 UTC |
| `status` | String | `"pending"` \| `"running"` \| `"done"` \| `"failed"` |
| `upstream`, `fork`, `mode`, `prompt` | String | Task fields (required) |
| `agent`, `system_prompt`, `backend`, `model` | String? | Optional task fields |
| `continues_from` | String? | TaskStore task ID to continue |
| `series` | String? | Series name to assign on completion |
| `task_id` | String? | TaskStore ID written when the daemon picks up the entry |
| `config_path` | String? | Path to app config (overrides default) |

Status transitions: `pending → running → done | failed`

### PID File

`~/.agent/queue/daemon.pid` — contains the daemon's OS PID as a decimal string.
Written at startup; deleted on clean exit. Used to prevent duplicate daemons and
to report daemon status in `agent queue`.

---

## File System Layout (additions)

```
~/.agent/
  queue/
    daemon.pid              ← written by queue-start, deleted on exit
    0123456789abcdef.json   ← one file per enqueued task
    ...
```

---

## New Module: `Agent/Queue.lean`

### Types

```lean
inductive QueueStatus where
  | pending | running | done | failed
deriving Repr

instance : FromJson QueueStatus   -- "pending" | "running" | "done" | "failed"
instance : ToJson QueueStatus

structure QueueEntry where
  id           : String
  createdAt    : String
  status       : QueueStatus    := .pending
  upstream     : String
  fork         : String
  mode         : TaskMode
  prompt       : String
  agent        : Option String  := none
  systemPrompt : Option String  := none
  backend      : Option String  := none
  model        : Option String  := none
  continuesFrom : Option String := none
  series       : Option String  := none
  taskId       : Option String  := none
  configPath   : Option String  := none

instance : FromJson QueueEntry
instance : ToJson QueueEntry
```

### Functions

| Function | Signature | Purpose |
|----------|-----------|---------|
| `queueDir` | `IO System.FilePath` | `~/.agent/queue/` |
| `pidFile` | `IO System.FilePath` | `~/.agent/queue/daemon.pid` |
| `saveEntry` | `QueueEntry → IO Unit` | Write `<id>.json` atomically |
| `loadEntry` | `String → IO (Option QueueEntry)` | Load by ID |
| `loadAllEntries` | `IO (Array QueueEntry)` | All entries, sorted by ID descending |
| `nextPending` | `IO (Option QueueEntry)` | Oldest `pending` entry, or `none` |
| `writePid` | `UInt32 → IO Unit` | Write daemon PID |
| `readPid` | `IO (Option UInt32)` | Read daemon PID; `none` if missing |
| `deletePid` | `IO Unit` | Remove PID file |
| `daemonRunning` | `IO Bool` | Check if process with stored PID is alive (`/proc/<pid>`) |

`saveEntry` writes atomically (temp file + rename), same pattern as `TaskStore`.

---

## Modified: `Main.lean`

### New handler: `queueStartHandler`

```
agent queue-start [--config <path>] [--debug]
```

1. Check `Queue.daemonRunning`. If true, print error and return 1.
2. Write own PID via `Queue.writePid`.
3. Load `AppConfig` from `--config` (or default).
4. Enter polling loop:
   - `Queue.nextPending` → if `none`, sleep 2 seconds and repeat.
   - Mark entry `running` (`Queue.saveEntry { e with status := .running }`).
   - Construct `Task` from entry fields.
   - Call `runTask appConfig task 0 debug (continuesFrom := e.continuesFrom) (series := e.series)`.
     - `runTask` already handles TaskStore recording, series pointer update, GitHub auth, cloning, MCP server, and validation loop.
   - On success: `Queue.saveEntry { e with status := .done, taskId := some taskId }`.
     - To get `taskId`, `runTask` must return the ID it created. See **`runTask` return value** below.
   - On exception: `Queue.saveEntry { e with status := .failed }`, log error, continue loop.
5. On SIGTERM / clean exit: `Queue.deletePid`.

### `runTask` return value change

`runTask` currently returns `IO Unit`. Change it to `IO String` (returns the
task ID it created). This lets `queueStartHandler` link the queue entry to the
TaskStore record.

The change is purely additive — existing callers that ignore the return value
need only add `let _ ←` or `discard`.

### New handler: `enqueueHandler`

```
agent enqueue <task-file> [--task N] [--series <name>] [--continues <id>] [--config <path>]
```

1. Load task file.
2. Select tasks (all, or just index `--task N`).
3. Validate `--continues` requires `--task` with multi-task files (same rule as `run`).
4. For each selected task, build a `QueueEntry`:
   - `id ← TaskStore.generateId` (reuse existing ID generator)
   - `createdAt ← TaskStore.currentIso8601`
   - Copy `upstream`, `fork`, `mode`, `prompt`, `agent`, `systemPrompt`, `backend`, `model` from task
   - `continuesFrom` and `series` from flags
   - `configPath` from `--config` flag
   - `status := .pending`
5. `Queue.saveEntry entry` for each.
6. Print each entry ID on stdout.

### New handler: `queueListHandler`

```
agent queue [--limit N]
```

1. `Queue.loadAllEntries` (most recent first, capped at `--limit` default 20).
2. If `Queue.daemonRunning`, print `Daemon running (PID <n>)`. Otherwise `Daemon not running.`
3. Print table:

```
ID               CREATED              FORK                        STATUS   SERIES
0123456789abcdef 2026-01-15T14:30:00Z mybot/mathlib4              pending  mathlib-fixes
fedcba9876543210 2026-01-15T14:28:00Z mybot/std4                  done     -
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
  tagCmd;
  resumeCmd;
  queueStartCmd;
  enqueueCmd;
  queueListCmd
```

---

## Integration with Existing Mechanisms

- **TaskStore**: `queueStartHandler` calls `runTask` which already handles all
  TaskStore operations. No changes needed to `TaskStore.lean`.
- **Series**: `runTask` already calls `TaskStore.updateSeriesPointer` when
  `series` is set. `enqueueHandler` stores the series name in the queue entry;
  `queueStartHandler` passes it to `runTask`.
- **`--continues`**: `runTask` already resolves `continuesFrom` to a session ID.
  `queueStartHandler` passes `continuesFrom` from the queue entry.
- **`AppConfig`**: `queueStartHandler` loads config once at startup (not per
  task), since the daemon is a single-user process. `enqueueHandler` stores
  `configPath` in the entry for the daemon to use if different from its own
  default.

---

## Implementation Order

1. **`Agent/Queue.lean`** — `QueueStatus`, `QueueEntry`, JSON instances,
   `queueDir`, `pidFile`, `saveEntry`, `loadEntry`, `loadAllEntries`,
   `nextPending`, `writePid`, `readPid`, `deletePid`, `daemonRunning`
2. **`Agent.lean`** — add `import Agent.Queue`
3. **`Main.lean`** — change `runTask` to return `IO String`; update existing callers
4. **`Main.lean`** — add `enqueueHandler` and `enqueueCmd`
5. **`Main.lean`** — add `queueStartHandler` and `queueStartCmd`
6. **`Main.lean`** — add `queueListHandler` and `queueListCmd`
7. **`Main.lean`** — register new commands in `agentCmd`
8. Build and verify
