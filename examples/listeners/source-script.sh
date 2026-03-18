#!/usr/bin/env bash
# Example shell source script for orchestra listeners.
#
# This script is invoked by the queue daemon at each poll interval.
# If it produces output on stdout, that output becomes the {{output}}
# variable in the listener's prompt_template, and one task is enqueued.
# If it produces no output, no task is enqueued.
#
# The script is responsible for its own deduplication: orchestra does not
# track processed IDs for shell sources (unlike GitHub sources). Use a
# state file to remember what has already been processed.
#
# This example reads a simple work-queue file and emits the next pending
# item, marking it as processed so it is not re-emitted on the next poll.

STATE_FILE="${HOME}/.agent/listeners/shell-source-state.txt"
QUEUE_FILE="/path/to/your/work-queue.txt"

if [ ! -f "$QUEUE_FILE" ]; then
  exit 0
fi

# Read already-processed lines
touch "$STATE_FILE"

# Find the first line in the queue file that has not been processed yet
while IFS= read -r line; do
  if [ -z "$line" ]; then
    continue
  fi
  if grep -qxF "$line" "$STATE_FILE"; then
    continue
  fi
  # Emit this line as the task prompt and record it as processed
  echo "$line"
  echo "$line" >> "$STATE_FILE"
  break
done < "$QUEUE_FILE"
