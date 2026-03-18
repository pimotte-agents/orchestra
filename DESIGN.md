# Wrapper for coding agent

This is a CLI tool managing and sandboxing coding agents.

## Assumptions and rules for a coding agent

- The coding agent works in a container.
- The coding agent has full write access to the fork (via a github app installed on the organization account
  owning the fork).
- The goal is to make new contributions to an upstream repository.
- For the upstream repository, the bot has no rights, but it can create new pull requests through this tool.
  The github app does not provide permissions to create a PR to the upstream repository, but this
  tool is equipped with a personal access token. The coding agent must therefore only have very limited
  acccess to this tool.

## Task

A task consists of a

- pair of upstream repository and fork repository
- tool permissions for the coding agent
- mode: either work only on the fork repository or prepare pull requests from the fork repository
  to the upstream repository (the mode determines if the agent is allowed to make PRs or not in this run)
- prompt

## Standard workflow

The user invokes this tool with a list of tasks which are given in the form of a structured input file
(e.g. toml or yaml, it is fine to support only one). This tool then invokes the coding agent on each of these tasks
with the correct prompt and the correct permissions (depending on the mode, it will allow creation of new
pullrequests on the upstream repository).

## Important restrictions

- The coding agent may not have access to the personal access token which allows for creation of pull requests.
  It only has autonomous access via the github app. All pull requests to the upstream repository
  need to be done via this tool.
- The github app provides a way to create access tokens and to setup authentication via `gh`. This
  token is only valid for some amount of time. Before launching a new task, the tool should reactivate
  the token and prepare gh authentication and also refresh the app token on request by the coding agent.
- The coding agent must not have read access to the private key used to sign the token requests or
  to the personal acccess token.

## Design suggestions

- The tool should manage its own working directory, cloning repositories if necessary. After performing
  a task, the repository should not be removed, but the tool should provide a way to perform the cleanup
  if requested by the user.

- The coding agent should be launched inside a landrun environment with file access restricted to the
  correct subdirectory containing the repository the agent should work on.

## Improvement / feature ideas

- Support a `.agent` configuration folder per repository. It should support shell scripts such as

  * `init.sh`: Run after cloning the repository (and should not be repeated afterwards)
  * `before.sh`: Run this each time before launching an agent (e.g. start a development container)
  * `validation.sh`: Run after each agent launch.
  * `after.sh`: Run this just before finishing the task (after possible validation re-runs). Possible
    actions could be to stop the development container.

  It should also support a config file `config.json` that allows configuring the validation workflow:
  When validation fails, with which prompt should the agent be re-launched. When re-launching an
  agent after validation failure, resume the previous session to preserve the context.

- Continue a previous session using lists of tasks that might run intertwined. For this,

  * Maintain a list of launched tasks (including repo details and prompts) with their corresponding resume keys in
    the global `.agent` repository. Every task should be assigned a unique id.
  * Allow running a new task continuing a previous task by giving the unique id. If a new task
    is run continuing an earlier task, the dependency relation should be stored in the task storage.
  * Add a subcommand to query old tasks.
  * Allow naming a series of tasks and add a command to resume a series with a prompt given on the command line.

  Later, this feature should be extended to support a queue mode. For this there should be a subcommand that launches
  a process that listens to new tasks on a queue. A further subcommand can add new tasks to the queue using the
  established format, possibly referencing a task series name (as described above) or a task id.

- Listeners: There should be a way to setup listeners. These listeners can be activated when starting the
  queue daemon. A listener should regularly check for new tasks from configurable sources. A listener
  consists of two components:
  * a source whose output is some text (see below, e.g. output could be the review comments or simply an issue number)
  * a configurable action taking in the input from the source. The action should be provided as a .json file
    that defines what task is sent to the queue depending on the source

  Possible sources:
  * Github issues
  * reviews of pull requests on Github

  It should be extensible by providing shell scripts for sources, but the two sources above should come pre-configured
  (and implemented in Lean as part of orchestra).
