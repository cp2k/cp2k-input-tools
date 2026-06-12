# Contributing

<!-- repo-governance-kit:contributing-v1 -->

## Issue Contract

Use the issue templates. Agent-ready work must include:

- Goal: the observable change requested.
- Acceptance criteria: concrete success conditions.
- Required tests: tests or checks that must be added or run.
- Out of scope: changes that must not be made.

Issues without acceptance criteria or a test plan are not ready for agent work.

## Branches

- Agent branches: `agent/issue-<number>-<slug>`
- Human feature branches: `feat/<slug>`, `fix/<slug>`, or `docs/<slug>`
- Local worktrees: `.worktrees/issue-<number>-<slug>`

## Commits

Use Conventional Commits:

```text
<type>[optional scope]: <description>
```

Allowed types: `feat`, `fix`, `docs`, `test`, `refactor`, `perf`, `ci`, `build`, `chore`, `revert`.

## Pull Requests

Every PR should include:

- A linked issue using `Fixes #<issue-number>`, `Closes #<issue-number>`, or `Resolves #<issue-number>`.
- A summary of changed behavior.
- TDD or test evidence.
- The local commands run.
- Risk and rollback notes.

## Local Quality Gate

Run the standard commands before review:

```bash
make format
make lint
make typecheck
make test
make check
```

## Review Standard

Reviewers should check behavior, tests, security, compatibility, and whether the PR stayed inside scope.
