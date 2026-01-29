# L7 (Senior Engineer) Re-Review: ABQ (Agent Bus Queue)

**Reviewer**: L7 Engineering
**Date**: 2026-01-28
**Status**: APPROVED — NO REGRESSIONS
**Prior Review**: 2025-01-28 (APPROVED WITH MINOR RECOMMENDATIONS)

## Changes Since Last Review

| Change | Impact | Regression? |
|--------|--------|-------------|
| Graphviz state machine in org-mode | Documentation improvement | No |
| abq.el Emacs support package | New tooling, no core changes | No |
| Bidirectional tangle/detangle workflow | Build system improvement | No |
| Security: hostname neutralization in docs | Security hardening | No — **improvement** |
| .env-based REMOTE_HOST config | Removed hardcoded infra | No — **improvement** |
| CI lint/typecheck fixes (ruff + mypy) | Code quality | No — **improvement** |
| Multi-host rsync sync | New feature (sync-remote) | No |
| Pre-commit detangle guard | Build safety | No |

## P0 Status Check (from prior review)

| Item | Status | Notes |
|------|--------|-------|
| LICENSE file | ✅ Done | MIT license present |
| Fix recv() race condition | ⚠️ Still open | core.py:224-229 — read+rename without try/except |
| CONTRIBUTING.md | ❌ Missing | Not yet created |

## Updated Assessment

### Overall Grade: B+ → A-

| Aspect | Prior | Now | Delta | Notes |
|--------|-------|-----|-------|-------|
| Code clarity | A | A | — | Maintained |
| API design | A- | A- | — | No API changes |
| Test coverage | B | B+ | ↑ | 29 tests (was 18), 34% line coverage |
| Documentation | A | A+ | ↑ | State machine diagram, tangle workflow, security-neutral docs |
| Error handling | B | B | — | recv() race still open |
| Type hints | B | B+ | ↑ | mypy passes clean (5 errors fixed) |
| Build/CI | B | A | ↑ | Lint, typecheck, coverage in CI; Makefile targets for tangle/detangle/dot |
| Security posture | B+ | A | ↑ | No hardcoded infra, .env config, pre-commit guards |

### New Strengths

1. **Literate programming done right**: org-mode as single source of truth, with `:comments link` for bidirectional tangle/detangle. The Makefile documents the workflow clearly.
2. **Security-first docs**: All internal hostnames replaced with generic placeholders. Configuration via gitignored `.env`. Pre-commit hook guards generated files.
3. **CI is green path**: ruff + mypy + pytest all pass. Coverage reporting wired to Codecov.
4. **Multi-host story is coherent**: rsync-based sync with inotify pickup on FreeBSD 15, polling fallback on 14. Documented in neutral terms.

### Remaining Issues

1. **recv() race condition** — Still P0. Two concurrent receivers can both read the same file before either renames it.
2. **CONTRIBUTING.md** — Still missing. Minor for internal use, needed for public release.
3. **Coverage at 34%** — cli.py has 0% coverage (not tested). Core at 90% is good. Watcher at 39% is understandable (platform-specific paths).
4. **pytest-cov not resolving locally via uv sync** — Dev dependency present in pyproject.toml but needed manual `uv pip install`. Investigate lock file.

### Use Case Validation

The same-machine use case (worktrees, separate processes within one repo) remains the core value proposition. The rsync multi-host extension is a natural addition — filesystem-based design means any file transport works. The inotify integration on FreeBSD 15 means rsync-delivered messages trigger the watcher immediately.

## Verdict

**APPROVED — NO REGRESSIONS DETECTED**

The project has improved since last review. Build system is stronger, docs are security-neutral, CI is cleaner. The recv() race condition remains the only P0 item. Everything else is additive improvement.

---

*Signed: L7 Engineering*
