# DevOps/SRE Re-Review: ABQ (Agent Bus Queue)

**Reviewer**: DevOps/SRE Team
**Date**: 2026-01-28
**Status**: APPROVED — IMPROVED
**Prior Review**: 2025-01-28 (APPROVED)

## Changes Reviewed

### Build System

| Addition | Ops Impact |
|----------|-----------|
| `gmake tangle` | Tangles org source to scripts/config |
| `gmake detangle` | Syncs edited tangled files back to org |
| `gmake dot-render` | Renders Graphviz diagrams (requires `dot`) |
| `gmake sync-down/up/both` | rsync channels to/from remote host |
| `gmake .env` | Creates `.env` from template |

Makefile is well-organized with section headers and help text. Pattern rules for dot rendering are clean.

### CI Pipeline

| Job | Prior | Current |
|-----|-------|---------|
| Lint (ruff) | ❌ Failing | ✅ Passing |
| Typecheck (mypy) | ❌ Failing | ✅ Passing |
| Tests (pytest) | ✅ 18 tests | ✅ 29 tests (3.10, 3.11, 3.12) |
| Coverage | Configured | Configured + Codecov upload |

### Configuration Management

- `REMOTE_HOST` moved from hardcoded default to `.env` — standard 12-factor pattern
- `.env.template` committed, `.env` gitignored — no secrets in repo
- `-include .env` in Makefile — silent fallback if missing

### Pre-Commit Hooks

Two guards now in `.git/hooks/pre-commit`:
1. **Beads sync**: Flushes pending issue changes to JSONL
2. **Detangle guard**: Warns if generated files staged without org source change

Both are reasonable and bypassable with `--no-verify`.

## Ops Burden Assessment

| Metric | Prior | Current | Delta |
|--------|-------|---------|-------|
| Ops burden rating | 1/10 | 1/10 | — |
| Dependencies (runtime) | 0 daemons | 0 daemons | — |
| New external requirements | — | `dot` (graphviz) for rendering | Optional |
| Failure modes | All obvious | All obvious | — |
| 3 AM pages expected | 0 | 0 | — |

### New Dependency: Graphviz

`dot` is only needed for `gmake dot-render`. Not a runtime dependency. Available via `pkg install graphviz` on FreeBSD. Acceptable.

## Prior Recommendations Status

| Recommendation | Status |
|----------------|--------|
| `abq cleanup --older-than 7d` | Not yet implemented |
| Metrics endpoint | Not yet implemented |
| Health checks | Not yet implemented |

These remain P2 — nice to have, not blocking.

## Verdict

**APPROVED — NO REGRESSIONS, IMPROVED CI**

The CI pipeline going from red to green is a meaningful improvement. Build system additions are well-structured. Ops burden remains minimal.

---

*Signed: DevOps/SRE Team*
