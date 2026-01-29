# Release Decision 4: ABQ v0.4-beta

**Date:** 2026-01-28
**Status:** APPROVED FOR CONTINUED PUBLIC RELEASE
**Round:** 4 (Final Consolidation)

## Review Summary

All 5 reviewers have approved the Round 4 release. No blocking issues identified.

| Reviewer | Decision | Notes |
|----------|----------|-------|
| L7 Engineering | APPROVED | Upgraded from A- to A. Protocol formalization and regression tests strengthen architecture. |
| Security | APPROVED | JSON schemas strengthen posture. Runtime validation recommended for future; not blocking. |
| CTO | APPROVED | v0.4-beta ready for continued release. Clear path to v1.0 established. |
| Legal | APPROVED | No regressions in license compliance. MIT maintained. |
| DevOps | APPROVED | CI green, codecov integrated. Path filters recommended in future iterations. |

## Key Changes (Round 3 → Round 4)

1. **JSON Schema Formalization** - Request and response schemas (Draft 2020-12)
2. **Protocol Regression Tests** - 12 new tests covering compatibility
3. **Test Isolation Fix** - Improved from 61/64 to 64/64 passing tests
4. **Codecov Integration** - Branch coverage tracking enabled
5. **ADR 002** - Session workflow retrospective documented

## Quality Gates

| Gate | Status | Details |
|------|--------|---------|
| Linting (ruff) | ✓ PASS | All checks passing |
| Formatting (ruff) | ✓ PASS | Code style compliant |
| Type Checking (mypy) | ✓ PASS | No type violations |
| Test Suite (pytest) | ✓ PASS | 64/64 tests passing |
| Coverage (overall) | ✓ 46% | Core 89%, CLI 36%, Watcher 34% |
| Pre-commit Hooks | ✓ ACTIVE | beads, detangle, ruff, mypy |

## Coverage Detail

```
Name                         Stmts   Miss  Cover
lib/python/abq/__init__.py       3      0   100%
lib/python/abq/cli.py          419    268    36%
lib/python/abq/core.py         140     15    89%
lib/python/abq/watcher.py      172    114    34%
────────────────────────────────────────────────
TOTAL                          734    397    46%
```

## Release Checklist

**P0 (Blocking) - ALL COMPLETE ✓**

- ✓ Core protocol stability
- ✓ JSON schema validation
- ✓ Test isolation and regression suite
- ✓ Pre-commit hooks active
- ✓ CI pipeline green

**P1 (Recommended for v1.0) - OPEN**

- ⧗ Channel name validation (low security risk, defer to v0.5)
- ⧗ Integration tests (expand test coverage, CTO recommended)
- ⧗ PyPI registration (defer to v1.0)

**Outstanding Beads**

- 1 open (P4: CLI UX review update)

## Verdict

**APPROVED FOR CONTINUED PUBLIC RELEASE**

v0.4-beta is approved for continued release in current public repositories. The formalization of protocol schemas, comprehensive regression test coverage, and unified reviewer agreement provide confidence for user adoption. Recommended path: v0.5 with enhanced integration tests, v1.0 with production hardening and PyPI registration.

---

**Next Review Cycle:** Post v0.5 implementation (estimated post-Q1 2026)
