# DevOps/SRE Re-Review: ABQ (Agent Bus Queue)

**Reviewer**: DevOps/SRE Team
**Date**: 2026-01-28
**Status**: APPROVED
**Prior Review**: 2025-01-28 (APPROVED — IMPROVED)

## Changes Reviewed

### Test Suite & Reliability

| Metric | Prior | Current | Improvement |
|--------|-------|---------|------------|
| Unit Tests | 29 passing | 64 passing | +35 tests |
| False Failures (CI) | 3 from cross-pollution | 0 | Fixed isolation |
| Python Versions | 3.10, 3.11, 3.12 | 3.10, 3.11, 3.12 | Consistent matrix |
| CI Pass Rate | 95% (flaky) | 100% | Deterministic |

Critical win: Test isolation fixed eliminates CI flakiness. No more debugging phantom failures.

### Code Coverage Instrumentation

**New: Codecov Integration**
- `codecov.yml` — Requires 46% threshold, flags branch coverage
- `CODECOV_TOKEN` secret added to GitHub Actions
- `codecov-action@v5` in CI matrix — Upload to Codecov SaaS
- Baseline 46% coverage tracked per-commit

This is a solid foundation for coverage-driven development. Recommend watching for coverage regression trends.

### Pre-Commit Hooks (Updated)

Four-layer guard stack now active:

1. **Beads flush** — Syncs pending issue changes to JSONL before commit
2. **Detangle guard** — Prevents orphaned generated code
3. **Ruff lint** — Auto-formats + flags style violations
4. **Mypy typecheck** — Enforces static typing

All bypassable with `--no-verify`. Burden on developers is acceptable.

## Ops Burden Assessment

| Metric | Status |
|--------|--------|
| Runtime dependencies | 0 daemons (unchanged) |
| Ops burden rating | 1/10 |
| Expected 3 AM pages | 0 |
| New external services | Codecov (SaaS, no ops burden) |

Codecov integration is service-based (no ops overhead). Token rotation recommended annually.

## Recommended Actions

### Gap: CI Path Filters ⚠️

**ADR 002 recommends** skipping CI for beads-only and docs/reviews commits. Currently **not implemented**.

Example: A `.beads/` change or `docs/reviews/` edit still triggers full 9-job matrix.

**Action**: Add `paths-ignore` to workflow YAML targeting:
- `.beads/**`
- `docs/reviews/**`
- `.gitattributes`

Estimated savings: ~10% CI minutes/month.

### Future (P2)

- Integration test stage (separate from unit tests)
- Release workflow (tag-triggered PyPI publish)
- Coverage badge in README

## Verdict

**APPROVED — SOLID RELIABILITY IMPROVEMENTS**

Test isolation fix + Codecov baseline are meaningful maturity gains. CI is now deterministic and observable. Only gap is the recommended path filters (minor efficiency issue, not blocking).

---

*Signed: DevOps/SRE Team*
