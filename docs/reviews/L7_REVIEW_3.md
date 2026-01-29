# L7 Engineering Review: ABQ Round 4
**Project:** Agent Bus Queue (ABQ) - Filesystem-based message bus for AI agent coordination
**Date:** 2026-01-28
**Overall Grade:** A (improved from A-)

## Executive Summary
ABQ demonstrates solid engineering maturity in Round 4. The team addressed critical test isolation issues, formalized the protocol via JSON schemas (Draft 2020-12), and drove coverage from 26% to 46%. Core module testing is strong (89% coverage). The project is production-ready for single-host deployments with good regression protection, though multi-agent integration testing and runtime schema validation remain gaps.

## Strengths

**Protocol Formalization** — JSON schemas for request/response with 12 new regression tests is excellent defensive engineering. Schema versioning is built in (Draft 2020-12), enabling future breaking changes to be detected early. This directly addresses message format drift risks in distributed agent systems.

**Test Isolation Resolution** — The monkeypatch-based `ABQ_HOME` isolation fix eliminated cross-test pollution that was producing false negatives. This was a known issue; fixing it is a meaningful reliability improvement. Isolation is now solid across all three test modules.

**Coverage Trajectory** — Growth from 26% → 46% is meaningful, particularly the 89% coverage in `core.py` (the business-critical module). `cli.py` (36%) and `watcher.py` (34%) have room to grow, but current coverage distribution is sensible for a young codebase.

**Quality Gates** — All four gates (ruff lint, ruff format, mypy, pytest) passing consistently. Codecov integration and GitHub Actions upload are in place. This is how teams prevent regression.

## Gaps

**Integration Testing** — 64 tests are all unit-level. Zero integration tests covering:
- Multi-agent synchronization workflows
- Concurrent message processing under load
- Filesystem-level race conditions

This is a hard requirement before production use in multi-agent scenarios.

**Runtime Schema Validation** — Schemas exist but aren't enforced at parse/send time. Consider adding a validation layer in `core.py` that fails fast on non-compliant messages.

**Property-Based Testing** — No hypothesis-style tests. File format, message sequencing, and disk I/O are natural targets for property tests.

## Recommendation
**Ship with current constraints.** Single-host, single or low-concurrency agent coordination is safe. Multi-agent deployments should wait for integration tests + runtime validation.

**Next priorities:**
1. Integration test suite (blocking multi-agent)
2. Runtime schema enforcement in message handlers
3. Watcher concurrency tests (file watch atomicity)

Engineering discipline is high. Team has good instincts for test isolation and protocol safety. Continue this trajectory.
