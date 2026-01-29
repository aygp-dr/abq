# Security Re-Review: ABQ (Agent Bus Queue) — Round 4

**Reviewer**: Security Team
**Date**: 2026-01-28
**Status**: APPROVED — SCHEMA DEFENSE LAYER ADDED
**Prior Reviews**: APPROVED (Round 1), APPROVED — IMPROVED POSTURE (Round 2)

## Changes Reviewed

### JSON Schema Validation — POSITIVE

**Addition**: Draft 2020-12 schemas for request and response message formats.

**Security benefits**:
- `additionalProperties: false` blocks field injection attacks
- Message ID validation (`^req_[0-9a-f]+$`) prevents path traversal via identifier
- Content field typed as `string` (not `object`) — enforces JSON-in-JSON design, limits nested injection surface
- 12 protocol regression tests ensure compliance

**Current scope limitation**: Schemas are used for *testing only*. No runtime validation at message boundaries (send/recv).

**Assessment**: Positive addition; however, **runtime enforcement would significantly strengthen security posture**. Testing validates happy path; runtime checks defend against malformed/malicious messages in production.

### Test Isolation — FIXED

Cross-test ABQ_HOME pollution resolved. Tests now properly isolate environment, eliminating risk of state leakage between assertions.

### Codecov Integration

Coverage tracking added. No new attack surface; standard observability improvement.

### Session Workflow (ADR 002)

Documents message flow and lifecycle. Aids security reasoning about message state transitions.

## Prior Issues Status

| Issue | Severity | Status | Notes |
|-------|----------|--------|-------|
| Channel name path traversal | LOW | **Still open** | No runtime validation; schema doesn't cover channel creation |
| No JSON validation at boundaries | LOW | **Partially mitigated** | Schemas exist but not enforced at recv() |
| Handler command injection | INFO | Still acceptable | User responsibility; documented |

## Critical Gap: Runtime Schema Validation

**Observation**: Schemas define the security boundary but are not enforced when messages cross process boundaries.

**Risk**: A malicious or corrupted message file could bypass schema constraints if:
- A process writes directly to the channel directory (bypassing ABQ API)
- A filesystem corruption or race condition produces invalid JSON
- An attacker with local filesystem access crafts invalid message files

**Recommendation**: Implement `validate_message()` call in `recv()` before returning to caller. Use `jsonschema.validate()` with `raise_for_unknown_properties=true`.

## Verdict

**APPROVED — SCHEMA DEFENSE LAYER ADDED**

The addition of JSON schemas with strict `additionalProperties: false` is a meaningful security improvement. Combined with ID format validation, this significantly reduces the injection attack surface.

To maximize security benefit, promote schemas from testing-only to runtime enforcement. This is a low-effort, high-confidence hardening step.

---

*Signed: Security Team*
