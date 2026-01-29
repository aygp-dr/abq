# Security Review: ABQ (Agent Bus Queue)

**Reviewer**: Security Team
**Date**: 2025-01-28
**Status**: APPROVED WITH NOTES

## Executive Summary

ABQ is a filesystem-based message queue for local AI agent coordination. The security model inherits Unix file permissions, which is appropriate for the single-machine use case. No critical vulnerabilities identified. Minor recommendations below.

## Threat Model

### Trust Boundaries

1. **User boundary**: All agents run as the same Unix user
2. **Machine boundary**: Designed for single-machine use only
3. **Process boundary**: Agents are separate processes, coordination via filesystem

### Assets

- Message content (may contain sensitive data)
- Channel directories (who can read/write)
- Handler scripts (execute with user privileges)

## Findings

### LOW: Path Traversal in Channel Names

**Location**: `core.py:86-88`

```python
def _get_channel_path(channel: str) -> Path:
    return ABQ_HOME / "channels" / channel
```

**Issue**: Channel names are not sanitized. A malicious channel name like `../../../etc` could theoretically traverse directories.

**Mitigating factors**:
- User must explicitly create channels
- ABQ_HOME is in user's home directory
- No network exposure

**Recommendation**: Add channel name validation (alphanumeric + hyphens only)

```python
import re
if not re.match(r'^[a-zA-Z0-9_-]+$', channel):
    raise ValueError(f"Invalid channel name: {channel}")
```

**Severity**: LOW (local user only)

### LOW: No Input Validation on JSON Content

**Location**: `core.py:159-201`

**Issue**: Message content is stored as-is. Malformed or malicious JSON payloads are passed to handlers.

**Mitigating factors**:
- Handlers are responsible for their own input validation
- This is documented behavior
- Similar to how shell pipes work

**Recommendation**: Document that handlers must validate input. Consider optional JSON schema validation.

**Severity**: LOW (expected behavior)

### INFO: Handler Command Injection

**Location**: `cli.py:167-195` (watch handler execution)

**Issue**: Handler scripts receive message content via environment variables (`ABQ_MSG_CONTENT`). If handlers use this unsafely (e.g., `eval "$ABQ_MSG_CONTENT"`), command injection is possible.

**Mitigating factors**:
- This is how shell works
- Users write their own handlers
- Documented in examples

**Recommendation**: Add security note to documentation about safe handler practices.

**Severity**: INFO (user responsibility)

### NONE: File Permission Model

**Assessment**: GOOD

The filesystem permission model is appropriate:
- `~/.abq/` inherits user's home directory permissions
- No world-readable/writable files created
- Multi-user scenarios would require explicit chmod

### NONE: Secrets Exposure

**Assessment**: GOOD

- No hardcoded secrets
- No API keys or credentials
- Git context extraction only reads public repo info
- Environment variable `ABQ_HOME` is optional

### NONE: Dependencies

**Assessment**: GOOD

- Minimal dependencies (stdlib only for core)
- No network calls
- No eval/exec of user input in core library

## Recommendations

1. **Add channel name validation** (LOW priority)
2. **Document handler security best practices** (LOW priority)
3. **Consider adding optional message signing** for multi-user scenarios (FUTURE)

## Verdict

**APPROVED FOR PUBLIC RELEASE**

The security model is appropriate for the stated use case (local, single-user agent coordination). The design follows the principle of "filesystem as database" which has well-understood security properties.

No CVE-worthy issues identified.

---

*Signed: Security Team*
