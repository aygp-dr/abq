# Security Re-Review: ABQ (Agent Bus Queue)

**Reviewer**: Security Team
**Date**: 2026-01-28
**Status**: APPROVED — IMPROVED POSTURE
**Prior Review**: 2025-01-28 (APPROVED WITH NOTES)

## Changes Reviewed

### Hostname Neutralization — PASS

All internal infrastructure identifiers removed from tracked files:

| Before | After | Files |
|--------|-------|-------|
| `hydrabos`, `nexusbos` | `$REMOTE_HOST` | MULTI_HOST.org, TEST_PLAN.org, cli.py |
| `hydra`, `nexus` | `host-a`, `host-b` | MULTI_HOST.org, TEST_PLAN.org, ADR 001 |
| `Tailscale` references | `VPN` | TEST_PLAN.org |
| Specific OS patch versions | `FreeBSD 14.x` / `15.x` | TEST_PLAN.org, MULTI_HOST.org |

**Verification**: `grep -rn 'hydra\|nexus\|tailscale'` across all tracked files returns zero results.

### Configuration Security — PASS

- `REMOTE_HOST` moved from hardcoded Makefile default to `.env` (gitignored)
- `.env.template` committed with empty placeholder
- Makefile uses `-include .env` (silent failure if absent)
- Sync targets error with helpful message if `REMOTE_HOST` unset

### Pre-Commit Guards — PASS

New detangle guard in `.git/hooks/pre-commit`:
- Warns if generated files (`docs/*.dot`, `*.png`, `*.svg`) staged without org source
- Prevents accidental commit of generated artifacts
- Bypassable with `--no-verify` (appropriate for power users)

### .gitignore — PASS

Generated files properly excluded:
- `docs/*.dot`, `docs/*.png`, `docs/*.svg` (tangled/rendered outputs)
- `.env` (local configuration)
- Previously tracked generated files removed from git history

## Prior Issues Status

| Issue | Severity | Status | Notes |
|-------|----------|--------|-------|
| Path traversal in channel names | LOW | Still open | No validation on channel name input |
| No JSON input validation | LOW | Still open | Handlers responsible (documented) |
| Handler command injection | INFO | Still open | User responsibility (documented) |

## New Considerations

### rsync Transport Security

The `abq sync-remote` command uses rsync over SSH. Security relies on:
- SSH key authentication (user-managed)
- VPN tunnel for network transport (user-managed)
- No ABQ-layer encryption (files are plaintext JSON)

**Assessment**: Appropriate for the threat model. ABQ is a local-first tool; network transport is explicitly out of scope and delegated to proven infrastructure (SSH, rsync).

### abq.el Emacs Package

- No network calls
- No eval of untrusted input
- Shell commands use `shell-quote-argument` for path safety
- No credential handling

**Assessment**: No security concerns.

## Verdict

**APPROVED — SECURITY POSTURE IMPROVED**

The hostname neutralization and .env configuration are meaningful improvements. No new attack surface introduced. Prior LOW-severity items remain acceptable for the threat model.

---

*Signed: Security Team*
