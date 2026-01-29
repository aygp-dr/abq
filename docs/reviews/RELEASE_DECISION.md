# Release Decision: ABQ (Agent Bus Queue)

**Date**: 2025-01-28
**Decision**: ✅ **APPROVED FOR PUBLIC RELEASE**

## Review Summary

| Review | Reviewer | Status | Key Finding |
|--------|----------|--------|-------------|
| Security | Security Team | ✅ APPROVED | No CVE-worthy issues |
| Legal | Legal Team | ✅ APPROVED | MIT license, no proprietary code |
| CTO | CTO Office | ✅ APPROVED | Fills market gap, good positioning |
| L7 | Senior Engineering | ✅ APPROVED | Solid engineering, research-aligned |
| DevOps | SRE Team | ✅ APPROVED | Ops burden 1/10 |

## Final Name

**ABQ** (Agent Bus Queue)
- Repository: `aygp-dr/abq` → to be transferred or made public
- PyPI package: `abq`
- CLI command: `abq`

## Pre-Release Checklist

### Required (P0)

- [ ] Add LICENSE file (MIT)
- [ ] Add CONTRIBUTING.md
- [ ] Fix race condition in recv() (minor)
- [ ] Make repository public

### Recommended (P1)

- [ ] Add channel name validation
- [ ] Complete type hints
- [ ] Add integration tests
- [ ] Register on PyPI

### Future (P2)

- [ ] `abq cleanup` command for archive management
- [ ] Metrics endpoint
- [ ] Windows testing

## Release Notes Draft

```markdown
# ABQ v1.0.0

**ABQ** (Agent Bus Queue) - A minimal filesystem-based message queue
for AI agent coordination.

## Highlights

- Zero daemon, zero broker - it's files in a folder
- Python library + CLI
- Cross-platform (Linux, macOS, FreeBSD)
- MIT licensed

## Installation

```bash
pip install abq
abq init
```

## Quick Start

```bash
abq channel create my-agent
abq send my-agent signal '{"done": true}'
abq watch my-agent --handler ./handler.sh
```

## Documentation

- [README](README.org)
- [ADR: Transport Selection](docs/adr/001-transport-selection.org)
- [Test Scenarios](docs/SCENARIOS.org)
```

## Reviewers Sign-off

- [x] Security Team - 2025-01-28
- [x] Legal Team - 2025-01-28
- [x] CTO Office - 2025-01-28
- [x] L7 Engineering - 2025-01-28
- [x] DevOps/SRE - 2025-01-28

## Authorization

Approved for public release pending completion of P0 checklist items.

---

*Document generated from beads tracking:*
- agent-bus-lsl (Security)
- agent-bus-txl (Legal)
- agent-bus-6xi (CTO)
- agent-bus-cl1 (L7)
- agent-bus-0ar (DevOps)
