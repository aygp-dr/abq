# DevOps Review: ABQ (Agent Bus Queue)

**Reviewer**: DevOps/SRE Team
**Date**: 2025-01-28
**Status**: APPROVED

## Executive Summary

ABQ is refreshingly simple from an ops perspective. No daemons to babysit, no ports to open, no clusters to manage. It's files in a folder.

**Would I want to support this in production?** Yes, because there's almost nothing to support.

## Installation & Setup

### Complexity: LOW ✓

```bash
# Option 1: pip/uv
uv pip install abq
abq init

# Option 2: From source
git clone https://github.com/aygp-dr/abq
cd abq
make install
```

**Dependencies**: Python 3.10+ (stdlib only for core)

**No external services required.**

### Cross-Platform Compatibility

| Platform | Status | Notes |
|----------|--------|-------|
| Linux | ✓ | Primary target |
| macOS | ✓ | Tested |
| FreeBSD | ✓ | Developed on |
| Windows | ? | Untested (pathlib should work) |

## Resource Usage

### Memory: MINIMAL

- No daemon = no persistent memory footprint
- Each `abq` invocation: ~10-20MB Python process
- Watcher process: ~20MB steady-state

### CPU: MINIMAL

- Polling (default 0.5s) is cheap
- JSON parsing is fast
- No compression, encryption by default

### Disk: MINIMAL

- Each message: ~500 bytes - 1KB JSON
- Archived messages persist (manual cleanup)
- No indexes, no WAL, no compaction

**Recommendation**: Add `abq cleanup --older-than 7d` command for archive management.

## Operational Concerns

### Failure Modes

| Failure | Impact | Recovery |
|---------|--------|----------|
| Disk full | New messages fail | Clear space, retry |
| ABQ_HOME deleted | All state lost | `abq init` (messages gone) |
| Watcher crash | Messages queue up | Restart watcher |
| Handler timeout | Message marked error | Manual retry or resend |
| Corrupt JSON | Single message stuck | Delete/fix file manually |

**All failures are obvious and recoverable.** No split-brain, no quorum issues.

### Monitoring

Currently: None built-in

**Recommendations**:

1. **Metrics endpoint** (future):
   ```bash
   abq metrics  # Prometheus format
   # abq_messages_pending{channel="x"} 5
   # abq_messages_processed_total{channel="x"} 100
   ```

2. **Health check**:
   ```bash
   abq status --json | jq '.channels | to_entries | map(select(.value.processing > 10)) | length'
   ```

3. **Log aggregation**: Handler stdout/stderr goes to watcher's terminal. Consider:
   ```bash
   abq watch channel -H ./handler.sh 2>&1 | logger -t abq-channel
   ```

### Alerting Suggestions

| Condition | Alert |
|-----------|-------|
| `requests/` > 100 files | Consumer may be down |
| `processing/` > 10 files | Handler may be stuck |
| `archive/` > 10000 files | Cleanup needed |

## CI/CD Integration

### GitHub Actions

```yaml
- name: Install abq
  run: uv pip install abq

- name: Notify test completion
  run: |
    abq init
    abq channel create ci-signals
    abq send ci-signals signal '{"event": "tests_passed", "sha": "${{ github.sha }}"}'
```

### Local Development

```bash
# In pre-commit hook
abq send dev-agent signal '{"event": "pre_commit", "files": "'"$(git diff --cached --name-only)"'"}'
```

## Scaling Considerations

### When ABQ is appropriate

- ✓ Single machine
- ✓ < 100 messages/minute
- ✓ < 10 agents
- ✓ Debuggability over performance

### When to use something else

- ✗ Multi-machine → Use NATS
- ✗ > 1000 messages/second → Use Redis/NATS
- ✗ Exactly-once delivery required → Use proper MQ
- ✗ Message ordering guarantees → Use Kafka

## Security Operations

- Inherits filesystem permissions
- No network exposure
- No authentication beyond Unix users
- Secrets in messages: user responsibility

**Recommendation**: Document that sensitive data in messages should be encrypted at application layer.

## Disaster Recovery

### Backup

```bash
tar czf abq-backup-$(date +%Y%m%d).tar.gz ~/.abq/
```

### Restore

```bash
tar xzf abq-backup-*.tar.gz -C ~/
```

**RTO**: Seconds (just extract files)
**RPO**: Depends on backup frequency

## Comparison to Alternatives

| Aspect | ABQ | Redis Pub/Sub | NATS | Kafka |
|--------|-----|---------------|------|-------|
| Install | pip | Redis server | Binary | Cluster |
| Memory | 0 (no daemon) | 100MB+ | 50MB | 1GB+ |
| Persistence | Always | Optional | Optional | Always |
| Debug | `ls` | redis-cli | nats cli | Complex |
| Learning curve | Minutes | Hours | Hours | Days |

## Verdict

**APPROVED FOR PUBLIC RELEASE**

This is exactly the kind of "boring technology" that SREs love:
- Simple to understand
- Easy to debug
- No 3 AM pages
- No vendor lock-in
- No licensing costs

The "it's just files" approach means any Unix admin can troubleshoot it without learning a new system.

**Ops burden rating**: 1/10 (basically zero)

---

*Signed: DevOps/SRE Team*

## Appendix: Quick Runbook

### Message stuck in processing

```bash
# Check what's stuck
ls -la ~/.abq/channels/*/processing/

# Move back to requests (for retry)
mv ~/.abq/channels/myagent/processing/req_*.json ~/.abq/channels/myagent/requests/

# Or archive it (give up)
mv ~/.abq/channels/myagent/processing/req_*.json ~/.abq/channels/myagent/archive/
```

### Watcher not starting

```bash
# Check if channel exists
abq channel list | grep myagent

# Check handler is executable
ls -la ./handler.sh
chmod +x ./handler.sh

# Run with verbose output
abq watch myagent -H ./handler.sh -p 1.0  # Slower polling for debugging
```

### Disk filling up

```bash
# Find large archives
du -sh ~/.abq/channels/*/archive/

# Cleanup old archives
find ~/.abq/channels/*/archive/ -mtime +7 -delete
```
