# ADR-001: Transport Selection

## Status

Accepted

## Date

2025-01-28

## Context

We need to coordinate AI agents across repositories/worktrees on the same machine. Primary use case: test agent signals app agent when tests complete (~2 messages/day).

## Options Considered

### Option 1: Enterprise Message Bus

- 6 Kafka brokers (HA)
- 3-node ZooKeeper ensemble (or KRaft)
- Schema Registry (Avro)
- Kafka Connect (filesystem sink for debugging)
- Cruise Control (rebalancing)
- Burrow (consumer lag monitoring)
- Prometheus + Grafana (observability)
- Jaeger (distributed tracing)

**Estimated cost**: $847/month
**Ops burden**: High (dedicated platform team recommended)
**Resume impact**: Excellent

### Option 2: Write JSON to a Folder

```bash
echo '{"done":true}' > ~/.aq/channels/app/requests/req_001.json
```

**Estimated cost**: $0
**Ops burden**: `ls`
**Resume impact**: Negligible

## Decision

Option 2.

## Rationale

- The failure mode is "I can literally `ls` the queue and see what's stuck"
- Debuggable with `cat` and `jq`
- Works from any language that can write files
- Survives restarts (it's just files)
- No daemon to babysit
- Polling at 100ms is fine for "tests finished"

## Consequences

- LinkedIn will not be impressed
- Cannot attend KafkaSummit as a speaker
- Will need to find other ways to mass overkill in Q3

## Observability Gap (Accepted Risk)

We lack distributed tracing for the 47-character journey from `requests/` to `processing/`. We may never know if a message briefly paused to contemplate existence between directories.

Will revisit if we scale to 3 messages/day.

## References

- [Efrit queue system](https://github.com/steveyegge/efrit)
- [Unix spool pattern](https://en.wikipedia.org/wiki/Spooling)
- The look on someone's face when you say "it's a folder" and they wait for the rest of the sentence
