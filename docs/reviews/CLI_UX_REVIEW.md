# CLI UX Review: ABQ Discoverability and Design

**Reviewer**: Claude (Opus 4.5)
**Date**: 2025-01-28
**Status**: REVIEW COMPLETE

## Executive Summary

Assessed ABQ's CLI design against established patterns from git, aws, gh, and beads. Overall, ABQ follows modern conventions well but has opportunities to improve agent discoverability at startup.

## Comparison Matrix

| Feature | git | aws | gh | beads | **abq** |
|---------|-----|-----|----|----|-----|
| `--help` at all levels | ✓ | ✓ | ✓ | ✓ | ✓ |
| Hierarchical subcommands | ✓ | ✓ | ✓ | ✓ | ✓ |
| Tab completion | ✓ | ✓ | ✓ | - | ❌ |
| `--version` | ✓ | ✓ | ✓ | ✓ | ❌ |
| Machine-readable output (`--json`) | ✓ | ✓ | ✓ | ✓ | ✓ |
| Status/overview command | status | sts get-caller-identity | auth status | list | ✓ |
| Colored output | ✓ | ✓ | ✓ | ✓ | ❌ |
| Progress indicators | ✓ | - | ✓ | - | ❌ |
| Config file | .gitconfig | credentials | hosts.yml | - | ❌ |
| Environment variable override | ✓ | ✓ | ✓ | ✓ | ✓ |

## Discoverability Assessment

### Current State

```bash
$ abq --help
usage: abq [-h] {send,recv,respond,watch,status,ls} ...

Agent Bus Queue - file-based message bus for AI agents

positional arguments:
  {send,recv,respond,watch,status,ls}

optional arguments:
  -h, --help            show this help message and exit
```

### Recommendations for Agent Discoverability

#### 1. Add Startup Discovery Banner (P1)

Like `gh auth status` or `aws sts get-caller-identity`, provide immediate context:

```bash
$ abq
ABQ (Agent Bus Queue) v1.0.0

Current agent: github.com/myorg/repo/main
Home: ~/.abq
Channels: 3 (test-runner, app-agent, broadcast)
Pending: 2 messages

Commands:
  send      Send a message to a channel
  recv      Receive a message from a channel
  watch     Watch channel and run handler
  status    Show agent bus status

Run 'abq <command> --help' for details.
```

#### 2. Add `--version` Flag (P1)

Every well-designed CLI has this:

```python
parser.add_argument('--version', action='version', version='%(prog)s 1.0.0')
```

#### 3. Add Tab Completion Script (P2)

Generate completion scripts like gh:

```bash
$ abq completion bash > ~/.local/share/bash-completion/completions/abq
$ abq completion zsh > ~/.zsh/completions/_abq
$ abq completion fish > ~/.config/fish/completions/abq.fish
```

#### 4. Add Colored Output (P2)

Use ANSI colors for readability:
- Green for success
- Red for errors
- Yellow for warnings
- Cyan for message IDs

## Command Hierarchy Analysis

### Current Structure

```
abq
├── send <channel> <type> [content]
├── recv <channel>
├── respond <channel> <id>
├── watch <channel> --handler <script>
├── status
└── ls <channel> [subdir]
```

### Comparison with Reference CLIs

**git pattern** (verb-noun):
```
git add <file>
git commit -m <msg>
git push origin main
```

**aws pattern** (service-resource-action):
```
aws s3 ls
aws ec2 describe-instances
aws lambda invoke
```

**gh pattern** (resource-verb):
```
gh repo create
gh pr list
gh issue view 123
```

**beads pattern** (verb focused):
```
bd create "title"
bd list
bd ready
bd close <id>
```

### ABQ Aligns With

ABQ follows the **beads/git verb-focused pattern**, which is appropriate for:
- Simple domain (messages, channels)
- Single-purpose tool
- Unix philosophy (do one thing well)

## Flag Ordering Semantics

### Current Flags

| Command | Positional | Optional |
|---------|------------|----------|
| send | channel, type, [content] | --reply-to, --ttl, --wait, --timeout, --json |
| recv | channel | --wait, --json |
| respond | channel, id | --status, --result, --error, --json |
| watch | channel | --handler, --poll, --timeout |
| ls | channel, [subdir] | --limit, --oldest-first, --count, --json |

### Assessment

**Strengths:**
- Consistent `--json` flag across all commands (machine-readable output)
- Positional args for required values (channel, type)
- Optional flags for modifiers

**Weaknesses:**
- `--handler` should be positional in `watch` (always required)
- `-H` for `--handler` conflicts with common `-h` for help muscle memory

### Recommended Changes

```bash
# Current
abq watch my-agent --handler ./script.sh

# Proposed (handler as positional)
abq watch my-agent ./script.sh

# Or use subcommand pattern like docker
abq watch my-agent -e ./script.sh  # -e for "execute"
```

## Error Messages

### Current

```
Error: Channel 'xyz' does not exist
Create it with: abq-channel create xyz
```

### Recommended Improvements

```
Error: Channel 'xyz' not found

Did you mean:
  my-agent
  test-runner

To create 'xyz':
  abq channel create xyz

Run 'abq channel list' to see all channels.
```

## Machine-Readable Output Patterns

### Current: `--json` flag

ABQ correctly implements JSON output:

```bash
$ abq send my-agent signal '{}' --json
{
  "id": "req_018d5a3b2c4e",
  "version": "1.0.0",
  ...
}
```

### Recommendation: Add `--format` Option

Like aws/gh, support multiple formats:

```bash
$ abq ls my-agent --format json    # Default for scripts
$ abq ls my-agent --format table   # Default for humans
$ abq ls my-agent --format csv     # For spreadsheets
```

## Environment Variables

### Current

| Variable | Purpose |
|----------|---------|
| ABQ_HOME | Override ~/.abq |

### Recommended Additions

| Variable | Purpose |
|----------|---------|
| ABQ_HOME | Override ~/.abq |
| ABQ_DEFAULT_CHANNEL | Default channel if not specified |
| ABQ_DEFAULT_HANDLER | Default handler script |
| ABQ_POLL_INTERVAL | Default polling interval (ms) |
| ABQ_COLORS | Enable/disable colors (auto/always/never) |
| ABQ_DEBUG | Enable debug output |

## Agent Startup Discovery Protocol

For AI agents to discover ABQ on startup:

### 1. Status Command Output

```bash
$ abq status --json
{
  "version": "1.0.0",
  "home": "/home/user/.abq",
  "agent": {
    "id": "github.com/myorg/repo/main",
    "pid": 12345,
    "pwd": "/path/to/repo"
  },
  "channels": [
    {"name": "test-runner", "pending": 0, "processing": 0},
    {"name": "app-agent", "pending": 2, "processing": 1}
  ],
  "capabilities": ["send", "recv", "watch", "respond"]
}
```

### 2. Machine-Readable Help

Like `gh --help --json` (proposed):

```bash
$ abq --help --json
{
  "name": "abq",
  "version": "1.0.0",
  "commands": [
    {
      "name": "send",
      "description": "Send a message to a channel",
      "args": [
        {"name": "channel", "required": true},
        {"name": "type", "required": true, "choices": ["eval", "command", "signal", "status"]},
        {"name": "content", "required": false, "default": "-"}
      ]
    }
  ]
}
```

### 3. Discovery Protocol for MCP Integration

```json
{
  "protocol": "abq",
  "version": "1.0.0",
  "discovery": {
    "check": "abq status --json",
    "channels": "abq channel list --json",
    "capabilities": ["send", "recv", "watch", "respond"]
  }
}
```

## Implementation Priority

### P0 (Before v1.0 Release)

- [x] `--help` at all levels
- [x] Hierarchical subcommands
- [x] `--json` for machine output
- [x] `abq status` overview command

### P1 (Improve Discoverability)

- [ ] Add `--version` flag
- [ ] Add startup banner when run without args
- [ ] Improve error messages with suggestions
- [ ] Add `abq channel` subcommand (group channel operations)

### P2 (Polish)

- [ ] Tab completion scripts
- [ ] Colored output
- [ ] `--format` option (json/table/csv)
- [ ] Config file support (~/.abqrc)

### P3 (Future)

- [ ] Interactive mode (`abq shell`)
- [ ] TUI for watching messages
- [ ] Plugin system for custom handlers

## References

- [Lucas Costa: UX Patterns for CLI Tools](https://www.lucasfcosta.com/blog/ux-patterns-cli-tools)
- [Agarau: CLI for Everybody - Content Designer's Guide](https://medium.com/@adedayoagarau/cli-for-everybody-a-content-designers-guide-to-command-line-ux-39d574b4cd86)
- [12 Factor CLI Apps](https://medium.com/@jdxcode/12-factor-cli-apps-dd3c227a0e46)
- [CLI Guidelines](https://clig.dev/)

## Verdict

**ABQ CLI is well-designed** for its purpose. It follows Unix conventions and provides machine-readable output. The main gap is **startup discoverability** - an agent or user running `abq` without arguments should immediately understand:

1. What ABQ is
2. What their current context is (agent identity)
3. What they can do (commands)
4. What state exists (channels, pending messages)

With the P1 improvements, ABQ will be fully agent-discoverable.

---

*Signed: Claude (Opus 4.5)*
