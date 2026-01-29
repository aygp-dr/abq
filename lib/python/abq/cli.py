"""
abq CLI - Command line interface for Agent Bus Queue.
"""

import argparse
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from . import __version__
from .core import (
    ABQ_HOME,
    VERSION,
    get_git_context,
    init,
    channel_create,
    channel_list,
    channel_remove,
    send,
    recv,
    respond,
    status,
    _get_channel_path,
)


def wait_for_response(channel: str, msg_id: str, timeout: int = 30) -> dict | None:
    """Wait for a response to a specific message."""
    channel_path = _get_channel_path(channel)
    responses_dir = channel_path / "responses"

    start = time.time()
    while time.time() - start < timeout:
        response_file = responses_dir / f"{msg_id}.json"
        if response_file.exists():
            response = json.loads(response_file.read_text())
            archive_dir = channel_path / "archive"
            archive_dir.mkdir(exist_ok=True)
            response_file.rename(archive_dir / f"{msg_id}_response.json")
            return response
        time.sleep(0.1)
    return None


def cmd_init(args):
    """Initialize agent bus."""
    home = init()
    print(f"Agent bus initialized at {home}")


def cmd_channel(args):
    """Manage channels."""
    if args.action == "create":
        if not args.name:
            print("Error: channel name required", file=sys.stderr)
            sys.exit(1)
        path = channel_create(args.name)
        print(f"Channel '{args.name}' created at {path}")

    elif args.action == "list":
        channels = channel_list()
        for ch in channels:
            print(ch)

    elif args.action == "rm":
        if not args.name:
            print("Error: channel name required", file=sys.stderr)
            sys.exit(1)
        if channel_remove(args.name):
            print(f"Channel '{args.name}' removed")
        else:
            print(f"Channel '{args.name}' not found", file=sys.stderr)
            sys.exit(1)


def cmd_send(args):
    """Send a message."""
    content = args.content
    if content == "-":
        content = sys.stdin.read()

    try:
        msg = send(
            channel=args.channel,
            msg_type=args.type,
            content=content,
            reply_to=args.reply_to,
            ttl=args.ttl
        )
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    if args.json:
        print(json.dumps(msg, indent=2))
    else:
        print(f"Sent: {msg['id']} -> {args.channel}")

    if args.wait:
        print(f"Waiting for response (timeout: {args.timeout}s)...")
        response = wait_for_response(args.channel, msg["id"], args.timeout)
        if response:
            if args.json:
                print(json.dumps(response, indent=2))
            else:
                s = response.get("status", "unknown")
                r = response.get("result", "")
                print(f"Response [{s}]: {r[:200]}")
        else:
            print("Timeout waiting for response", file=sys.stderr)
            sys.exit(1)


def cmd_recv(args):
    """Receive a message."""
    try:
        msg = recv(
            channel=args.channel,
            wait=args.wait,
            timeout=getattr(args, 'timeout', 30)
        )
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    if msg is None:
        sys.exit(0)

    if args.json:
        print(json.dumps(msg, indent=2))
    else:
        print(f"ID: {msg['id']}")
        print(f"Type: {msg['type']}")
        print(f"From: {msg['from']['agent']}")
        print(f"Content: {msg['content'][:500]}")


def cmd_respond(args):
    """Send a response."""
    result = args.result
    if result == "-":
        result = sys.stdin.read()

    try:
        response = respond(
            channel=args.channel,
            msg_id=args.id,
            status=args.status,
            result=result,
            error=args.error
        )
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    if args.json:
        print(json.dumps(response, indent=2))
    else:
        print(f"Response sent: {args.id} [{args.status}]")


def cmd_watch(args):
    """Watch a channel and run handler."""
    channel_path = _get_channel_path(args.channel)
    requests_dir = channel_path / "requests"

    if not requests_dir.exists():
        print(f"Error: Channel '{args.channel}' does not exist", file=sys.stderr)
        sys.exit(1)

    print(f"Watching channel: {args.channel}")
    print(f"Handler: {args.handler}")

    while True:
        messages = sorted(requests_dir.glob("*.json"))

        for msg_file in messages:
            msg = json.loads(msg_file.read_text())
            msg_id = msg["id"]

            processing_dir = channel_path / "processing"
            processing_dir.mkdir(exist_ok=True)
            processing_file = processing_dir / msg_file.name
            msg_file.rename(processing_file)

            print(f"Processing: {msg_id}")

            try:
                env = os.environ.copy()
                env["ABQ_MSG_ID"] = msg_id
                env["ABQ_MSG_TYPE"] = msg["type"]
                env["ABQ_MSG_FROM"] = msg["from"]["agent"]
                env["ABQ_MSG_CONTENT"] = msg["content"]
                env["ABQ_CHANNEL"] = args.channel

                result = subprocess.run(
                    [args.handler],
                    input=json.dumps(msg),
                    capture_output=True,
                    text=True,
                    env=env,
                    timeout=args.timeout
                )

                handler_status = "success" if result.returncode == 0 else "error"
                output = result.stdout if result.returncode == 0 else result.stderr

            except subprocess.TimeoutExpired:
                handler_status = "error"
                output = f"Handler timeout after {args.timeout}s"
            except Exception as e:
                handler_status = "error"
                output = str(e)

            respond(
                channel=args.channel,
                msg_id=msg_id,
                status=handler_status,
                result=output.strip(),
                error=None if handler_status == "success" else output.strip()
            )

            print(f"  -> {handler_status}: {output[:100]}")

        time.sleep(args.poll)


def cmd_status(args):
    """Show status."""
    s = status()

    print("Agent Bus Queue Status")
    print("======================")
    print(f"Home: {s['home']}")
    print(f"Agent: {s['agent']}")
    print(f"PWD: {s['pwd']}")
    print(f"Worktree: {s['is_worktree']}")
    print()
    print("Channels:")
    for name, counts in sorted(s['channels'].items()):
        print(f"  {name}: {counts['requests']} pending, {counts['processing']} processing, {counts['responses']} responses")


def cmd_ls(args):
    """List messages in a channel."""
    channel_path = _get_channel_path(args.channel)
    subdir = args.subdir or "requests"
    target_dir = channel_path / subdir

    if not target_dir.exists():
        print(f"No {subdir} directory in channel '{args.channel}'", file=sys.stderr)
        sys.exit(1)

    messages = sorted(target_dir.glob("*.json"), reverse=not args.oldest_first)

    if args.count:
        print(len(messages))
        return

    for msg_file in messages[:args.limit]:
        msg = json.loads(msg_file.read_text())
        if args.json:
            print(json.dumps(msg))
        else:
            ts = msg.get("timestamp", "")[:19]
            from_agent = msg.get("from", {}).get("agent", "unknown")
            msg_type = msg.get("type", msg.get("status", "?"))
            content = msg.get("content", msg.get("result", ""))[:60]
            print(f"{msg['id'][:20]}  {ts}  {msg_type:8}  {from_agent[:30]}  {content}")


def cmd_sync_remote(args):
    """Sync channels with a remote host via rsync."""
    abq_home = os.environ.get("ABQ_HOME", os.path.expanduser("~/.abq"))
    channels_path = os.path.join(abq_home, "channels/")
    remote_path = f"{args.host}:{channels_path}"

    rsync_cmd = ["rsync", "-avz"]
    if args.delete:
        rsync_cmd.append("--delete")

    direction = args.direction

    if direction in ("down", "both"):
        cmd = rsync_cmd + [remote_path, channels_path]
        print(f"Pulling from {args.host}...")
        result = subprocess.run(cmd, capture_output=not args.verbose, text=True)
        if result.returncode != 0:
            print(f"Error pulling from {args.host}", file=sys.stderr)
            if not args.verbose and result.stderr:
                print(result.stderr, file=sys.stderr)
            sys.exit(1)
        if args.verbose:
            pass  # already printed by rsync
        else:
            print(f"Pull complete.")

    if direction in ("up", "both"):
        cmd = rsync_cmd + [channels_path, remote_path]
        print(f"Pushing to {args.host}...")
        result = subprocess.run(cmd, capture_output=not args.verbose, text=True)
        if result.returncode != 0:
            print(f"Error pushing to {args.host}", file=sys.stderr)
            if not args.verbose and result.stderr:
                print(result.stderr, file=sys.stderr)
            sys.exit(1)
        if args.verbose:
            pass
        else:
            print(f"Push complete.")


def cmd_version(args):
    """Show version."""
    print(f"abq {__version__}")


def main():
    parser = argparse.ArgumentParser(
        description="Agent Bus Queue - file-based message bus for AI agents",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  abq init
  abq channel create my-agent
  abq send my-agent signal '{"signal": "tests_passed"}'
  abq send app-agent command "deploy" --wait
  abq recv my-channel --wait
  abq respond my-channel req_xxx --status success --result "done"
  abq watch my-channel --handler ./process.sh
  abq status
        """
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    # init
    p_init = subparsers.add_parser("init", help="Initialize agent bus")
    p_init.set_defaults(func=cmd_init)

    # channel
    p_channel = subparsers.add_parser("channel", help="Manage channels")
    p_channel.add_argument("action", choices=["create", "list", "rm"])
    p_channel.add_argument("name", nargs="?", help="Channel name")
    p_channel.set_defaults(func=cmd_channel)

    # send
    p_send = subparsers.add_parser("send", help="Send a message")
    p_send.add_argument("channel", help="Target channel")
    p_send.add_argument("type", choices=["eval", "command", "signal", "status"])
    p_send.add_argument("content", nargs="?", default="-", help="Content (- for stdin)")
    p_send.add_argument("--reply-to", help="Reply channel")
    p_send.add_argument("--ttl", type=int, default=300, help="TTL in seconds")
    p_send.add_argument("--wait", "-w", action="store_true", help="Wait for response")
    p_send.add_argument("--timeout", "-t", type=int, default=30, help="Timeout")
    p_send.add_argument("--json", "-j", action="store_true", help="JSON output")
    p_send.set_defaults(func=cmd_send)

    # recv
    p_recv = subparsers.add_parser("recv", help="Receive a message")
    p_recv.add_argument("channel", help="Channel")
    p_recv.add_argument("--wait", "-w", action="store_true", help="Wait for message")
    p_recv.add_argument("--json", "-j", action="store_true", help="JSON output")
    p_recv.set_defaults(func=cmd_recv)

    # respond
    p_resp = subparsers.add_parser("respond", help="Send response")
    p_resp.add_argument("channel", help="Channel")
    p_resp.add_argument("id", help="Message ID")
    p_resp.add_argument("--status", "-s", default="success", choices=["success", "error", "pending"])
    p_resp.add_argument("--result", "-r", default="", help="Result (- for stdin)")
    p_resp.add_argument("--error", "-e", help="Error message")
    p_resp.add_argument("--json", "-j", action="store_true", help="JSON output")
    p_resp.set_defaults(func=cmd_respond)

    # watch
    p_watch = subparsers.add_parser("watch", help="Watch channel")
    p_watch.add_argument("channel", help="Channel")
    p_watch.add_argument("--handler", "-H", required=True, help="Handler script")
    p_watch.add_argument("--poll", "-p", type=float, default=0.5, help="Poll interval")
    p_watch.add_argument("--timeout", "-t", type=int, default=60, help="Handler timeout")
    p_watch.set_defaults(func=cmd_watch)

    # status
    p_status = subparsers.add_parser("status", help="Show status")
    p_status.set_defaults(func=cmd_status)

    # ls
    p_ls = subparsers.add_parser("ls", help="List messages")
    p_ls.add_argument("channel", help="Channel")
    p_ls.add_argument("subdir", nargs="?", help="Subdir (requests/responses/processing/archive)")
    p_ls.add_argument("--limit", "-n", type=int, default=20, help="Max messages")
    p_ls.add_argument("--oldest-first", "-o", action="store_true")
    p_ls.add_argument("--count", "-c", action="store_true", help="Just count")
    p_ls.add_argument("--json", "-j", action="store_true", help="JSON output")
    p_ls.set_defaults(func=cmd_ls)

    # sync-remote
    p_sync = subparsers.add_parser("sync-remote", help="Sync channels with remote host via rsync")
    p_sync.add_argument("host", help="Remote host (e.g., myhost, user@host)")
    p_sync.add_argument("direction", nargs="?", default="down",
                        choices=["up", "down", "both"], help="Sync direction (default: down)")
    p_sync.add_argument("--delete", action="store_true", help="Delete extraneous files on receiver")
    p_sync.add_argument("--verbose", "-v", action="store_true", help="Show rsync output")
    p_sync.set_defaults(func=cmd_sync_remote)

    # version
    p_version = subparsers.add_parser("version", help="Show version")
    p_version.set_defaults(func=cmd_version)

    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
