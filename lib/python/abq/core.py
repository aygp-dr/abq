"""
abq core library - filesystem-based message queue operations.
"""

import hashlib
import json
import os
import subprocess
import time
from datetime import datetime, timezone
from pathlib import Path

VERSION = "1.0.0"
ABQ_HOME = Path(os.environ.get("ABQ_HOME", Path.home() / ".abq"))


def ulid_simple() -> str:
    """Generate a simple time-sortable ID."""
    ts = int(time.time() * 1000)
    rand = hashlib.sha256(os.urandom(16)).hexdigest()[:10]
    return f"{ts:012x}{rand}"


def get_git_context(pwd: Path | None = None) -> dict:
    """Extract git context from current directory."""
    pwd = pwd or Path.cwd()

    try:
        # Get remote URL
        result = subprocess.run(
            ["git", "-C", str(pwd), "remote", "get-url", "origin"],
            capture_output=True, text=True, timeout=5
        )
        if result.returncode == 0:
            remote = result.stdout.strip()
            remote = remote.replace("git@github.com:", "github.com/")
            remote = remote.replace("https://github.com/", "github.com/")
            remote = remote.rstrip(".git")
        else:
            remote = "local"

        # Get branch
        result = subprocess.run(
            ["git", "-C", str(pwd), "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True, text=True, timeout=5
        )
        branch = result.stdout.strip() if result.returncode == 0 else "unknown"

        # Check if worktree
        result = subprocess.run(
            ["git", "-C", str(pwd), "rev-parse", "--git-common-dir"],
            capture_output=True, text=True, timeout=5
        )
        common_dir = Path(result.stdout.strip()) if result.returncode == 0 else None

        result = subprocess.run(
            ["git", "-C", str(pwd), "rev-parse", "--git-dir"],
            capture_output=True, text=True, timeout=5
        )
        git_dir = Path(result.stdout.strip()) if result.returncode == 0 else None

        is_worktree = common_dir and git_dir and common_dir.resolve() != git_dir.resolve()

        agent = f"{remote}/worktrees/{branch}" if is_worktree else f"{remote}/{branch}"

        return {
            "agent": agent,
            "remote": remote,
            "branch": branch,
            "is_worktree": is_worktree,
            "pwd": str(pwd)
        }

    except Exception as e:
        return {
            "agent": f"local/unknown/{pwd.name}",
            "remote": "local",
            "branch": "unknown",
            "is_worktree": False,
            "pwd": str(pwd),
            "error": str(e)
        }


def _get_channel_path(channel: str) -> Path:
    """Get path to channel directory."""
    return ABQ_HOME / "channels" / channel


def _resolve_address(addr: str, context: dict) -> str:
    """Resolve special addresses like @self, @parent."""
    if addr == "@self":
        return str(context["agent"])
    if addr == "@parent":
        agent = str(context["agent"])
        parts = agent.split("/worktrees/")
        if len(parts) > 1:
            return parts[0] + "/main"
        return agent
    return addr


def init() -> Path:
    """Initialize agent bus directory structure. Returns ABQ_HOME path."""
    ABQ_HOME.mkdir(parents=True, exist_ok=True)
    (ABQ_HOME / "channels").mkdir(exist_ok=True)
    (ABQ_HOME / "agents").mkdir(exist_ok=True)
    (ABQ_HOME / "logs").mkdir(exist_ok=True)

    # Create broadcast channel
    broadcast = ABQ_HOME / "channels" / "broadcast"
    for subdir in ["requests", "responses", "processing", "archive"]:
        (broadcast / subdir).mkdir(parents=True, exist_ok=True)

    # Initialize registry if not exists
    registry = ABQ_HOME / "registry.json"
    if not registry.exists():
        registry.write_text(json.dumps({
            "version": VERSION,
            "created": datetime.now(timezone.utc).isoformat(),
            "agents": {},
            "channels": {
                "broadcast": {
                    "type": "pubsub",
                    "description": "Global broadcast channel"
                }
            }
        }, indent=2))

    return ABQ_HOME


def channel_create(name: str) -> Path:
    """Create a new channel. Returns channel path."""
    channel_dir = ABQ_HOME / "channels" / name
    for subdir in ["requests", "responses", "processing", "archive"]:
        (channel_dir / subdir).mkdir(parents=True, exist_ok=True)
    return channel_dir


def channel_list() -> list[str]:
    """List all channels."""
    channels_dir = ABQ_HOME / "channels"
    if not channels_dir.exists():
        return []
    return sorted([ch.name for ch in channels_dir.iterdir() if ch.is_dir()])


def channel_remove(name: str) -> bool:
    """Remove a channel. Returns True if removed."""
    import shutil
    channel_dir = ABQ_HOME / "channels" / name
    if channel_dir.exists():
        shutil.rmtree(channel_dir)
        return True
    return False


def send(
    channel: str,
    msg_type: str,
    content: str,
    reply_to: str | None = None,
    ttl: int = 300
) -> dict:
    """Send a message to a channel. Returns the message dict."""
    context = get_git_context()
    channel = _resolve_address(channel, context)

    channel_path = _get_channel_path(channel)
    requests_dir = channel_path / "requests"

    if not requests_dir.exists():
        raise FileNotFoundError(
            f"Channel '{channel}' does not exist. Create with: abq channel create {channel}"
        )

    msg_id = f"req_{ulid_simple()}"

    message = {
        "id": msg_id,
        "version": VERSION,
        "type": msg_type,
        "from": {
            "agent": context["agent"],
            "pid": os.getpid(),
            "pwd": context["pwd"]
        },
        "to": channel,
        "content": content,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "ttl": ttl,
    }

    if reply_to:
        message["reply_to"] = reply_to

    msg_file = requests_dir / f"{msg_id}.json"
    msg_file.write_text(json.dumps(message, indent=2))

    return message


def recv(
    channel: str,
    wait: bool = False,
    timeout: int = 30
) -> dict | None:
    """Receive a message from a channel. Returns message dict or None."""
    channel_path = _get_channel_path(channel)
    requests_dir = channel_path / "requests"
    processing_dir = channel_path / "processing"

    if not requests_dir.exists():
        raise FileNotFoundError(f"Channel '{channel}' does not exist")

    processing_dir.mkdir(exist_ok=True)

    start = time.time()
    while True:
        messages = sorted(requests_dir.glob("*.json"))

        if messages:
            msg_file = messages[0]
            msg: dict = json.loads(msg_file.read_text())

            # Move to processing
            processing_file = processing_dir / msg_file.name
            msg_file.rename(processing_file)

            return msg

        if not wait:
            return None

        if time.time() - start > timeout:
            return None

        time.sleep(0.1)


def respond(
    channel: str,
    msg_id: str,
    status: str = "success",
    result: str = "",
    error: str | None = None
) -> dict:
    """Send a response to a received message. Returns response dict."""
    channel_path = _get_channel_path(channel)
    processing_dir = channel_path / "processing"
    responses_dir = channel_path / "responses"
    archive_dir = channel_path / "archive"

    responses_dir.mkdir(exist_ok=True)
    archive_dir.mkdir(exist_ok=True)

    processing_file = processing_dir / f"{msg_id}.json"
    if not processing_file.exists():
        raise FileNotFoundError(f"No processing message with id {msg_id}")

    context = get_git_context()

    response = {
        "id": msg_id,
        "version": VERSION,
        "status": status,
        "from": {
            "agent": context["agent"],
            "pid": os.getpid()
        },
        "result": result,
        "error": error,
        "timestamp": datetime.now(timezone.utc).isoformat()
    }

    # Write response
    response_file = responses_dir / f"{msg_id}.json"
    response_file.write_text(json.dumps(response, indent=2))

    # Archive original request
    processing_file.rename(archive_dir / f"{msg_id}_request.json")

    return response


def status() -> dict:
    """Get agent bus status. Returns status dict."""
    context = get_git_context()

    channels = {}
    channels_dir = ABQ_HOME / "channels"
    if channels_dir.exists():
        for ch in channels_dir.iterdir():
            if ch.is_dir():
                def _count(subdir):
                    d = ch / subdir
                    return len(list(d.glob("*.json"))) if d.exists() else 0

                channels[ch.name] = {
                    "requests": _count("requests"),
                    "processing": _count("processing"),
                    "responses": _count("responses"),
                }

    return {
        "home": str(ABQ_HOME),
        "agent": context["agent"],
        "pwd": context["pwd"],
        "is_worktree": context["is_worktree"],
        "channels": channels,
    }
