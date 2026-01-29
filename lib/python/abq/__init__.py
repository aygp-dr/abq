"""
abq - Agent Bus Queue

A minimal file-based message bus for AI agent coordination.

The name "abq" represents both patterns supported:
- Agent Bus: many-to-many pub/sub (broadcast channel)
- Agent Queue: point-to-point FIFO (named channels)

Usage:
    from abq import send, recv, channel_create

    channel_create("my-agent")
    send("my-agent", "signal", '{"done": true}')
    msg = recv("my-agent", wait=True)
"""

__version__ = "1.0.0"

from .core import (
    ABQ_HOME,
    channel_create,
    channel_list,
    channel_remove,
    get_git_context,
    init,
    recv,
    respond,
    send,
    status,
)

__all__ = [
    "ABQ_HOME",
    "get_git_context",
    "send",
    "recv",
    "respond",
    "channel_create",
    "channel_list",
    "channel_remove",
    "init",
    "status",
]
