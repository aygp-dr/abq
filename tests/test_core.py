"""Tests for abq core functionality."""

import json
import os
import tempfile

import pytest

# Set ABQ_HOME before importing
_test_home = tempfile.mkdtemp(prefix="abq_test_")
os.environ["ABQ_HOME"] = _test_home

from abq import (  # noqa: E402
    channel_create,
    channel_list,
    channel_remove,
    init,
    recv,
    respond,
    send,
    status,
)


@pytest.fixture(autouse=True)
def setup_abq_home(tmp_path):
    """Set up a fresh ABQ_HOME for each test."""
    os.environ["ABQ_HOME"] = str(tmp_path)
    # Reimport to pick up new home
    import abq.core
    abq.core.ABQ_HOME = tmp_path
    init()
    yield tmp_path


class TestInit:
    def test_init_creates_directories(self, setup_abq_home):
        home = setup_abq_home
        assert (home / "channels").exists()
        assert (home / "agents").exists()
        assert (home / "logs").exists()
        assert (home / "channels" / "broadcast").exists()

    def test_init_creates_registry(self, setup_abq_home):
        home = setup_abq_home
        registry = home / "registry.json"
        assert registry.exists()
        data = json.loads(registry.read_text())
        assert data["version"] == "1.0.0"
        assert "broadcast" in data["channels"]


class TestChannels:
    def test_channel_create(self, setup_abq_home):
        _home = setup_abq_home
        path = channel_create("test-agent")
        assert path.exists()
        assert (path / "requests").exists()
        assert (path / "responses").exists()
        assert (path / "processing").exists()
        assert (path / "archive").exists()

    def test_channel_list(self, setup_abq_home):
        channel_create("agent-a")
        channel_create("agent-b")
        channels = channel_list()
        assert "agent-a" in channels
        assert "agent-b" in channels
        assert "broadcast" in channels

    def test_channel_remove(self, setup_abq_home):
        home = setup_abq_home
        channel_create("temp-agent")
        assert (home / "channels" / "temp-agent").exists()
        result = channel_remove("temp-agent")
        assert result is True
        assert not (home / "channels" / "temp-agent").exists()

    def test_channel_remove_nonexistent(self, setup_abq_home):
        result = channel_remove("nonexistent")
        assert result is False


class TestSendRecv:
    def test_send_creates_message(self, setup_abq_home):
        home = setup_abq_home
        channel_create("test-channel")

        msg = send("test-channel", "signal", '{"done": true}')

        assert msg["id"].startswith("req_")
        assert msg["type"] == "signal"
        assert msg["content"] == '{"done": true}'
        assert msg["to"] == "test-channel"

        # Check file was created
        requests_dir = home / "channels" / "test-channel" / "requests"
        files = list(requests_dir.glob("*.json"))
        assert len(files) == 1

    def test_send_to_nonexistent_channel(self, setup_abq_home):
        with pytest.raises(FileNotFoundError):
            send("nonexistent", "signal", "test")

    def test_recv_gets_message(self, setup_abq_home):
        home = setup_abq_home
        channel_create("test-channel")

        sent = send("test-channel", "signal", '{"x": 1}')
        received = recv("test-channel")

        assert received is not None
        assert received["id"] == sent["id"]
        assert received["content"] == '{"x": 1}'

        # Check file moved to processing
        requests_dir = home / "channels" / "test-channel" / "requests"
        processing_dir = home / "channels" / "test-channel" / "processing"
        assert len(list(requests_dir.glob("*.json"))) == 0
        assert len(list(processing_dir.glob("*.json"))) == 1

    def test_recv_empty_channel(self, setup_abq_home):
        channel_create("empty-channel")
        received = recv("empty-channel", wait=False)
        assert received is None

    def test_recv_nonexistent_channel(self, setup_abq_home):
        with pytest.raises(FileNotFoundError):
            recv("nonexistent")


class TestRespond:
    def test_respond_creates_response(self, setup_abq_home):
        home = setup_abq_home
        channel_create("test-channel")

        sent = send("test-channel", "command", "do something")
        recv("test-channel")  # Move to processing

        response = respond("test-channel", sent["id"], status="success", result="done")

        assert response["id"] == sent["id"]
        assert response["status"] == "success"
        assert response["result"] == "done"

        # Check files
        responses_dir = home / "channels" / "test-channel" / "responses"
        archive_dir = home / "channels" / "test-channel" / "archive"
        assert len(list(responses_dir.glob("*.json"))) == 1
        assert len(list(archive_dir.glob("*_request.json"))) == 1

    def test_respond_nonexistent_message(self, setup_abq_home):
        channel_create("test-channel")
        with pytest.raises(FileNotFoundError):
            respond("test-channel", "nonexistent_id")


class TestStatus:
    def test_status_returns_info(self, setup_abq_home):
        channel_create("agent-1")
        send("agent-1", "signal", "test")

        s = status()

        assert "home" in s
        assert "agent" in s
        assert "channels" in s
        assert "agent-1" in s["channels"]
        assert s["channels"]["agent-1"]["requests"] == 1


class TestMessageTypes:
    @pytest.mark.parametrize("msg_type", ["eval", "command", "signal", "status"])
    def test_all_message_types(self, setup_abq_home, msg_type):
        channel_create("typed-channel")
        msg = send("typed-channel", msg_type, "content")
        assert msg["type"] == msg_type
