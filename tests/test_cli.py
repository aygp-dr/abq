"""Tests for the CLI module."""

import json
import subprocess
import sys
from argparse import Namespace

import abq.core as _core
import pytest
from abq import cli


class TestCmdCheck:
    """Test cmd_check function directly."""

    def test_check_clean(self, tmp_path, monkeypatch, capsys):
        """Check passes on clean state."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        (tmp_path / "channels").mkdir()

        args = Namespace(verbose=False)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_check(args)
        assert exc.value.code == 0

        captured = capsys.readouterr()
        assert "checks passed" in captured.out

    def test_check_verbose(self, tmp_path, monkeypatch, capsys):
        """Check verbose shows details."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        (tmp_path / "channels").mkdir()

        args = Namespace(verbose=True)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_check(args)
        assert exc.value.code == 0

        captured = capsys.readouterr()
        assert "✓ ABQ home exists" in captured.out

    def test_check_no_home(self, tmp_path, monkeypatch, capsys):
        """Check fails when home missing."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path / "nonexistent"))

        args = Namespace(verbose=False)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_check(args)
        assert exc.value.code == 1


class TestCmdGc:
    """Test cmd_gc function directly."""

    def test_gc_empty(self, tmp_path, monkeypatch, capsys):
        """GC on empty channels."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        (tmp_path / "channels").mkdir()

        args = Namespace(verbose=False)
        cli.cmd_gc(args)

        captured = capsys.readouterr()
        assert "Archived 0" in captured.out


class TestCmdRequeue:
    """Test cmd_requeue function directly."""

    def test_requeue_empty(self, tmp_path, monkeypatch, capsys):
        """Requeue on empty channels."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        (tmp_path / "channels").mkdir()

        args = Namespace(verbose=False)
        cli.cmd_requeue(args)

        captured = capsys.readouterr()
        assert "Requeued 0" in captured.out


class TestCmdVersion:
    """Test cmd_version function directly."""

    def test_version(self, capsys):
        """Version prints version string."""
        args = Namespace()
        cli.cmd_version(args)

        captured = capsys.readouterr()
        assert "abq" in captured.out


class TestCmdStatus:
    """Test cmd_status function directly."""

    def test_status(self, tmp_path, monkeypatch, capsys):
        """Status shows info."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        (tmp_path / "channels").mkdir()
        (tmp_path / "registry.json").write_text("{}")

        args = Namespace()
        cli.cmd_status(args)

        captured = capsys.readouterr()
        assert "Agent Bus Queue Status" in captured.out
        assert "Channels:" in captured.out


class TestCmdInit:
    """Test cmd_init function directly."""

    def test_init(self, capsys):
        """Init prints message."""
        args = Namespace()
        cli.cmd_init(args)

        captured = capsys.readouterr()
        assert "initialized" in captured.out


class TestCmdChannel:
    """Test cmd_channel function directly."""

    def test_channel_create(self, tmp_path, monkeypatch, capsys):
        """Create a channel."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        (tmp_path / "channels").mkdir()

        args = Namespace(action="create", name="test-ch")
        cli.cmd_channel(args)

        captured = capsys.readouterr()
        assert "test-ch" in captured.out

    def test_channel_list(self, capsys):
        """List channels."""
        # Uses actual ABQ_HOME
        args = Namespace(action="list", name=None)
        cli.cmd_channel(args)
        # Just verify it runs without error


class TestCmdSend:
    """Test cmd_send function directly."""

    def test_send_signal(self, tmp_path, monkeypatch, capsys):
        """Send a signal."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        ch_path = tmp_path / "channels" / "test-ch"
        ch_path.mkdir(parents=True)
        (ch_path / "requests").mkdir()

        args = Namespace(
            channel="test-ch",
            type="signal",
            content='{"test": true}',
            reply_to=None,
            ttl=300,
            wait=False,
            json=False,
        )
        cli.cmd_send(args)

        captured = capsys.readouterr()
        assert "Sent:" in captured.out


class TestCmdRecv:
    """Test cmd_recv function directly."""

    def test_recv_empty(self, tmp_path, monkeypatch):
        """Recv on empty channel exits 0."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        ch_path = tmp_path / "channels" / "test-ch"
        ch_path.mkdir(parents=True)
        (ch_path / "requests").mkdir()
        (ch_path / "processing").mkdir()

        args = Namespace(channel="test-ch", wait=False, json=False, timeout=30)
        # recv returns None and exits 0 when empty
        try:
            cli.cmd_recv(args)
        except SystemExit as e:
            assert e.code == 0


class TestCmdLs:
    """Test cmd_ls function directly."""

    def test_ls_empty(self, tmp_path, monkeypatch, capsys):
        """List empty channel."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        ch_path = tmp_path / "channels" / "test-ch"
        ch_path.mkdir(parents=True)
        (ch_path / "requests").mkdir()

        args = Namespace(
            channel="test-ch",
            subdir=None,
            limit=20,
            oldest_first=False,
            count=False,
            json=False,
        )
        cli.cmd_ls(args)
        # No output for empty

    def test_ls_count(self, tmp_path, monkeypatch, capsys):
        """List with count."""
        import abq.core as _core

        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        ch_path = tmp_path / "channels" / "test-ch"
        ch_path.mkdir(parents=True)
        (ch_path / "requests").mkdir()

        args = Namespace(
            channel="test-ch",
            subdir=None,
            limit=20,
            oldest_first=False,
            count=True,
            json=False,
        )
        cli.cmd_ls(args)

        captured = capsys.readouterr()
        assert "0" in captured.out


class TestCLIHelp:
    """Test CLI help and basic commands."""

    def test_help(self):
        """CLI shows help."""
        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "--help"],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        assert "Agent Bus Queue" in result.stdout

    def test_version(self):
        """CLI shows version."""
        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "version"],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        assert "abq" in result.stdout


class TestCLIStatus:
    """Test status command."""

    def test_status(self, tmp_path, monkeypatch):
        """Status command works."""
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        # Create minimal structure
        (tmp_path / "channels").mkdir()
        (tmp_path / "registry.json").write_text("{}")

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "status"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0
        assert "Agent Bus Queue Status" in result.stdout


class TestCLICheck:
    """Test check command."""

    def test_check_clean(self, tmp_path):
        """Check command passes on clean state."""
        # Create minimal structure
        (tmp_path / "channels").mkdir()

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "check"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0
        assert "checks passed" in result.stdout

    def test_check_verbose(self, tmp_path):
        """Check command verbose mode."""
        (tmp_path / "channels").mkdir()

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "check", "-v"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0
        assert "✓" in result.stdout


class TestCLIGc:
    """Test gc command."""

    def test_gc_empty(self, tmp_path):
        """GC on empty channels."""
        (tmp_path / "channels").mkdir()

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "gc"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0
        assert "Archived 0" in result.stdout


class TestCLIRequeue:
    """Test requeue command."""

    def test_requeue_empty(self, tmp_path):
        """Requeue on empty channels."""
        (tmp_path / "channels").mkdir()

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "requeue"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0
        assert "Requeued 0" in result.stdout


class TestCLIChannel:
    """Test channel commands."""

    def test_channel_list_empty(self, tmp_path):
        """List channels when empty."""
        (tmp_path / "channels").mkdir()

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "channel", "list"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0

    def test_channel_create(self, tmp_path):
        """Create a channel."""
        (tmp_path / "channels").mkdir()

        result = subprocess.run(
            [sys.executable, "-m", "abq.cli", "channel", "create", "test-channel"],
            capture_output=True,
            text=True,
            env={**subprocess.os.environ, "ABQ_HOME": str(tmp_path)},
        )
        assert result.returncode == 0
        assert "test-channel" in result.stdout
        assert (tmp_path / "channels" / "test-channel").exists()


class TestCmdSendRecvRespond:
    """Test send → recv → respond cycle via direct cmd calls."""

    def _setup(self, tmp_path, monkeypatch):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()

    def test_send_json_output(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("ch1")
        args = Namespace(
            channel="ch1",
            type="signal",
            content='{"ok": true}',
            reply_to=None,
            ttl=300,
            wait=False,
            json=True,
        )
        cli.cmd_send(args)
        captured = capsys.readouterr()
        msg = json.loads(captured.out)
        assert msg["type"] == "signal"
        assert msg["to"] == "ch1"

    def test_send_error_no_channel(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        args = Namespace(
            channel="nonexistent",
            type="signal",
            content="{}",
            reply_to=None,
            ttl=300,
            wait=False,
            json=False,
        )
        with pytest.raises(SystemExit) as exc:
            cli.cmd_send(args)
        assert exc.value.code == 1

    def test_recv_message(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("ch2")
        _core.send("ch2", "command", '{"cmd": "test"}')
        args = Namespace(channel="ch2", wait=False, json=False, timeout=30)
        cli.cmd_recv(args)
        captured = capsys.readouterr()
        assert "ID:" in captured.out
        assert "Type: command" in captured.out

    def test_recv_json_output(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("ch3")
        _core.send("ch3", "signal", '{"x": 1}')
        args = Namespace(channel="ch3", wait=False, json=True, timeout=30)
        cli.cmd_recv(args)
        captured = capsys.readouterr()
        msg = json.loads(captured.out)
        assert msg["type"] == "signal"

    def test_recv_error_no_channel(self, tmp_path, monkeypatch):
        self._setup(tmp_path, monkeypatch)
        args = Namespace(channel="nonexistent", wait=False, json=False, timeout=30)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_recv(args)
        assert exc.value.code == 1

    def test_respond_success(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("ch4")
        sent = _core.send("ch4", "command", "do it")
        _core.recv("ch4")
        args = Namespace(
            channel="ch4",
            id=sent["id"],
            status="success",
            result="done",
            error=None,
            json=False,
        )
        cli.cmd_respond(args)
        captured = capsys.readouterr()
        assert "Response sent" in captured.out
        assert sent["id"] in captured.out

    def test_respond_json_output(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("ch5")
        sent = _core.send("ch5", "signal", "{}")
        _core.recv("ch5")
        args = Namespace(
            channel="ch5",
            id=sent["id"],
            status="success",
            result="ok",
            error=None,
            json=True,
        )
        cli.cmd_respond(args)
        captured = capsys.readouterr()
        resp = json.loads(captured.out)
        assert resp["status"] == "success"

    def test_respond_error_no_message(self, tmp_path, monkeypatch):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("ch6")
        args = Namespace(
            channel="ch6",
            id="req_fake",
            status="success",
            result="",
            error=None,
            json=False,
        )
        with pytest.raises(SystemExit) as exc:
            cli.cmd_respond(args)
        assert exc.value.code == 1


class TestCmdChannelDirect:
    """Test cmd_channel paths via direct call with proper ABQ_HOME patching."""

    def _setup(self, tmp_path, monkeypatch):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()

    def test_channel_create_no_name(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        args = Namespace(action="create", name=None)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_channel(args)
        assert exc.value.code == 1

    def test_channel_rm(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("removeme")
        args = Namespace(action="rm", name="removeme")
        cli.cmd_channel(args)
        captured = capsys.readouterr()
        assert "removed" in captured.out

    def test_channel_rm_no_name(self, tmp_path, monkeypatch):
        self._setup(tmp_path, monkeypatch)
        args = Namespace(action="rm", name=None)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_channel(args)
        assert exc.value.code == 1

    def test_channel_rm_nonexistent(self, tmp_path, monkeypatch):
        self._setup(tmp_path, monkeypatch)
        args = Namespace(action="rm", name="ghost")
        with pytest.raises(SystemExit) as exc:
            cli.cmd_channel(args)
        assert exc.value.code == 1

    def test_channel_list(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("listed")
        args = Namespace(action="list", name=None)
        cli.cmd_channel(args)
        captured = capsys.readouterr()
        assert "listed" in captured.out
        assert "broadcast" in captured.out


class TestCmdLsDirect:
    """Test cmd_ls with messages present."""

    def _setup(self, tmp_path, monkeypatch):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()

    def test_ls_with_messages(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("lsme")
        _core.send("lsme", "signal", '{"data": "test"}')
        _core.send("lsme", "command", "run")
        args = Namespace(
            channel="lsme",
            subdir=None,
            limit=20,
            oldest_first=False,
            count=False,
            json=False,
        )
        cli.cmd_ls(args)
        captured = capsys.readouterr()
        assert "signal" in captured.out
        assert "command" in captured.out

    def test_ls_json_output(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("lsjson")
        _core.send("lsjson", "signal", '{"x": 1}')
        args = Namespace(
            channel="lsjson",
            subdir=None,
            limit=20,
            oldest_first=True,
            count=False,
            json=True,
        )
        cli.cmd_ls(args)
        captured = capsys.readouterr()
        msg = json.loads(captured.out.strip())
        assert msg["type"] == "signal"

    def test_ls_nonexistent_subdir(self, tmp_path, monkeypatch):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("lsfail")
        args = Namespace(
            channel="lsfail",
            subdir="bogus",
            limit=20,
            oldest_first=False,
            count=False,
            json=False,
        )
        with pytest.raises(SystemExit) as exc:
            cli.cmd_ls(args)
        assert exc.value.code == 1

    def test_ls_count_with_messages(self, tmp_path, monkeypatch, capsys):
        self._setup(tmp_path, monkeypatch)
        _core.channel_create("lscount")
        _core.send("lscount", "signal", "{}")
        _core.send("lscount", "signal", "{}")
        args = Namespace(
            channel="lscount",
            subdir=None,
            limit=20,
            oldest_first=False,
            count=True,
            json=False,
        )
        cli.cmd_ls(args)
        captured = capsys.readouterr()
        assert "2" in captured.out


class TestCmdStatusDirect:
    """Test cmd_status with proper ABQ_HOME patching."""

    def test_status_with_channels(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()
        _core.channel_create("st-ch")
        _core.send("st-ch", "signal", "{}")
        args = Namespace()
        cli.cmd_status(args)
        captured = capsys.readouterr()
        assert "st-ch" in captured.out
        assert "1 pending" in captured.out


class TestCmdInitDirect:
    """Test cmd_init with proper ABQ_HOME patching."""

    def test_init_creates_structure(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        args = Namespace()
        cli.cmd_init(args)
        captured = capsys.readouterr()
        assert "initialized" in captured.out
        assert (tmp_path / "channels").exists()


class TestCmdGcDirect:
    """Test gc with stale messages."""

    def test_gc_archives_stale(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()
        _core.channel_create("gcch")
        # Send a message with ttl=0 so it's immediately stale
        msg = _core.send("gcch", "signal", "{}", ttl=0)
        args = Namespace(verbose=True)
        cli.cmd_gc(args)
        captured = capsys.readouterr()
        assert "Archived 1" in captured.out
        assert msg["id"] in captured.out

    def test_gc_no_channels(self, tmp_path, monkeypatch):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        args = Namespace(verbose=False)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_gc(args)
        assert exc.value.code == 1


class TestCmdRequeueDirect:
    """Test requeue with stuck messages."""

    def test_requeue_stuck(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()
        _core.channel_create("rqch")
        _core.send("rqch", "signal", "{}")
        _core.recv("rqch")  # moves to processing
        args = Namespace(verbose=True)
        cli.cmd_requeue(args)
        captured = capsys.readouterr()
        assert "Requeued 1" in captured.out

    def test_requeue_no_channels(self, tmp_path, monkeypatch):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        args = Namespace(verbose=False)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_requeue(args)
        assert exc.value.code == 1


class TestCmdCheckDirect:
    """Test check with stale/stuck messages."""

    def test_check_with_stale(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()
        _core.channel_create("ckch")
        _core.send("ckch", "signal", "{}", ttl=0)
        args = Namespace(verbose=True)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_check(args)
        assert exc.value.code == 0
        captured = capsys.readouterr()
        assert "stale" in captured.out.lower() or "⚠" in captured.out

    def test_check_with_stuck(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(_core, "ABQ_HOME", tmp_path)
        monkeypatch.setenv("ABQ_HOME", str(tmp_path))
        _core.init()
        _core.channel_create("stuckch")
        _core.send("stuckch", "signal", "{}")
        _core.recv("stuckch")  # moves to processing
        args = Namespace(verbose=False)
        with pytest.raises(SystemExit) as exc:
            cli.cmd_check(args)
        assert exc.value.code == 0
        captured = capsys.readouterr()
        assert "stuck" in captured.out.lower() or "processing" in captured.out.lower()
