"""Tests for the CLI module."""

import subprocess
import sys
from argparse import Namespace

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
