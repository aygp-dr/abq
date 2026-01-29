"""Tests for the cross-platform file watcher."""

from pathlib import Path
from unittest.mock import patch

from abq.watcher import FileWatcher, _has_inotify, get_watcher_info


class TestWatcherDetection:
    """Test platform detection logic."""

    def test_get_watcher_info_returns_dict(self):
        """get_watcher_info returns expected keys."""
        info = get_watcher_info()
        assert "platform" in info
        assert "freebsd_version" in info
        assert "has_inotify" in info
        assert "inotify_backend" in info
        assert "has_watchdog" in info
        assert "recommended_backend" in info
        # inotify_backend should be one of the known values
        assert info["inotify_backend"] in ("inotify.adapters", "pyinotify", "none")

    def test_platform_detected(self):
        """Platform should be detected."""
        info = get_watcher_info()
        assert info["platform"] in ("Linux", "FreeBSD", "Darwin", "Windows")

    @patch("abq.watcher._PLATFORM", "Linux")
    def test_linux_checks_inotify_import(self):
        """Linux should check for inotify libraries."""
        # On Linux, _has_inotify checks if inotify.adapters or pyinotify is importable
        # The result depends on whether the libraries are installed
        result = _has_inotify()
        assert isinstance(result, bool)

    @patch("abq.watcher._PLATFORM", "FreeBSD")
    @patch("abq.watcher._FREEBSD_VERSION", 14)
    def test_freebsd_14_no_inotify(self):
        """FreeBSD 14 should not have inotify."""
        assert _has_inotify() is False

    @patch("abq.watcher._PLATFORM", "FreeBSD")
    @patch("abq.watcher._FREEBSD_VERSION", 15)
    def test_freebsd_15_checks_inotify_import(self):
        """FreeBSD 15 checks if inotify module is available."""
        # Will be False unless py-inotify is installed
        # Just verify it doesn't crash
        result = _has_inotify()
        assert isinstance(result, bool)


class TestFileWatcher:
    """Test FileWatcher class."""

    def test_watcher_init(self, tmp_path):
        """FileWatcher initializes with a directory."""
        watcher = FileWatcher(tmp_path)
        assert watcher.directory == tmp_path
        assert watcher.poll_interval == 0.5

    def test_watcher_custom_poll_interval(self, tmp_path):
        """FileWatcher accepts custom poll interval."""
        watcher = FileWatcher(tmp_path, poll_interval=1.0)
        assert watcher.poll_interval == 1.0

    def test_watcher_backend_is_string(self, tmp_path):
        """Backend should be a known string."""
        watcher = FileWatcher(tmp_path)
        assert watcher.backend in ("inotify", "watchdog", "polling")

    def test_watcher_expands_user_path(self):
        """FileWatcher expands ~ in paths."""
        watcher = FileWatcher(Path("~/.abq/test"))
        assert "~" not in str(watcher.directory)

    def test_directory_glob_finds_json(self, tmp_path):
        """Verify directory glob pattern works for JSON files."""
        # This tests the core logic used by polling
        (tmp_path / "test1.json").write_text("{}")
        (tmp_path / "test2.json").write_text("{}")
        (tmp_path / "ignore.txt").write_text("not json")

        files = list(tmp_path.glob("*.json"))
        assert len(files) == 2
        assert all(f.suffix == ".json" for f in files)


class TestFreeBSDVersionParsing:
    """Test FreeBSD version parsing edge cases."""

    def test_current_platform_info(self):
        """Current platform info is valid."""
        info = get_watcher_info()
        if info["platform"] == "FreeBSD":
            # Version should be an integer or None
            assert info["freebsd_version"] is None or isinstance(
                info["freebsd_version"], int
            )
