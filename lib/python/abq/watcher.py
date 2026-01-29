"""
abq watcher - cross-platform file watching with auto-detection.

Automatically selects the best available file watching method:
- Linux: inotify
- FreeBSD 15+: inotify (via libinotify compatibility)
- FreeBSD <15 / macOS: kqueue (via watchdog)
- Fallback: polling
"""

import platform
import sys
import time
from collections.abc import Callable
from pathlib import Path

# Detect platform capabilities
_PLATFORM = platform.system()
_FREEBSD_VERSION: int | None = None

if _PLATFORM == "FreeBSD":
    try:
        # Extract major version from release string like "15.0-RELEASE"
        release = platform.release()
        _FREEBSD_VERSION = int(release.split(".")[0])
    except (ValueError, IndexError):
        _FREEBSD_VERSION = None


def _has_inotify() -> bool:
    """Check if inotify is available (Linux or FreeBSD 15+)."""
    if _PLATFORM == "Linux":
        # Try inotify.adapters first, then pyinotify
        try:
            import inotify.adapters  # noqa: F401

            return True
        except ImportError:
            pass
        try:
            import pyinotify  # noqa: F401

            return True
        except ImportError:
            return False
    if _PLATFORM == "FreeBSD" and _FREEBSD_VERSION and _FREEBSD_VERSION >= 15:
        # FreeBSD uses pyinotify with libinotify
        try:
            import pyinotify  # noqa: F401

            return True
        except ImportError:
            pass
        try:
            import inotify.adapters  # noqa: F401

            return True
        except ImportError:
            return False
    return False


def _get_inotify_backend() -> str:
    """Determine which inotify library is available."""
    try:
        import inotify.adapters  # noqa: F401

        return "inotify.adapters"
    except ImportError:
        pass
    try:
        import pyinotify  # noqa: F401

        return "pyinotify"
    except ImportError:
        pass
    return "none"


def _has_watchdog() -> bool:
    """Check if watchdog library is available."""
    try:
        from watchdog.observers import Observer  # noqa: F401

        return True
    except ImportError:
        return False


class FileWatcher:
    """
    Cross-platform file watcher with automatic backend selection.

    Usage:
        watcher = FileWatcher(Path("~/.abq/channels/my-agent/requests"))
        for filepath in watcher.watch():
            process(filepath)
    """

    def __init__(self, directory: Path, poll_interval: float = 0.5):
        self.directory = Path(directory).expanduser().resolve()
        self.poll_interval = poll_interval
        self._backend = self._select_backend()

    def _select_backend(self) -> str:
        """Select the best available watching backend."""
        if _has_inotify():
            return "inotify"
        if _has_watchdog():
            return "watchdog"
        return "polling"

    @property
    def backend(self) -> str:
        """Return the name of the selected backend."""
        return self._backend

    def watch(self, callback: Callable[[Path], None] | None = None):
        """
        Watch directory for new files.

        Args:
            callback: Optional function to call with each new file path.
                     If None, yields file paths as a generator.

        Yields:
            Path objects for new files (if callback is None)
        """
        if self._backend == "inotify":
            yield from self._watch_inotify(callback)
        elif self._backend == "watchdog":
            yield from self._watch_watchdog(callback)
        else:
            yield from self._watch_polling(callback)

    def _watch_inotify(self, callback: Callable[[Path], None] | None = None):
        """Watch using inotify (Linux, FreeBSD 15+)."""
        backend = _get_inotify_backend()

        if backend == "inotify.adapters":
            yield from self._watch_inotify_adapters(callback)
        elif backend == "pyinotify":
            yield from self._watch_pyinotify(callback)
        else:
            # Fallback to polling if no inotify available
            yield from self._watch_polling(callback)

    def _watch_inotify_adapters(self, callback: Callable[[Path], None] | None = None):
        """Watch using inotify.adapters library."""
        import inotify.adapters

        i = inotify.adapters.Inotify()
        i.add_watch(str(self.directory))

        try:
            for event in i.event_gen(yield_nones=False):
                (_, type_names, path, filename) = event

                if "IN_CREATE" in type_names or "IN_MOVED_TO" in type_names:
                    if filename.endswith(".json"):
                        filepath = Path(path) / filename
                        if callback:
                            callback(filepath)
                        else:
                            yield filepath
        finally:
            i.remove_watch(str(self.directory))

    def _watch_pyinotify(self, callback: Callable[[Path], None] | None = None):
        """Watch using pyinotify library (FreeBSD 15+ with libinotify)."""
        import queue

        import pyinotify

        file_queue: queue.Queue[Path] = queue.Queue()

        class EventHandler(pyinotify.ProcessEvent):
            def process_IN_CREATE(self, event):  # noqa: N802
                if event.pathname.endswith(".json"):
                    file_queue.put(Path(event.pathname))

            def process_IN_MOVED_TO(self, event):  # noqa: N802
                if event.pathname.endswith(".json"):
                    file_queue.put(Path(event.pathname))

        wm = pyinotify.WatchManager()
        handler = EventHandler()
        notifier = pyinotify.ThreadedNotifier(wm, handler)
        notifier.start()

        mask = pyinotify.IN_CREATE | pyinotify.IN_MOVED_TO
        wm.add_watch(str(self.directory), mask)

        try:
            while True:
                try:
                    filepath = file_queue.get(timeout=self.poll_interval)
                    if callback:
                        callback(filepath)
                    else:
                        yield filepath
                except queue.Empty:
                    continue
        finally:
            notifier.stop()

    def _watch_watchdog(self, callback: Callable[[Path], None] | None = None):
        """Watch using watchdog (kqueue on BSD/macOS, inotify on Linux)."""
        import queue

        from watchdog.events import FileCreatedEvent, FileSystemEventHandler
        from watchdog.observers import Observer

        file_queue: queue.Queue[Path] = queue.Queue()

        class Handler(FileSystemEventHandler):
            def on_created(self, event: FileCreatedEvent):
                if not event.is_directory and event.src_path.endswith(".json"):
                    file_queue.put(Path(event.src_path))

        observer = Observer()
        observer.schedule(Handler(), str(self.directory), recursive=False)
        observer.start()

        try:
            while True:
                try:
                    filepath = file_queue.get(timeout=self.poll_interval)
                    if callback:
                        callback(filepath)
                    else:
                        yield filepath
                except queue.Empty:
                    continue
        finally:
            observer.stop()
            observer.join()

    def _watch_polling(self, callback: Callable[[Path], None] | None = None):
        """Watch using polling (fallback for all platforms)."""
        seen: set[str] = set()

        # Initialize with existing files
        if self.directory.exists():
            for f in self.directory.glob("*.json"):
                seen.add(str(f))

        while True:
            if self.directory.exists():
                for filepath in sorted(self.directory.glob("*.json")):
                    if str(filepath) not in seen:
                        seen.add(str(filepath))
                        if callback:
                            callback(filepath)
                        else:
                            yield filepath

            time.sleep(self.poll_interval)


def get_watcher_info() -> dict:
    """Return information about the file watching capabilities."""
    return {
        "platform": _PLATFORM,
        "freebsd_version": _FREEBSD_VERSION,
        "has_inotify": _has_inotify(),
        "inotify_backend": _get_inotify_backend(),
        "has_watchdog": _has_watchdog(),
        "recommended_backend": FileWatcher(Path("."))._select_backend(),
    }


if __name__ == "__main__":
    # Quick test / info display
    import json

    info = get_watcher_info()
    print(json.dumps(info, indent=2))

    if len(sys.argv) > 1:
        watch_dir = Path(sys.argv[1])
        watcher = FileWatcher(watch_dir)
        print(f"Watching {watch_dir} using {watcher.backend}...")
        for filepath in watcher.watch():
            print(f"New file: {filepath}")
