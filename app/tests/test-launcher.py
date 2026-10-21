"""Safety/idempotence tests; never changes a live service or working document."""
from contextlib import redirect_stdout
import importlib.util
import io
from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest.mock import patch

root = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("builder_manager", root / "scripts/manage-builder.py")
manager = importlib.util.module_from_spec(spec)
spec.loader.exec_module(manager)


class LauncherGuards(unittest.TestCase):
    def test_unmanaged_port_is_left_untouched(self):
        with patch.object(manager, "job", return_value=None), patch.object(manager, "port_open", return_value=True), patch.object(manager, "launchctl") as launch:
            with self.assertRaisesRegex(RuntimeError, "left untouched"):
                manager.start()
            launch.assert_not_called()

    def test_repeated_start_reuses_process(self):
        current = {"pid": 1234, "target": manager.TARGET}
        with patch.object(manager, "job", return_value=current), patch.object(manager, "port_open", return_value=True), patch.object(manager, "healthy", return_value=True), patch.object(manager, "launchctl") as launch, redirect_stdout(io.StringIO()) as output:
            manager.start()
            self.assertIn('"status": "already running"', output.getvalue())
            self.assertIn('"pid": 1234', output.getvalue())
            launch.assert_not_called()

    def test_mismatched_job_is_never_stopped(self):
        other = SimpleNamespace(returncode=0, stdout="program = /different/app", stderr="")
        with patch.object(manager, "launchctl", return_value=other) as launch:
            with self.assertRaisesRegex(RuntimeError, "refusing to alter"):
                manager.stop()
            self.assertEqual(launch.call_count, 1)
            self.assertEqual(launch.call_args.args, ("print", manager.TARGET))

    def test_missing_job_does_not_signal_processes(self):
        with patch.object(manager, "job", return_value=None), patch.object(manager, "launchctl") as launch, redirect_stdout(io.StringIO()):
            manager.stop()
            launch.assert_not_called()


if __name__ == "__main__":
    unittest.main()
