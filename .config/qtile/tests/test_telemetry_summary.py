import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts" / "qtile-telemetry-summary.py"


def record(event="session_start"):
    return {
        "schema_version": 1,
        "timestamp": "2026-09-21T03:00:00+00:00",
        "session": "test",
        "event": event,
        "auto_mode": False,
    }


class TelemetrySummaryIntegrityTests(unittest.TestCase):
    def run_summary(self, *paths, strict=False):
        command = [sys.executable, str(SCRIPT)]
        if strict:
            command.append("--fail-on-active-corruption")
        command.extend(str(path) for path in paths)
        result = subprocess.run(command, text=True, capture_output=True, check=False)
        return result, json.loads(result.stdout)

    def test_historical_rotation_corruption_is_reported_not_fatal(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            rotated = root / "telemetry.jsonl.3"
            active = root / "telemetry.jsonl"
            rotated.write_text(json.dumps(record("window_focus")) + "\n{broken\n", encoding="utf-8")
            active.write_text(json.dumps(record()) + "\n", encoding="utf-8")

            result, report = self.run_summary(rotated, active)
            self.assertEqual(result.returncode, 0)
            self.assertEqual(report["records"], 2)
            self.assertEqual(report["integrity"]["status"], "degraded")
            self.assertEqual(report["integrity"]["rotation_corrupt_records"], 1)
            self.assertEqual(report["integrity"]["active_corrupt_records"], 0)
            self.assertNotIn("broken", json.dumps(report))

    def test_active_corruption_can_fail_strict_integrity_after_report(self):
        with tempfile.TemporaryDirectory() as directory:
            active = Path(directory) / "telemetry.jsonl"
            active.write_text(json.dumps(record()) + "\n{broken\n", encoding="utf-8")

            normal, report = self.run_summary(active)
            self.assertEqual(normal.returncode, 0)
            self.assertEqual(report["records"], 1)
            self.assertEqual(report["integrity"]["active_corrupt_records"], 1)

            strict, strict_report = self.run_summary(active, strict=True)
            self.assertEqual(strict.returncode, 2)
            self.assertEqual(strict_report["integrity"]["active_corrupt_records"], 1)

    def test_non_object_json_is_integrity_failure_not_a_crash(self):
        with tempfile.TemporaryDirectory() as directory:
            active = Path(directory) / "telemetry.jsonl"
            active.write_text("[]\n" + json.dumps(record()) + "\n", encoding="utf-8")
            result, report = self.run_summary(active)
            self.assertEqual(result.returncode, 0)
            self.assertEqual(report["records"], 1)
            self.assertEqual(report["integrity"]["active_corrupt_records"], 1)


if __name__ == "__main__":
    unittest.main()
