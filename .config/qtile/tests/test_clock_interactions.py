#!/usr/bin/env python3
"""Lightweight source contracts for Qtile clock interactions."""

from pathlib import Path
import unittest

SOURCE = (Path(__file__).resolve().parents[1] / "qtile_control.py").read_text(encoding="utf-8")


class ClockInteractionTests(unittest.TestCase):
    def test_full_date_opens_single_day_org_agenda(self):
        self.assertIn('dropdown_toggle("org-agenda-day")', SOURCE)
        self.assertIn("org-agenda-list nil (current-time) 1", SOURCE)

    def test_time_only_clock_uses_dunst_month_calendar(self):
        self.assertIn("lazy.function(_show_month_calendar)", SOURCE)
        self.assertIn("calendar.TextCalendar", SOURCE)
        self.assertIn('_notify(stamp.strftime("%B %Y"), month_calendar_text(stamp))', SOURCE)


if __name__ == "__main__":
    unittest.main()
