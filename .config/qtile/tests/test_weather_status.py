#!/usr/bin/env python3
"""Tests for the Qtile Open-Meteo helper."""

from __future__ import annotations

import importlib.util
import os
import sys
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "weather_status.py"
SPEC = importlib.util.spec_from_file_location("weather_status", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class WeatherStatusTests(unittest.TestCase):
    def test_configured_coordinates_do_not_require_geocoding(self):
        with mock.patch.dict(
            os.environ,
            {"WEATHER_LATITUDE": "40.0", "WEATHER_LONGITUDE": "-83.0", "WEATHER_LABEL": "Home"},
            clear=True,
        ):
            self.assertEqual(MODULE.configured_coordinates(), (40.0, -83.0, "Home"))

    def test_location_is_required_when_coordinates_are_absent(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            with self.assertRaisesRegex(RuntimeError, "WEATHER_LOCATION"):
                MODULE.resolve_location()

    def test_parse_current_and_render_imperial(self):
        payload = {
            "current": {
                "temperature_2m": 72.4,
                "apparent_temperature": 71.8,
                "precipitation": 0,
                "weather_code": 1,
                "wind_speed_10m": 8.2,
            }
        }
        parsed = MODULE.parse_current(payload, "Test")
        with mock.patch.dict(os.environ, {"WEATHER_UNITS": "imperial"}, clear=True):
            text = MODULE.render(parsed)
        self.assertIn("Test 72°F", text)
        self.assertIn("8 mph", text)

    def test_forecast_url_uses_open_meteo_and_no_api_key(self):
        with mock.patch.dict(os.environ, {"WEATHER_UNITS": "metric"}, clear=True):
            url = MODULE.weather_url(40.0, -83.0)
        self.assertTrue(url.startswith(MODULE.FORECAST_URL))
        self.assertIn("temperature_unit=celsius", url)
        self.assertNotIn("apikey", url.casefold())


if __name__ == "__main__":
    unittest.main()
