#!/usr/bin/env sh

exec xrandr \
  --output HDMI-A-0 --mode 1920x1080 --rate 60.00 --pos 0x0 --rotate normal \
  --output DisplayPort-1 --primary --mode 1920x1080 --rate 60.00 --pos 1920x0 --rotate normal \
  --output DisplayPort-0 --mode 1920x1080 --rate 60.00 --pos 3840x0 --rotate normal \
  --output DisplayPort-2 --off
