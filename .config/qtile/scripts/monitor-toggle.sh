#!/usr/bin/env sh

state_file="/tmp/displayport_state.txt"

# Check if the state file exists
if [ -f "$state_file" ]; then
  # Read the current state from the file
  state=$(cat "$state_file")

  if [ "$state" == "on" ]; then
    # DisplayPort is currently on, so turn it off
    xrandr --output DisplayPort-2 --off
    echo "off" > "$state_file"
  else
    # DisplayPort is currently off, so turn it on
    xrandr --output DisplayPort-2 --right-of HDMI-A-0 --auto
    echo "on" > "$state_file"
  fi
else
  # State file doesn't exist, assume DisplayPort is off
  xrandr --output DisplayPort-2 --auto
  echo "on" > "$state_file"
fi
