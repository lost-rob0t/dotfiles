#!/usr/bin/env sh

# this file wil load a list of ntfy topics and sub to them

topics_file="$HOME/.config/ntfy.txt"
while read -r topic; do
  # Run the command on the current topic
   "$topic"
   ntfy sub mytopic 'dunstify "$t" "$m"' &
done < "$topics_file"
