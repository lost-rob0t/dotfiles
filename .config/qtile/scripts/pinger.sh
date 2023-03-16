#!/usr/bin/env sh

while true; do
while read -r host; do
    ping -c 1 "$host" &> /dev/null || dunstify -a "Pinger" -u CRITICAL "$host is down!"
done < $1
sleep 30
done
