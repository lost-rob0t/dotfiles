#!/bin/sh

export XDG_SESSION_TYPE=x11
export XDG_CURRENT_DESKTOP=qtile
export DESKTOP_SESSION=qtile

systemctl --user import-environment \
  DISPLAY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP DESKTOP_SESSION

exec qtile start
