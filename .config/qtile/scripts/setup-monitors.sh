#!/usr/bin/env sh

profile="$HOME/.config/qtile/scripts/monitors/$(id -un)@$(hostname).sh"

[ -x "$profile" ] || exit 0
exec "$profile"
