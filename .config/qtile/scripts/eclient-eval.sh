#!/usr/bin/env sh
# launch emacs but also eval
emacsclient -c -F "'(name . \"floating\"))" --eval $1
