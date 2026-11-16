#!/usr/bin/env sh
# launch emacs but also eval
emacsclient -a false -c -F "'(name . \"floating\"))" --eval $1
