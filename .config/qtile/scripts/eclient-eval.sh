#!/usr/bin/env sh
# launch emacs but also eval
emacsclient -s qtile -a false -c -F "'(name . \"floating\"))" --eval $1
