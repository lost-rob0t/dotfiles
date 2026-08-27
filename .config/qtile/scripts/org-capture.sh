#!/usr/bin/env bash
# launch emacs
emacsclient -s qtile -a false -c -F "'(name . \"org-capture\"))" --eval "(org-capture)"
