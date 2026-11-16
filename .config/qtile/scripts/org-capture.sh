#!/usr/bin/env bash
# launch emacs
emacsclient -a false -c -F "'(name . \"org-capture\"))" --eval "(org-capture)"
