#!/usr/bin/env bash
# launch emacs
emacsclient -c -F "'(name . \"org-capture\"))" --eval "(org-capture)"
