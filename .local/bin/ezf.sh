#!/usr/bin/env bash
# source: https://www.masteringemacs.org/article/fuzzy-finding-emacs-instead-of-fzf

set -o nounset -o errexit -o pipefail

field=nil
# the elisp function to use for completing read
candidate_fn=ezf-default
while getopts c:f: OPT; do
    case $OPT in
        c)
            candidate_fn=$OPTARG
            ;;
        f)
            field=$OPTARG
            ;;
        *)
            echo "usage: ${0##*/} [-f field] [-c candidate-fn]"
            exit 2
    esac
done
shift $(( OPTIND - 1 ))
OPTIND=1

ezftmp="$(mktemp)"
trap 'rm -f -- "$ezftmp"' EXIT
> "$ezftmp" cat -
# xargs is there to strip the "" from the beginning and end of the output from Emacs.
selection=$(emacsclient  -e "(ezf \"$ezftmp\" $field #'$candidate_fn)" | xargs)
if [[ "$selection" == "nil" ]]; then
    exit 1
else
   echo "$selection"
fi
