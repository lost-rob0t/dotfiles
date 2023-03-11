#!/usr/bin/env sh

repo=$0
source=$2
repoList=$3
sshKey=$4
export GIT_SSH=$sshKey

function cloneOrigin {
    git checkout
}
