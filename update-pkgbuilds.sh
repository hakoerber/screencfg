#!/usr/bin/env bash

set -o nounset
set -o errexit

if git remote | grep -q ^aur$ ; then
    printf >&2 'aur remote already exists\n'
    exit 1
fi

git remote add aur "ssh://aur@aur.archlinux.org/screencfg.git"
trap "git remote rm aur 2>&1 || true" EXIT

git subtree push --prefix "pkg/arch/screencfg" aur master

git remote rm aur
