#!/usr/bin/env bash

set -o nounset
set -o errexit

for pkg in screencfg screencfg-git ; do
    if git remote | grep -q ^aur$ ; then
        printf >&2 'aur remote already exists\n'
        exit 1
    fi

    git remote add aur "ssh://aur@aur.archlinux.org/${pkg}.git"
    trap "git remote rm aur 2>&1 || true" EXIT

    git subtree push --prefix "pkg/arch/${pkg}" aur master

    git remote rm aur
done
