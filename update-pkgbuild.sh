#!/usr/bin/env bash

if ! git remote | grep -q ^aur$ ; then
    git remote add aur ssh://aur@aur.archlinux.org/screencfg-git.git
fi

git subtree push --prefix pkg/arch/screencfg-git aur master

git remote rm aur
