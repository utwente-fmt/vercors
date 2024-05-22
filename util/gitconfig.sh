#!/bin/bash

# IMPORTANT:
# Git hooks can contain arbitrary code.
# They are run when you run certain git commands, such as commit, checkout, etc.
# Make sure you trust the hook scripts before you enable them.

git config --local core.hooksPath util/githooks
git config --local blame.ignoreRevsFile .git-blame-ignore-revs
