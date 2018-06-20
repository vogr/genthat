#!/bin/sh
set -e

echo "* Checking whitespaces..."
git diff --check

echo "* Checking for calls to 'browser()'..."
if grep --color -nr "browser(" R tests; then
    exit 1
else
    exit 0
fi
