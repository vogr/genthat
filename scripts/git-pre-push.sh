#!/bin/sh
set -e

remote="$1"
url="$2"

echo "* Running tests..."
make test
