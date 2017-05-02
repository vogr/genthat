#!/bin/sh

# Redirect output to stderr.
exec 1>&2

# Test
echo "Running tests"
Rscript -e "devtools::test()"
