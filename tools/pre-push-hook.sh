#!/bin/sh

# Redirect output to stderr.
exec 1>&2

# Test
echo "Running tests"
Rscript -e "options(genthat.run_integration_tests=TRUE); devtools::test()"
