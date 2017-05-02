#!/bin/sh

# Redirect output to stderr.
exec 1>&2

echo $PATH
# Regenerate the documentation
echo "Updating documentation"
Rscript -e "devtools::document()"
