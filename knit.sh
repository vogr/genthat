#!/bin/sh

[ $# -eq 1 ] || exit 1

Rscript -e "rmarkdown::render('$1')"
