#!/home/krikava/R/R-3.4.3/bin/Rscript

# the package to be run
# it has to be installed, ideally with:
# INSTALL_opts=c('--example', '--install-tests', '--with-keep.source', '--no-multiarch')
pkg <- commandArgs(trailingOnly=TRUE)[1]
# the package to be decorated
pkg_decorate <- "base"
# what to run (examples/tests/vignettes/all)
type <- "all"
type <- "tests"
# where to store the output
output_dir <- file.path(getwd(), pkg)
# be quiet
quiet <- TRUE
quiet <- FALSE

# this will kill the process if the package does not exist
invisible(find.package(pkg))
stopifnot(dir.exists(output_dir) || dir.create(output_dir))

options(error=function() {traceback(3); if(!interactive()) quit(status=1, save='no')})
options(genthat.debug=!quiet)

# from here everything will be slow
genthat::gen_from_package(pkg_decorate, pkg, types=type, quiet=quiet, action="export", output_dir=output_dir)
