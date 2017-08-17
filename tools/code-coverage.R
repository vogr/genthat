#!/usr/bin/env Rscript

input <- commandArgs(trailingOnly=TRUE)[1]
output <- commandArgs(trailingOnly=TRUE)[2]

pkg <- devtools::as.package(input)
time <- system.time(coverage <- covr::package_coverage(input, type="all"))

ret <- list(pkg=pkg, time=time, coverage=coverage)

saveRDS(ret, file.path(output, paste0(pkg$package, ".RDS")))
