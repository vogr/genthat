#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(repos="https://cloud.r-project.org/")

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
    stop("Usage: <--local|--all> <path to packages directory>")
}

available_pkgs <-
    if (args[1] == "--all") {
        contrib_url <- utils::contrib.url(getOption("repos"), "source")
        available.packages(contrib_url)
    } else if (args[1] == "--local") {
        installed.packages()
    }

available_pkgs <- available_pkgs[, 1]

dest <- args[2]
stopifnot(dir.exists(dest) || dir.create(dest))

# TODO: replace with library(genthat)
devtools::load_all()

N <- length(available_pkgs)
downloaded <- 0
curr <- 0

for (pkg in available_pkgs) {
    curr <- curr + 1

    tryCatch({
        time <- as.numeric(Sys.time())
        genthat::download_package(pkg, dest, extract=FALSE)
        time <- as.numeric(Sys.time()) - time

        downloaded <- downloaded + 1
        message("[", curr, "/", downloaded, "/", N, "] Downloaded: ", pkg, " in ", time)
    }, error=function(e) {
        message("[", curr, "/", downloaded, "/", N, "] Failed: ", pkg, " - ", e$message)
    })
}
