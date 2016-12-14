
catUnderlined.TOP <- 1
catUnderlined.BOTTOM <- 2
catUnderlined.BOTH <- 3

catUnderlined <- function(..., side = catUnderlined.BOTTOM) {
    omsg <- paste0(..., "\n")
    # over
    if (bitwAnd(1, side)) message(gsub(".", "-", omsg))
    message("\n")
    # message
    message(omsg)
    # under
    if (bitwAnd(2, side)) message(gsub(".", "-", omsg))
    message("\n")
}

with.wd <- function(wd, fn) {
    old.wd <- getwd()
    setwd(wd)
    fn()
    setwd(old.wd)
}


run_R_tests <- function(package.dir, verbose) {
    test.dir <- file.path(package.dir, "tests")
    test.files <- list.files(path = test.dir, full.names = TRUE, pattern = "\\.R$", recursive = TRUE, no.. = TRUE)
    package.name <- devtools::as.package(package.dir)$package

    success <- TRUE
    for (file in test.files) {
        with.wd(dirname(file), function() {
            if (verbose) catUnderlined("\nRunning testfile: ", basename(file), " in ", getwd())
            env <- new.env(parent = getNamespace(package.name))
            test.failed <- FALSE
            tryCatch(source(basename(file), local = env), error = function(e) {
                if (verbose) message(e$message)
                test.failed <<- TRUE
            })
            if (test.failed) {
                success <<- FALSE
                if (verbose) catUnderlined("TEST RESULT: [FAIL]", side = catUnderlined.TOP)
            } else {
                if (verbose) catUnderlined("TEST RESULT: [OK]", side = catUnderlined.BOTH)
            }
        })
    }

    return(success)
}
