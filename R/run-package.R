#' @title run package code
#' @description run package examples, tests and/or vignettes
#'
#'
#' @export
#'
# TODO: support commentDontrun, commentDonttest
run_package <- function(pkg, pkg_dir=find.package(pkg),
                       types=c("examples", "tests", "vignettes"),
                       working_dir=tempfile(pattern="run_package"),
                       quiet=TRUE,
                       runner=run_r_script) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(pkg_dir))
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))

    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)

    owd <- getwd()
    on.exit(setwd(owd), add=TRUE)

    # so the output list is named
    names(types) <- types

    lapply(types, function(type) {
        fun <- switch(
            type,
            examples=run_package_examples,
            tests=run_package_tests,
            vignettes=run_package_vignettes
        )

        if (!quiet) {
            message("Running `", pkg, "' package ", type, " (from: ", pkg_dir, ")")
        }

        tryCatch({
            cwd <- file.path(working_dir, type)
            stopifnot(dir.exists(cwd) || dir.create(cwd))
            setwd(cwd)

            fun(pkg, pkg_dir, working_dir=cwd, quiet=quiet, runner=runner)
        }, empty=function(e) {
            message("No", type, "for package", pkg)

            -2
        }, error=function(e) {
            message("Running failed: ", e$message)

            -127
        })
    })
}

#' @importFrom tools Rd_db Rd2ex
run_package_examples <- function(pkg, pkg_dir, working_dir, quiet, runner) {
    db <- tools::Rd_db(basename(pkg_dir), lib.loc=dirname(pkg_dir))
    if (!length(db)) {
        signal_empty()
    }

    files <- names(db)

    examples <- sapply(files, function(x) {
        f <- file.path(working_dir, paste0(basename(x), ".R"))
        tools::Rd2ex(db[[x]], f, defines=NULL)

        if (!file.exists(f)) {
            if (!quiet) {
                message("Rd file `", x, "' does not contain any code to be run")
            }
            NA
        } else {
            # prepend the file with library call
            txt <- c(
                paste0("library(", pkg, ")"),
                "",
                "",
                readLines(f)
            )
            writeLines(txt, f)
            f
        }
    })

    examples <- na.omit(examples)

    run_files(examples, quiet, runner)
}

run_package_tests <- function(pkg, pkg_dir, working_dir, quiet=quiet, runner) {
    test_dir <- file.path(pkg_dir, "tests")

    if (!dir.exists(test_dir)) {
        signal_empty()
    }

    files <- Sys.glob(file.path(test_dir, "*"))
    file.copy(files, working_dir, recursive=TRUE)

    tests <- file.path(working_dir, basename(files))
    tests <- tests[!dir.exists(tests)]

    run_files(tests, quiet, runner)
}

#' @importFrom tools checkVignettes getVignetteInfo
run_package_vignettes <- function(pkg, pkg_dir, working_dir, quiet=quiet, runner) {
    vinfo <- tools::pkgVignettes(pkg, source=T)
    if (length(vinfo$docs) == 0) {
        signal_empty()
    }

    if (length(vinfo$sources) == 0) {
        # so far no sources. The following should generate them if there are any
        # sources in the R code. It might actually run the vignettes as well.
        # That is a pity, but there is no way to tell it not to (the tangle is
        # needed to it extracts the R code)
        tools::checkVignettes(pkg, pkg_dir, tangle=TRUE, weave=FALSE, workdir="src")
    }

    # check if there are some sources
    vinfo <- tools::pkgVignettes(pkg, source=T)
    files <- as.character(vinfo$sources)
    if (length(files) == 0) {
        signal_empty()
    }

    file.copy(files, to=working_dir)
    vignettes <- file.path(working_dir, basename(files))

    run_files(vignettes, quiet, runner)
}

run_files <- function(files, quiet, runner) {
    names(files) <- basename(files)

    sapply(files, function(f) {
        if (!quiet) {
            message("Running `", f, "'")
        }

        tryCatch({
            status <- runner(f, quiet=quiet)

            if (!quiet) {
                message("File `", f, "' exited with ", status)
            }

            status
        }, error=function(e) {
            message("Unable to process or run: `", f, "': ", e$message)
            -1
        })
    })
}

local_runner <- function(file, quiet) {
    source(file, echo=!quiet)
    0
}

signal_empty <- function() {
    cond <- structure(list(message="", call=NULL), class=c("empty", "condition"))
    signalCondition(cond)
}
