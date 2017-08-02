#' @title run package code
#' @description run package examples, tests and/or vignettes
#' @param ... commentDontrun, commentDonttest
#'
#' @export
#'
run_package <- function(pkg, types=c("examples", "tests", "vignettes"),
                       fork=TRUE, quiet=TRUE, clean=TRUE,
                       lib_path=.libPaths(), ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    pkg_path <- find.package(pkg, lib_path)

    result <- list()

    for (type in types) {
        output_dir <- tempfile(pattern="run_package-", fileext=".out")
        stopifnot(dir.create(output_dir))

        if (clean) {
            on.exit(unlink(output_dir, recursive=TRUE), add=TRUE)
        }

        fun <-
            switch(type,
                examples=run_package_examples,
                tests=run_package_tests,
                vignettes=run_package_vignettes)

        if (!quiet) {
            message("Running `", pkg, "' package ", type, " (from: ", pkg_path, ")")
        }

        owd <- setwd(output_dir)
        on.exit(setwd(owd), add=TRUE)

        result[[type]] <-
            tryCatch({
                fun(pkg, pkg_path, output_dir=output_dir, fork=fork, quiet=quiet, clean=clean, lib_path=lib_path, ...)
            }, error=function(e) {
                message("Running failed: ", e$message)
                list(status=NA, output=e$message)
            })

        if (!clean) {
            result[[type]]$output_dir <- output_dir
            if (!quiet) {
                message("Output stored in ", output_dir)
            }
        }
    }

    result
}

run_package_examples <- function(pkg, pkg_path, output_dir,
                                fork=TRUE, quiet=TRUE, clean=TRUE,
                                lib_path=NULL, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    result <- list(
        status=-1,
        output=NA,
        elapsed=NA
    )

    src_file <- tools:::.createExdotR(pkg, pkg_path, silent=quiet, ...)
    if (is.null(src_file)) {
        result$output <- "No examples"

        return(result)
    }

    src_file <- file.path(getwd(), src_file)

    if (!quiet) {
        message("  Running ", src_file)
    }

    run <- run_r_script(src_file, .lib_paths=lib_path, quiet=!is_debug_enabled(), clean=clean)

    result$status <- run$status
    result$output <- run$output
    result$elapsed <- run$elapsed

    result
}

run_package_tests <- function(pkg, pkg_path, output_dir,
                             fork=TRUE, quiet=TRUE, clean=TRUE,
                             lib_path=NULL, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    result <- list(
        status=-1,
        output=NA,
        elapsed=NA
    )

    test_dir <- file.path(pkg_path, "tests")

    if (!dir.exists(test_dir)) {
        result$output <- "No tests"

        return(result)
    }

    file.copy(Sys.glob(file.path(test_dir, "*")), output_dir, recursive=TRUE)

    tests <- list.files(".", pattern = "\\.[rR]$", recursive=FALSE, full.names=TRUE)
    run <- lapply(tests, function(test) {
        if (!quiet) {
            message("  Running ", test)
        }

        run_r_script(test, .lib_paths=lib_path, quiet=!is_debug_enabled(), clean=clean)
    })

    result$status <- if (any(sapply(run, `[[`, "status") != 0)) 1 else 0
    result$output <- paste(sapply(run, `[[`, "output"), collapse="\n\n##TEST\n\n")
    result$elapsed <- sum(sapply(run, `[[`, "elapsed"))

    result
}

run_package_vignettes <- function(pkg, pkg_path, output_dir,
                                 fork=TRUE, quiet=TRUE, clean=TRUE,
                                 lib_path=NULL, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    result <- list(
        status=-1,
        output=NA,
        elapsed=NA
    )

    vignettes <- tools::getVignetteInfo(pkg, lib.loc=lib_path, all=TRUE)
    if (length(vignettes) == 0) {
        result$output <- "No vignettes"

        return(result)
    }


    code <- substitute({
        tools::buildVignettes(package=PKG, dir=PKG_PATH, quiet=QUIET)
    }, list(
        PKG=pkg,
        PKG_PATH=pkg_path,
        QUIET=quiet
    ))

    run <- run_r_code(code, quiet=!is_debug_enabled(), clean=clean)

    result$status <- run$status
    result$output <- run$output
    result$elapsed <- run$elapsed

    result
}

