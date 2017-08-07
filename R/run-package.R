#' @title run package code
#' @description run package examples, tests and/or vignettes
#' @param ... commentDontrun, commentDonttest
#'
#' @export
#'
run_package <- function(pkg, types=c("examples", "tests", "vignettes"),
                       output_dir=tempfile(pattern="run_package-output-dir"),
                       fork=TRUE, quiet=TRUE,
                       lib_path=.libPaths(), ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    pkg_path <- find.package(pkg, lib_path)

    result <- list()

    for (type in types) {
        stopifnot(dir.exists(output_dir) || dir.create(output_dir))

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
                fun(pkg, pkg_path, output_dir=output_dir, fork=fork, quiet=quiet, lib_path=lib_path, ...)
            }, empty=function(e) {
                msg <- paste("No", type, "for package", pkg)
                if (!quiet) {
                    message(msg)
                }

                list(status=-1, output=msg, elapsed=NA)
            }, error=function(e) {
                msg <- paste("Running failed: ", e$message)
                if (!quiet) {
                    message(msg)
                }

                list(status=-2, output=e$message, elapsed=NA)
            })


        result[[type]]$output_dir <- output_dir

        output_file <- file.path(output_dir, paste0(type, ".out"))
        write(x=result[[type]]$output, file=output_file)

        if (!quiet) {
            message("Output stored in ", output_dir)
        }

    }

    result
}

run_package_examples <- function(pkg, pkg_path, output_dir,
                                fork=TRUE, quiet=TRUE,
                                lib_path=NULL, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    src_file <- tools:::.createExdotR(pkg, pkg_path, silent=quiet, ...)
    if (is.null(src_file)) {
        signal_empty()
    }

    src_file <- file.path(getwd(), src_file)

    if (!quiet) {
        message("  Running ", src_file)
    }

    run <- run_r_script(src_file, .lib_paths=lib_path, quiet=!is_debug_enabled())

    list(
        status=run$status,
        output=run$output,
        elapsed=run$elapsed
    )
}

run_package_tests <- function(pkg, pkg_path, output_dir,
                             fork=TRUE, quiet=TRUE,
                             lib_path=NULL, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    test_dir <- file.path(pkg_path, "tests")

    if (!dir.exists(test_dir)) {
        signal_empty()
    }

    file.copy(Sys.glob(file.path(test_dir, "*")), output_dir, recursive=TRUE)

    tests <- list.files(".", pattern = "\\.[rR]$", recursive=FALSE, full.names=TRUE)
    run <- lapply(tests, function(test) {
        if (!quiet) {
            message("  Running ", test)
        }

        run_r_script(test, .lib_paths=lib_path, quiet=!is_debug_enabled())
    })

    status <- if (any(sapply(run, `[[`, "status") != 0)) 1 else 0
    output <- paste(sapply(run, `[[`, "output"), collapse="\n\n##TEST\n\n")
    elapsed <- sum(sapply(run, `[[`, "elapsed"))

    list(
        status=status,
        output=output,
        elapsed=elapsed
    )
}

run_package_vignettes <- function(pkg, pkg_path, output_dir,
                                 fork=TRUE, quiet=TRUE,
                                 lib_path=NULL, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    vignettes <- tools::getVignetteInfo(pkg, lib.loc=lib_path, all=TRUE)
    if (length(vignettes) == 0) {
        signal_empty()
    }

    code <- substitute({
        tools::buildVignettes(package=PKG, dir=PKG_PATH, quiet=QUIET, tangle=TRUE)
    }, list(
        PKG=pkg,
        PKG_PATH=pkg_path,
        QUIET=quiet
    ))

    run <- run_r_code(code, quiet=!is_debug_enabled(), .lib_path=lib_path)

    list(
        status=run$status,
        output=run$output,
        elapsed=run$elapsed
    )
}

signal_empty <- function() {
    cond <- structure(list(message="", call=NULL), class=c("empty", "condition"))
    signalCondition(cond)
}
