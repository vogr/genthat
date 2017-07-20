#' status code:
#'  0: OK
#' -1: Test could not be run - i.e. a syntax error
#'  1: Test failed
#'  2: Test threw an exception
#'  3: Test did not contain any tests (nb field in the testthat_result was 0)
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils getTxtProgressBar
#' @export
#'
run_generated_tests <- function(tests, quiet=TRUE, show_progress=isTRUE(getOption("genthat.show_progress"))) {
    stopifnot(is.data.frame(tests))

    if (quiet && show_progress) {
        pb <- utils::txtProgressBar(min=0, max=nrow(tests), initial=0, style=3)

        after_one_run <- function() utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1)
        after_all_runs <- function() close(pb)
    } else {
        after_one_run <- function() {}
        after_all_runs <- function() {}
    }

    runs <- apply(tests, 1, function(x) {
        x <- as.list(x)

        result <- list(
            trace_id=x$trace_id,
            trace=x$trace,
            fun=x$fun,
            code=x$code,
            test=NA,
            status=NA,
            error=NA,
            stdout=NA,
            stderr=NA,
            elapsed=NA
        )

        tryCatch({
            if (!is.na(x$code)) {
                if (!quiet) {
                    message("Running test from trace: ", x$trace_id, " (", nchar(x$code, type="bytes"), " bytes)")
                }

                run <- run_generated_test(x$code, quiet)
                result$test <- run$test
                result$status <- run$status
                result$error <- run$error
                result$stdout <- run$stdout
                result$stderr <- run$stderr
                result$elapsed <- run$elapsed
            }
        }, error=function(e) {
            msg <- e$message
            message("Unable to run tests from trace: ", x$trace_id, " - ", msg)
            result$error <- msg
            result$status <- -1
        })

        after_one_run()
        as.data.frame(result, stringsAsFactors=FALSE)
    })

    after_all_runs()

    if (requireNamespace("dplyr", quietly=TRUE)) {
        dplyr::as_data_frame(dplyr::bind_rows(runs))
    } else {
        message("dplyr is not available, which is a pity since it will speed up things")
        do.call(rbind, runs)
    }
}

run_generated_test <- function(code, quiet=TRUE) {
    stopifnot(is.character(code) && length(code) == 1)

    tmp_test_file <- tempfile()
    on.exit(file.remove(tmp_test_file))
    write(code, file=tmp_test_file)

    run <- capture(test_result <- testthat::test_file(tmp_test_file))

    result <- list(
        test=NA,
        status=NA,
        error=NA,
        stdout=run$stdout,
        stderr=run$stderr,
        elapsed=run$elapsed
    )

    if (!is.null(test_result) && is.null(run$error)) {
        test_result <- as.data.frame(test_result)

        result$test <- test_result$test
        result$status <-
            if (test_result$failed) {
                if (!quiet) {
                    message("Test failed")
                }

                1
            } else if (test_result$error) {
                if (!quiet) {
                    message("Test threw an error")
                }

                2
            } else if (test_result$nb == 0) {
                if (!quiet) {
                    message("Test did not run any tests")
                }

                3
            } else {
                if (!quiet) {
                    message("Test succeeded in ", run$elapsed)
                }

                0
            }
    } else {
        msg <- result$error$message
        if (is_debug_enabled()) {
            message("Test for function `", x, "` could not be run: ", msg)
        }

        result$error <- msg
    }

    result
}

run_package_ <- function(pkg, type, output_dir, lib_path=NULL, quiet=TRUE, ...) {
    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))
    type <- match.arg(type, c("examples", "tests"), several.ok=FALSE)

    pkg_path <- find.package(pkg, lib.loc=lib_path)

    if (!quiet) {
        message("Running ", pkg, " package ", type, " (from: ", pkg_path, ") in ", output_dir)
    }

    status <-
        tools::testInstalledPackage(
            pkg,
            outDir=output_dir,
            errorsAreFatal=FALSE,
            lib.loc=lib_path,
            types=type,
            ...
        )

    pattern <- if (status == 0) "Rout$" else "fail$"
    output_files <- list.files(output_dir, pattern=pattern, recursive=TRUE, full.names=TRUE)
    output <- paste(sapply(output_files, read_text_file), collapse="\n\n")

    list(
        status=status,
        output=output
    )
}

# inspired by run_vignettes form covr
# has to be handled separately since the testInstalledPackage does not work well with packages
run_package_vignettes <- function(pkg, output_dir, lib_path=NULL, quiet=TRUE, ...) {
    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    pkg_path <- find.package(pkg, lib.loc=lib_path)

    if (!quiet) {
        message("Running ", pkg, " package vignettes (from: ", pkg_path, ") in ", output_dir)
    }

    code <- substitute({
        options(error=function() { quit('no', status=1, runLast=TRUE) })
        tools::buildVignettes(package=PKG, dir=PKG_PATH, quiet=QUIET)
    }, list(
        PKG=pkg,
        PKG_PATH=pkg_path,
        QUIET=quiet
    ))

    run <- run_r_code(code)
    run
    ## str(run)

    ## list(
    ##     status=run$status,
    ##     output=run$out
    ## )
}

#'  @param ... commentDontrun, commentDonttest
#'
#' @export
#'
run_package <- function(pkg, types=c("examples", "tests", "vignettes"),
                       clean=TRUE, lib_path=.libPaths(), quiet=TRUE, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    pkg_path <- find.package(pkg, lib_path)

    ret <- list()
    for (type in types) {
        output_dir=tempfile()
        stopifnot(dir.create(output_dir))

        if (clean) {
            on.exit(unlink(output_dir, recursive=TRUE))
        }

        if (type == "vignettes") {
            # this is because testInstalledPackage fails running vignettes
            ret[[type]] <- run_package_vignettes(pkg, output_dir, lib_path, quiet, ...)
        } else {
            ret[[type]] <- run_package_(pkg, type, output_dir, lib_path, quiet, ...)
        }

        if (!clean) {
            ret[[type]]$output_dir <- output_dir
        }
    }

    ret
}

run_r_code <- function(code_to_run, save_image=FALSE, ...) {
    # TODO: warn if there are any free variables

    code <- substitute(code_to_run)
    script <-
        if (typeof(code) == "language") {
            deparse(code)
        } else {
            deparse(code_to_run)
        }

    if (save_image) {
        image_file <- tempfile()
        on.exit(file.remove(image_file), add=TRUE)
        script <- c(script, deparse(substitute(save.image(file=FILE), list(FILE=image_file))))
    }

    script_file <- tempfile()
    writeLines(script, script_file)
    on.exit(file.remove(script_file))

    run <- run_r_script(script_file, ...)

    if (run$status == 0 && save_image) {
        e <- new.env(parent=emptyenv())
        load(file=image_file, envir=e)
        run$image <- e
    }

    run
}

run_r_script <- function(script_file, args=character(), .lib_paths=NULL, split=TRUE) {
    stopifnot(file.exists(script_file))
    stopifnot(is.null(.lib_paths) || all(dir.exists(.lib_paths)))

    out_file = tempfile()
    run_file = tempfile()

    on.exit({
        if (file.exists(out_file)) file.remove(out_file)
        if (file.exists(run_file)) file.remove(run_file)
    })

    env <-
        if (is.null(.lib_paths)) {
            character()
        } else {
            # This is fairly counter intuitive, but
            # the problem is that in some cases some R
            # code (e.g. tools::testInstalledPackage)
            # sets R_LIBS to "" and therefore the .lib_paths
            # won't work. Therefore reset the variables
            # to be the same. This might break other things I guess
            # but so far so good.
            paths <- paste(shQuote(c(.lib_paths, .libPaths())), collapse=.Platform$path.sep)
            c(
                paste("R_LIBS", paths, sep="=")
                ## paste("R_LIBS_USER", paths, sep="="),
                ## paste("R_LIBS_SITE", paths, sep="=")
            )
        }

    env <- paste(env, collapse=" ")

    Rscript <- file.path(R.home("bin"), "Rscript")
    command <-
        paste("(", env, " ", Rscript, shQuote(script_file), "; echo $? >", shQuote(run_file), ")",
            if (split) "2>&1 | tee" else "&>",
            shQuote(out_file)
            , collapse=" ")

    if (is_debug_enabled()) {
        message("run_r_script: running: ", command)
    }
    status <- system(command)

    if (status != 0) {
        stop("Unable to run: ", command, " exit: ", status)
    }

    list(
        command=command,
        script=read_text_file(script_file),
        status=as.numeric(read_text_file(run_file)),
        output=read_text_file(out_file)
    )
}
