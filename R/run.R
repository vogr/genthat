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

run_package_examples <- function(pkg, pkg_path, output_dir,
                                lib_path=NULL, quiet=TRUE, clean=TRUE, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    src_file <- file.path(getwd(), tools:::.createExdotR(pkg, pkg_path, silent=quiet, ...))
    if (!quiet) {
        message("  Running ", src_file)
    }

    run <- run_r_script(src_file, .lib_paths=lib_path, quiet=!is_debug_enabled(), clean=clean)
    list(status=run$status, output=run$output, elapsed=run$elapsed)
}

run_package_tests <- function(pkg, pkg_path, output_dir,
                             lib_path=NULL, quiet=TRUE, clean=TRUE, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    test_dir <- file.path(pkg_path, "tests")

    if (!dir.exists(test_dir)) {
        message("No tests found for package: ", pkg, " (from: ", pkg_path, ")")
        return(list(status=0, output=""))
    }

    file.copy(Sys.glob(file.path(test_dir, "*")), output_dir, recursive=TRUE)

    tests <- list.files(".", pattern = "\\.[rR]$", recursive=FALSE, full.names=TRUE)
    result <- lapply(tests, function(test) {
        if (!quiet) {
            message("  Running ", test)
        }

        run_r_script(test, .lib_paths=lib_path, quiet=!is_debug_enabled(), clean=clean)
    })

    status <- sapply(result, `[[`, "status")
    status <- if (any(status != 0)) 1 else 0
    output <- paste(sapply(result, `[[`, "output"), collapse="\n\n##TEST\n\n")
    elapsed <- sum(sapply(result, `[[`, "elapsed"))

    list(
        status=status,
        output=output,
        elapsed=elapsed
    )
}

run_package_vignettes <- function(pkg, pkg_path, output_dir,
                                 lib_path=NULL, quiet=TRUE, clean=TRUE, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    code <- substitute({
        options(error=function() { quit('no', status=1, runLast=TRUE) })
        tools::buildVignettes(package=PKG, dir=PKG_PATH, quiet=QUIET)
    }, list(
        PKG=pkg,
        PKG_PATH=pkg_path,
        QUIET=quiet
    ))

    run <- run_r_code(code,, quiet=!is_debug_enabled(), clean=clean)
    list(status=run$status, output=run$output, elapsed=run$elapsed)
}

#'  @param ... commentDontrun, commentDonttest
#'
#' @export
#'
run_package <- function(pkg, types=c("examples", "tests", "vignettes"),
                       lib_path=.libPaths(), quiet=TRUE, clean=TRUE, ...) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    pkg_path <- find.package(pkg, lib_path)

    ret <- list()
    for (type in types) {
        output_dir=tempfile()
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

        ret[[type]] <-
            tryCatch({
                fun(pkg, pkg_path, output_dir, lib_path, quiet, clean, ...)
            }, error=function(e) {
                message("Running failed: ", e$message)
                list(status=NA, output=e$message)
            }, finally={
                setwd(owd)
            })

        if (!clean) {
            ret[[type]]$output_dir <- output_dir
            if (!quiet) {
                message("Output stored in ", output_dir)
            }
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

run_r_script <- function(script_file, args=character(), .lib_paths=NULL, quiet=TRUE, clean=TRUE) {
    stopifnot(file.exists(script_file))
    stopifnot(is.null(.lib_paths) || all(dir.exists(.lib_paths)))

    out_file = tempfile()
    retval_file = tempfile()

    if (clean) {
        on.exit({
            if (file.exists(out_file)) file.remove(out_file)
            if (file.exists(retval_file)) file.remove(retval_file)
        })
    }

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
        paste("(", env, " ", Rscript, shQuote(script_file), "; echo $? >", shQuote(retval_file), ")",
            if (!quiet) "2>&1 | tee" else "2>&1 >",
            shQuote(out_file)
            , collapse=" ")

    if (is_debug_enabled()) {
        message("run_r_script: command: ", command)

        if (!clean) {
            message("run_r_script: script: ", script_file)
            message("run_r_script: output: ", out_file)
            message("run_r_script: retval: ", retval_file)
        }
    }

    time <- system.time(status <- system(command))

    if (status != 0) {
        stop("Unable to run: ", command, " exit: ", status)
    }

    list(
        command=command,
        script=read_text_file(script_file),
        status=as.numeric(read_text_file(retval_file)),
        output=read_text_file(out_file),
        elapsed=time["elapsed"]
    )
}
