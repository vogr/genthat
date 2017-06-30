#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils getTxtProgressBar
#' @export
#'
run_generated_tests <- function(tests, show_progress=isTRUE(getOption("genthat.show_progress"))) {
    stopifnot(is.data.frame(tests))

    if (show_progress) {
        pb <- utils::txtProgressBar(min=0, max=nrow(tests), initial=0, style=3)

        after_one_run <- function() utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb)+1)
        after_all_runs <- function() close(pb)
    } else {
        after_one_run <- function() {}
        after_all_runs <- function() {}
    }

    missing <- which(is.na(tests$code))
    if (length(missing)) {
        stop("Missing code for traces #", paste(missing, collapse=","))
    }

    runs <- apply(tests, 1, function(x) {
        x <- as.list(x)

        # run the test
        tmp_test_file <- tempfile()
        write(x$code, file=tmp_test_file)
        test_result <- capture(testthat::test_file(tmp_test_file))
        after_one_run()

        result <- list(
            fun=x$fun,
            trace=x$trace,
            code=x$code,
            out=test_result$out,
            err=test_result$err,
            time=test_result$time,
            test=NA,
            result=NA,
            error=NA
        )

        if (is.null(test_result$error)) {
            if (is_debug_enabled()) {
                message("Test for function `", x$fun, "` succeeded")
            }

            test_run <- as.data.frame(test_result$result)

            result$test <- test_run$test
            result$result <-
                if (test_run$failed) {
                    2
                } else if (test_run$error) {
                    3
                } else if (test_run$nb == 0) {
                    4
                } else {
                    1
                }

        } else {
            msg <- test_result$error$message
            if (is_debug_enabled()) {
                message("Test for function `", x, "` failed: ", msg)
            }

            result$error <- msg
        }

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

run_package_examples <- function(pkg, output_dir, lib_path=NULL, ...) {
    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    if (is_debug_enabled()) {
        message("Running examples for: ", pkg, " (", find.package(pkg, lib.loc=lib_path), ") output: ", output_dir)
    }

    res <-
        tools::testInstalledPackage(
            pkg,
            outDir=output_dir,
            errorsAreFatal=FALSE,
            lib.loc=lib_path,
            types="examples",
            ...
        )

    output_files <- list.files(output_dir, pattern="Rout$", recursive=FALSE, full.names=TRUE)
    output <- sapply(output_files, read_text_file)

    fail_files <- list.files(output_dir, pattern="fail$", recursive=FALSE, full.names=TRUE)
    fail <- sapply(fail_files, read_text_file)

    list(
        success=res == 0,
        output=output,
        fail=fail
    )
}

run_package_tests <- function(pkg, output_dir, lib_path=NULL, ...) {
    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    if (is_debug_enabled()) {
        message("Running tests for: ", pkg, " (", find.package(pkg, lib.loc=lib_path), ") output: ", output_dir)
    }

    res <-
        tools::testInstalledPackage(
            pkg,
            outDir=output_dir,
            errorsAreFatal=FALSE,
            lib.loc=lib_path,
            types="tests",
            ...
        )

    output_files <- list.files(output_dir, pattern="Rout$", recursive=TRUE, full.names=TRUE)
    output <- sapply(output_files, read_text_file)

    fail_files <- list.files(output_dir, pattern="fail$", recursive=TRUE, full.names=TRUE)
    fail <- sapply(fail_files, read_text_file)

    list(
        success=res == 0,
        output=output,
        fail=fail
    )
}

# inspired by run_vignettes form covr
run_package_vignettes <- function(pkg, output_dir, lib_path=NULL, ...) {
    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(dir.exists(output_dir))

    pkg_path <- find.package(pkg, lib.loc=lib_path)

    if (is_debug_enabled()) {
        message("Running examples for: ", pkg, " (", pkg_path, ") output: ", output_dir)
    }

    out_file <- file.path(output_dir, paste0(pkg, "-Vignette.Rout"))
    fail_file <- paste(out_file, "fail", sep = "." )

    cat("tools::buildVignettes(dir='", pkg_path, "', quiet=FALSE)\n", file=out_file, sep="")
    # TODO: move to run_r_code
    cmd <- paste(
        shQuote(file.path(R.home("bin"), "R")),
        "CMD BATCH --vanilla --no-timing",
        shQuote(out_file),
        shQuote(fail_file)
    )

    res <- system(cmd)
    if (res != 0) {
        fail <- read_text_file(fail_file)
        output <- NULL
    } else {
        file.rename(fail_file, out_file)

        fail <- NULL
        output <- read_text_file(out_file)
    }

    list(
        success=res == 0,
        output=output,
        fail=fail
    )
}

#'  @param ... commentDontrun, commentDonttest
#'
#' @export
#'
run_package <- function(pkg, types=c("examples", "tests", "vignettes"), clean=TRUE, lib_path=.libPaths(), ...) {
    stopifnot(length(pkg) == 1)
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    pkg_path <- find.package(pkg, lib_path)

    ret <- list()
    for (type in types) {
        output_dir=tempfile()
        stopifnot(dir.create(output_dir))

        if (clean) {
            on.exit(unlink(output_dir, recursive=TRUE))
        }

        if (is_debug_enabled()) {
            message("Running package in ", pkg, "(from: ", pkg_path, ") in ", output_dir)
        }

        fce <- switch(
            type,
            examples=run_package_examples,
            tests=run_package_tests,
            vignettes=run_package_vignettes
        )

        ret[[type]] <- fce(pkg, output_dir, lib_path, ...)

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
        script <- c(script, deparse(substitute(save.image(file=FILE), list(FILE=image_file))))
    }

    script_file <- tempfile()

    on.exit(file.remove(script_file))

    writeLines(script, script_file)

    ret <- run_r_script(script_file, ...)

    if (save_image) {
        e <- new.env(parent=emptyenv())
        load(file=image_file, envir=e)
        ret$image <- e
    }

    ret
}

run_r_script <- function(script_file, args=character(), .lib_paths=NULL) {
    stopifnot(file.exists(script_file))
    stopifnot(is.null(.lib_paths) || all(dir.exists(.lib_paths)))

    out_file = tempfile()

    on.exit(if (file.exists(out_file)) file.remove(out_file))

    Rcmd <- file.path(R.home("bin"), "R")
    args <- c(
        "CMD",
        "BATCH",
        "--slave",
        "--vanilla",
        "--no-timing",
        shQuote(args),
        shQuote(script_file),
        shQuote(out_file))

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
            paths <- paste(c(.lib_paths,.libPaths()), collapse=.Platform$path.sep)
            c(
                paste("R_LIBS", paths, sep="="),
                paste("R_LIBS_USER", paths, sep="="),
                paste("R_LIBS_SITE", paths, sep="=")
            )
        }

    tryCatch({
        res <- system2(Rcmd, args, wait=TRUE, env=env)
    }, finally={
        out <- read_text_file(out_file)
    })

    list(
        command=paste(c(Rcmd, args), collapse=" "),
        script=read_text_file(script_file),
        status=res,
        out=out
    )
}
