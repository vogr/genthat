#' genthat: A framework for unit tests generation
#'
#' @docType package
#' @name genthat
#' @useDynLib genthat
#' @importFrom Rcpp evalCpp
NULL

# TODO: sync API with covr?
# TODO: gen_from_function
# TODO: gen_from_code
# TODO: gen_from_source

#' @title Generate test cases for a package
#'
#' @description Decorates all functions in a package and then generates test cases based on
#' the code contained in the package examples, vignettes and tests.
#' @export
#'
gen_from_package <- function(package=".", output_dir="generated_tests",
                            type=c("tests", "vignettes", "examples"),
                            clean=FALSE, quiet=FALSE) {

    # TODO: can we remove this dependency
    pkg <- devtools::as.package(package)

    if (missing(type)) {
        type <- "tests"
    }

    tmp_lib <- tempfile("R_LIBS")
    stopifnot(dir.create(tmp_lib))

    if (isTRUE(clean)) {
        on.exit({
            clean_objects(pkg$path)
        }, add=TRUE)
    }

    utils::install.packages(
               repos=NULL,
               lib=tmp_lib,
               pkg$path,
               type="source",
               dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"),
               INSTALL_opts=c("--example", # TODO: only if we need it
                   "--install-tests", # TODO: only if we need it
                   "--with-keep.source",
                   "--no-multiarch"),
               quiet=quiet)

    libs <- env_path(tmp_lib, .libPaths())
    pkg_dir <- file.path(tmp_lib, pkg$package)
    genthat_output <- file.path(pkg_dir, "genthat.RDS")

    if (is_debug_enabled()) {
        cat("Working dir:", pkg_dir)
    }

    add_package_hook(
        pkg$package,
        tmp_lib,
        on_load=substitute({
            message("Invoking genthat package loading hook")
            install.packages(repos=NULL, type="source", pkgs=GENTHAT_PATH)
            genthat::decorate_environment(ns)
        }, list(GENTHAT_PATH=find.package("genthat"))),
        on_gc_finalizer=substitute({
            genthat::export_traces(GENTHAT_OUTPUT)
        }, list(GENTHAT_OUTPUT=genthat_output))
    )

    # TODO: debug trace
    #cat(paste0(readLines(file.path(pkg_dir,"R","stringr")), collapse="\n"))

    withr::with_envvar(c(R_LIBS=libs, R_LIBS_USER=libs, R_LIBS_SITE=libs), {
        ## if ("vignettes" %in% type) {
        ##     type <- type[type != "vignettes"]
        ##     run_vignettes(pkg, tmp_lib)
        ## }

        ## if ("examples" %in% type) {
        ##     type <- type[type != "examples"]
        ##     # testInstalledPackage explicitly sets R_LIBS="" on windows, and does
        ##     # not restore it after, so we need to reset it ourselves.
        ##     withr::with_envvar(c(R_LIBS = Sys.getenv("R_LIBS")), {
        ##         result <- tools::testInstalledPackage(pkg$package, outDir = pkg_dir, types = "examples", lib.loc = tmp_lib) # TODO: add ...
        ##         if (result != 0L) {
        ##             show_failures(pkg_dir)
        ##         }
        ##     })
        ## }

        if ("tests" %in% type) {
            result <- tools::testInstalledPackage(pkg$package, outDir=pkg_dir, types="tests", lib.loc=tmp_lib) # TODO: add ...

            if (!quiet) {
                out <- file.path(pkg_dir, paste0(pkg$package, "-tests"), "testthat.Rout")
                if (file.exists(out)) cat(paste0(readLines(out), collapse="\n"))
            }

            if (result != 0L) {
                show_failures(pkg_dir)
            }
        }
    })

    if (!file.exists(genthat_output)) {
        stop("genthat output does not exist ", genthat_output)
    }

    output <- readRDS(genthat_output)

    if (is_debug_enabled()) {
        cat("Genthat RDS output: ", genthat_output, "\n")
    }

    stopifnot(is.environment(output))

    tests <- generate_tests(output$traces, output_dir)

    res <- structure(
        list(
            traces=output$traces,
            errors=filter(output$traces, is, class2="genthat_trace_entry"),
            failures=filter(output$traces, is, class2="genthat_trace_error"),
            tests=tests
        ),
        class="genthat_result"
    )

    if (is_debug_enabled()) {
       res$genthat_output <- genthat_output
       res$libs <- libs
       res$pkg_dir <- pkg_dir
    }

    res
}

#' @title Exports recorded traces into RDS file
#'
#' @description Exports recorded traces into given file in RDS format using `saveRDS`.
#' @export
#'
export_traces <- function(file) {
    saveRDS(cache, file)
}

summarizes_genthat_results <- function(x) {
    x$traces <- length(x$traces)
    x$errors <- length(x$errors)
    x$failures <- length(x$failures)
    x$tests <- length(x$tests)

    c(x)
}

#' @export
format.genthat_result <- function(x, ...) {
    format(summarizes_genthat_results(x))
}

#' @export
print.genthat_result <- function(x, ...) {
    print(summarizes_genthat_results(x))
}
