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
# TODO: gen_from_package

#' @title Generate test cases for a package
#'
#' @description Decorates all functions in a package and then generates test cases based on
#' the code contained in the package examples, vignettes and tests.
#' @export
#'
gen_from_package <- function(package, output_dir="generated_tests",
                            types=c("tests", "vignettes", "examples"),
                            clean=FALSE, quiet=TRUE) {

    browser()
    ret <- trace_package(package, types, clean=clean, quiet=quiet)
    tests <- generate_tests(ret$traces, include_trace_dump=TRUE)

    ## res <- structure(
    ##     list(
    ##         traces=output$traces,
    ##         errors=filter(output$traces, is, class2="genthat_trace_entry"),
    ##         failures=filter(output$traces, is, class2="genthat_trace_error"),
    ##         tests=tests
    ##     ),
    ##     class="genthat_result"
    ## )

    ## if (is_debug_enabled()) {
    ##     res$output <- output_dir
    ##     res$genthat_output <- genthat_output
    ##     res$libs <- libs
    ##     res$pkg_dir <- pkg_dir
    ## }

    ## res
}

# TODO: vectorize over package
#' @export
#'
trace_package <- function(pkg_path, code_to_run, clean=TRUE, quiet=TRUE,
                         .tmp_lib=tempfile("R_genthat_")) {

    stopifnot(dir.exists(.tmp_lib) || dir.create(.tmp_lib))
    if (is_debug_enabled() && !clean) {
        message("Working in ", .tmp_lib)
    }

    # TODO: no a very good heuristics (read manually using read.dcf)
    pkg_name <-
        if (file.exists(pkg_path) && endsWith(pkg_path, ".tar.gz")) {
            basename(untar(pkg_path, list=TRUE)[1])
        } else if (dir.exists(pkg_path)) {
            basename(pkg_path)
        } else {
            # try if it is an installed package
            tryCatch({
                name <- pkg_path
                pkg_path <- find.package(name)
                name
            }, error=function(e) {
                stop("Unsupported / non-existing pkg_path")
            })
        }

    # where the pkg_path will be installed
    tmp_pkg_path <- file.path(.tmp_lib, pkg_name)

    if (clean) {
        on.exit(unlink(.tmp_lib, recursive=TRUE), add=TRUE)
    }

    # install the target pkg_path
    if (is_debug_enabled()) {
        message("Installing ", pkg_path, " into: ", tmp_pkg_path)
    }

    tryCatch({
        # locally install the package from its location
        utils::install.packages(
            pkgs=pkg_path,
            repos=NULL,
            lib=.tmp_lib,
            type="source",
            dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"),
            INSTALL_opts=c(
                "--example",
                "--install-tests",
                "--with-keep.source",
                "--no-multiarch"),
            quiet=quiet)
    }, warning=function(e) {
        stop("Installation of ", pkg_path, " failed with: ", e$message)
    }, error=function(e) {
        stop("Installation of ", pkg_path, " failed with: ", e$message)
    })

    # install genthat
    # the dependencies should be already included since this is only called
    # from an existing genthat installation
    genthat_path <- find.package("genthat")
    if (is_debug_enabled()) {
        message("Installing genthat from: ", genthat_path, " into: ", .tmp_lib)
    }
    tryCatch({
        utils::install.packages(
            pkgs=genthat_path,
            repos=NULL,
            lib=.tmp_lib,
            type="source",
            quiet=quiet)
    }, warning=function(e) {
        stop("Installation of ", pkg_path, " failed with: ", e$message)
    }, error=function(e) {
        stop("Installation of ", pkg_path, " failed with: ", e$message)
    })

    libs <- env_path(.tmp_lib, .libPaths())
    genthat_output <- file.path(tmp_pkg_path, "genthat.RDS")

    # TODO: always use a new file - save to directory
    add_package_hook(
        pkg_name,
        .tmp_lib,
        on_load=substitute({
            message("Invoking genthat pkg_path loading hook")
            options(genthat.debug=GENTHAT_DEBUG)

            time <-
                system.time(genthat::decorate_environment(ns))

            message("Decorated ", length(genthat::get_replacements()), " in ", time["elapsed"])
        }, list(GENTHAT_DEBUG=getOption("genthat.debug", default=FALSE))),
        on_gc_finalizer=substitute({
            message("Invoking genthat pkg_path unloading hook")

            time <-
                system.time(
                    saveRDS(
                        list(
                            replacements=sapply(genthat::get_replacements(), `[[`, "name", USE.NAMES=FALSE),
                            traces=genthat::copy_call_traces()
                        ),
                        GENTHAT_OUTPUT
                    )
                )

            message("Saved replacements in ", time["elapsed"])
        }, list(GENTHAT_OUTPUT=genthat_output))
    )

    code <- substitute(code_to_run)
    if (typeof(code) == "language") {
        run <- run_r_code(code, save_image=TRUE, .lib_paths=.tmp_lib)
    } else {
        run <- run_r_code(code_to_run, save_image=TRUE, .lib_paths=.tmp_lib)
    }

    if (file.exists(genthat_output)) {
        output <- readRDS(genthat_output)
        file.remove(genthat_output)
    } else {
        output <- list(traces=list(), replacements=list())
    }

    result <- list(
        traces=output$traces,
        replacements=output$replacements,
        result=run
    )

    attr(result, "class") <- "genthat_traces"

    result
}

#' @export
#'
as.data.frame.genthat_traces <- function (x, row.names = NULL, optional = FALSE, ...) {
    x$replacements <- length(x$replacements)
    x$traces <- length(x$traces)
    x$result <- NULL
    c(x)
}

#' @export
#'
format.genthat_traces <- function(x, ...) {
    format(as.data.frame(x))
}

#' @export
#'
print.genthat_traces <- function(x, ...) {
    print(as.data.frame(x))
}

#' @export
#'
enable_tracing <- function() {
    cache$tracing <- TRUE
}

#' @export
#'
disable_tracing <- function() {
    cache$tracing <- FALSE
}

#' @export
#'
is_tracing_enabled <- function() {
    cache$tracing
}
