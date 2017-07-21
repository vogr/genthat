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

    ## browser()
    ## ret <- trace_package(package, types, clean=clean, quiet=quiet)
    ## tests <- generate_tests(ret$traces, include_trace_dump=TRUE)

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
trace_package <- function(package, code_to_run,
                         output_dir=".",
                         batch_size=0,
                         clean=TRUE, quiet=TRUE,
                         .tmp_lib=tempfile("R_genthat_")) {

    output_dir <- normalizePath(output_dir, mustWork=TRUE)

    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(dir.exists(.tmp_lib) || dir.create(.tmp_lib))
    if (!quiet && !clean) {
        message("Working in ", .tmp_lib)
    }

    # is it an installed package or external package file (extracted or zip)?
    pkg_name <- resolve_package_name(package)
    pkg_path <- if (file.exists(package)) package else find.package(pkg_name)

    # where the pkg_path will be installed
    tmp_pkg_path <- file.path(.tmp_lib, pkg_name)

    if (clean) {
        on.exit(unlink(.tmp_lib, recursive=TRUE), add=TRUE)
    }

    # install the target pkg_path
    if (!quiet) {
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
    if (!quiet) {
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

            saveRDS(list(decorating_time=time["elapsed"]), GENTHAT_OUTPUT)

            message("Decorated ", length(genthat::get_replacements()), " in ", time["elapsed"])
        }, list(
            GENTHAT_OUTPUT=genthat_output,
            GENTHAT_DEBUG=getOption("genthat.debug", default=TRUE))),

        # TODO: capture error
        on_gc_finalizer=substitute({
            message("Invoking genthat unloading hook")

            time <-
                system.time({
                    traces <- genthat::copy_call_traces()
                    n_traces <- length(traces)
                    message("Saving ", n_traces, " into ", OUTPUT_DIR)

                    trace_files <-
                        genthat:::export_traces(
                            traces,
                            prefix=PKG,
                            output_dir=OUTPUT_DIR,
                            batch_size=BATCH_SIZE
                        )
                })

            message("Saved in ", time["elapsed"])

            output <- readRDS(GENTHAT_OUTPUT)

            output$n_traces <- n_traces
            output$saving_time <- time["elapsed"]
            output$trace_files <- trace_files
            output$traced_functions <- sapply(genthat::get_replacements(), `[[`, "name", USE.NAMES=FALSE)

            saveRDS(output, GENTHAT_OUTPUT)
        }, list(
            PKG=pkg_name,
            OUTPUT_DIR=output_dir,
            BATCH_SIZE=batch_size,
            GENTHAT_OUTPUT=genthat_output
        ))
    )

    code <- substitute(code_to_run)
    code <-
        if (typeof(code) == "language") {
            code
        } else {
            code_to_run
        }

    # here the save image is OK, because the only code that will be saved
    # comes from the supplied user code
    run <- run_r_code(code, save_image=TRUE, .lib_paths=.tmp_lib, quiet=quiet, clean=clean)

    if (file.exists(genthat_output)) {
        output <- readRDS(genthat_output)
        file.remove(genthat_output)
    } else {
        output <- list(traces=list(), replacements=list())
    }

    output$status <- run$status
    output$run <- run

    attr(output, "class") <- "genthat_traces"

    output
}

export_traces <- function(traces, prefix, output_dir, batch_size) {
    stopifnot(is.list(traces))
    stopifnot(dir.exists(output_dir) && length(output_dir) == 1)
    stopifnot(batch_size >= 0)

    n_traces <- length(traces)

    if (batch_size == 0) {
        batch_size <- n_traces
    }

    n_batches <- ceiling(n_traces / batch_size)
    if (n_batches == 0) {
        return(character())
    }

    batches <- sapply(1:n_batches, function(i) {
        lower <- (i - 1) * batch_size + 1
        upper <- min(n_traces, lower + batch_size)

        fname <- file.path(output_dir, paste0(prefix, "-", i, ".RDS"))
        batch <- traces[lower:upper]

        message("Saving traces [", i,"/", n_batches, "] to: ", fname)
        saveRDS(batch, fname)

        fname
    }, USE.NAMES=FALSE)
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
