#' Decorate all functions from given package and runs package code
#'
#' Decorates all functions in a package and then generates test cases based on
#'     the code contained in the package examples, vignettes and tests.
#'
#' @param packages chr vector of package names whose functions should be traced.
#'     All the packages need to be installed in the `lib_paths`.
#' @param files chr vector of paths to R files that shall be run
#' @param output_dir the name of the directory where to output traces or NULL if
#'     traces should not be saved
#'
#' @importFrom utils read.csv
#'
#' @export
#'
# TODO: this shall call trace_functions
trace_package <- function(packages, files,
                            output_dir=".",
                            action=c("stats", "export", "generate"),
                            working_dir=tempfile(pattern="genthat-trace-"),
                            quiet=TRUE, lib_paths=NULL) {

    stopifnot(is.character(files))
    if (length(files) == 0) {
        return(character())
    }

    if (is.null(names(files))) {
        names(files) <- files
    }

    stopifnot(is.character(packages), length(packages) > 0)
    # check if packages exists in the provided libraries
    find.package(packages, lib_paths)

    # both output_dir and working_dir must be absolute because the future R
    # instances will be run from the dirs where the files are
    stopifnot(is_chr_scalar(output_dir))
    stopifnot(dir.exists(output_dir) || dir.create(output_dir, recursive=TRUE))
    output_dir <- normalizePath(output_dir, mustWork=TRUE)

    stopifnot(is_chr_scalar(working_dir))
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))
    working_dir <- normalizePath(working_dir, mustWork=TRUE)

    action <- match.arg(action, c("stats", "export", "generate"), several.ok=FALSE)

    # TODO: check tracer
    # TODO: check decorator

    # used for export traces during run
    stats_file <- tempfile("genthat-tracing", tmpdir=working_dir, fileext=".csv")
    set_tracer_session_file <- tempfile("set-tracer-session", tmpdir=working_dir, fileext=".RDS")
    on.exit({
        if (file.exists(stats_file)) file.remove(stats_file)
        if (file.exists(set_tracer_session_file)) file.remove(set_tracer_session_file)
    })

    run_file <- function(fname) {
        log_debug("Running ", fname, " ...")

        # TODO: all this should go into an RDS file
        vars <- list()
        vars$debug <- is_debug_enabled()
        vars$keep_all_traces <- getOption("genthat.keep_all_traces", FALSE)
        vars$keep_failed_traces <- getOption("genthat.keep_failed_traces", FALSE)
        vars$tracer_type <- getOption("genthat.tracer_type")
        vars$session_file <- set_tracer_session_file
        vars$action <- action
        vars$current_file <- fname
        vars$output_dir <- output_dir
        vars$stats_file <- stats_file
        vars$max_trace_size <- getOption("genthat.max_trace_size", .Machine$integer.max)
        vars$packages <- paste(packages, sep=",")

        # convert the variables to the expected format GENTHAT_<VAR>=<value>
        env <- sapply(names(vars), function(x) {
            paste(toupper(paste0("genthat_", x)), as.character(vars[[x]]), sep="=")
        }, USE.NAMES=FALSE)

        site_file <- system.file("genthat-tracing-site-file.R", package="genthat")

        if (!file.exists(fname)) {
            paste(fname, "does not exist")
        } else {
            tryCatch({
                run <- run_r_script(fname, site_file=site_file, env=env, quiet=quiet, lib_paths=lib_paths)

                if (file.exists(stats_file)) {
                    # running of the file might have failed, but still some traces
                    # might have been generated

                    # TODO: do we want to indicate somewhere that the running has failed?
                    st <- read.csv(stats_file, stringsAsFactors=FALSE)
                    file.remove(stats_file)
                    st
                } else {
                    # Running either failed or there were no calls to the traced
                    # functions (e.g. a data file) so the finalizer exporting the
                    # traces did not kicked in. based on the status we can figure
                    # out what has happened.
                    run
                }
            }, error=function(e) {
                e$message
            })
        }
    }

    log_debug("Running ", length(files), " files")
    lapply(files, run_file)
}
