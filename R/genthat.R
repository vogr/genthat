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
gen_from_package <- function(pkg, types=c("examples", "tests", "vignettes"),
                            output_dir=".",
                            working_dir=tempfile(pattern="gen_from_package-"),
                            batch_size=0,
                            quiet=TRUE,
                            lib_paths=NULL) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(length(output_dir) == 1)
    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(length(working_dir) == 1)
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))

    output_dir <- normalizePath(output_dir, mustWork=TRUE)
    working_dir <- normalizePath(working_dir, mustWork=TRUE)
    pkg_dir <- find.package(pkg, lib_paths)
    stats_file <- file.path(working_dir, "genthat-exports.csv")

    runner <- function(type) {
        function(fname, quiet) {
            site_file <- genthat_tracing_site_file(
                pkg,
                output_dir,
                tag=paste0(type, ".", tools::file_path_sans_ext(basename(fname))),
                stats_file=stats_file,
                batch_size=batch_size
            )

            run_r_script(fname, site_file=site_file, quiet=quiet, lib_paths=lib_paths)
        }
    }

    runs <- lapply(types, function(type) run_package(pkg, pkg_dir, type, working_dir, quiet=quiet, runner(type)))

    if (!file.exists(stats_file)) {
        stop("Tracing failed - cannot find the stats file `", stats_file, "'")
    }

    traces <- read_stats_file(stats_file)
    runs <- unlist(runs)
    tags <- tools::file_path_sans_ext(names(runs))
    status <- data.frame(tag=tags, status=runs, stringsAsFactors=FALSE, row.names=NULL)

    merge(traces, status, by="tag")
}

#' @export
#'
export_traces <- function(traces, output_dir,
                         tag=NA,
                         stats_file=NULL,
                         batch_size=0) {

    stopifnot(is.list(traces))
    stopifnot(length(output_dir) == 1)
    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(is.null(stats_file) || (is.character(stats_file) && length(stats_file) == 1))
    stopifnot(is.na(tag) || (is.character(tag) && length(tag) == 1 && nchar(tag) > 0))
    stopifnot(batch_size >= 0)

    n_traces <- length(traces)
    if (n_traces == 0) {
        return(invisible(character()))
    } else {
        message("Saving ", n_traces, " traces into ", output_dir)
    }

    if (batch_size == 0) {
        batch_size <- n_traces
    }

    file_prefix <- if (is.na(tag)) {
        ""
    } else {
        paste0(tag, "-")
    }

    n_batches <- ceiling(n_traces / batch_size)
    n_existing <- length(Sys.glob(path=file.path(output_dir, paste0(file_prefix, "*", ".RDS"))))

    batches <- lapply(1:n_batches, function(i) {
        lower <- (i - 1) * batch_size + 1
        upper <- min(n_traces, lower + batch_size)

        fname <- file.path(output_dir, paste0(file_prefix, (i + n_existing), ".RDS"))
        batch <- traces[lower:upper]

        message("Saving traces [", i, "/", n_batches, "] to: ", fname)
        saveRDS(batch, fname)

        data.frame(
            tag=tag,
            filename=fname,
            n_traces=length(batch),
            stringsAsFactors=FALSE
        )
    })

    stats <- do.call(rbind, batches)

    if (!is.null(stats_file) && nrow(stats) > 0) {
        write.table(
            stats,
            file=stats_file,
            row.names=FALSE,
            col.names=FALSE,
            append=TRUE,
            qmethod="double",
            sep=","
        )
    }

    stats$filename
}

genthat_tracing_site_file <- function(...) {
    site_file_code <- genthat_tracing_preamble(...)
    site_file <- tempfile()

    cat(site_file_code, file=site_file)

    site_file
}

genthat_tracing_preamble <- function(pkgs,
                                    output_dir,
                                    tag="",
                                    debug=getOption("genthat.debug", FALSE),
                                    stats_file=NULL,
                                    batch_size=0) {

    stopifnot(is.character(pkgs) && length(pkgs) > 0)

    paste(c(
        '## genthat tracing preamble',
        paste0('options(genthat.debug=', debug, ')'),
        '',
        sapply(pkgs, function(x) paste0('genthat::decorate_environment("', x, '")')),
        '',
        'reg.finalizer(loadNamespace("genthat"), onexit=TRUE, function(x) {',
        paste0(
            '  genthat::export_traces(genthat::copy_traces(genthat::get_tracer()), ',
            '"', output_dir, '", ',
            'tag="', tag, '",',
            'batch_size=', batch_size, ',',
            'stats_file="', stats_file, '"',
            ')'
        ),
        '})',
        ''
    ), collapse="\n")
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
    options(genthat.tracing=TRUE)
}

#' @export
#'
disable_tracing <- function() {
    options(genthat.tracing=FALSE)
}

#' @export
#'
is_tracing_enabled <- function() {
    isTRUE(getOption("genthat.tracing"))
}

#' @export
#'
is_debug_enabled <- function() {
    isTRUE(getOption("genthat.debug"))
}

#' @export
#'
read_stats_file <- function(fname) {
    read.csv(
        fname,
        header=FALSE,
        stringsAsFactors=FALSE,
        col.names=c("tag", "filename", "n_traces")
    )
}

run_integration_tests <- function() {
    withr::with_options(list(genthat.run_itests=T), devtools::test(filter="integration"))
}
