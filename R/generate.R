format_calling_args <- function(args, include_names=TRUE, serializer) {
    args <- lapply(args, serializer$serialize_value)

    if (include_names && !is.null(names(args))) {
        pairs <- zip(name=names(args), value=args)

        lapply(pairs, function(x) {
            if(!is_empty_str(x$name)) {
                paste0(escape_name(x$name), "=", x$value)
            } else {
                x$value
            }
        })
    } else {
        args
    }
}

dump_raw_trace <- function(trace) {
    s <- utils::capture.output(utils::str(trace))
    s <- paste("# ", s, collapse="\n")
    s <- paste0("# TRACE:\n", s, "\n\n")
    s
}

generate_call <- function(trace, serializer) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun
    pkg <- trace$pkg
    if (is.null(pkg)) {
        pkg <- ""
    }

    if (is_infix_fun(fun) && pkg == "base") {
        args <- format_calling_args(trace$args, include_names=FALSE, serializer)

        if (length(args) != 2) {
            stop("Call to infix function: `", fun,
                 "` does not have exactly two arguments, instead it has: ",
                 paste(args, collapse=", "))
        }

        sep <- if (is_infix_fun_no_space(fun)) "" else " "

        paste(args[[1]], fun, args[[2]], collapse=sep)
    } else {
        args <- format_calling_args(trace$args, include_names=TRUE, serializer)
        args_str <- paste(args, collapse=", ")
        fun <- escape_name(fun)
        pkg <- trace$pkg
        pkg <- if (is.null(trace$pkg)) "" else paste0(pkg, ":::")

        paste0(pkg, fun, '(', args_str, ')')
    }
}

generate_globals <- function(globals, serializer) {
    names <- sapply(names(globals), escape_name, USE.NAMES=FALSE)
    values <- lapply(globals, serializer$serialize_value)

    paste(names, values, sep=" <- ", collapse="\n")
}

#' @title Generate test case code from a trace
#' @description Given a genthat trace it generates a corresponding test case
#'
#' @param trace trace value
#' @param include_trace_dump whether to include raw trace at the beginning of the test
#' the trace is formatted using `str` function.
#'
#' @importFrom utils str
#' @export
generate_test <- function(trace, ...) {
    UseMethod("generate_test")
}

#' @export
generate_test.genthat_trace <- function(trace, dest_basename, include_trace_dump=FALSE, format_code=TRUE) {
    tryCatch({
        serializer <- new(Serializer)
        call <- generate_call(trace, serializer)
        globals <- generate_globals(trace$globals, serializer)
        externals <- new.env(parent=emptyenv())

        # Save seed and retv in the datafile
        externals$.ext.seed <- trace$seed
        externals$.ext.retv <- trace$retv
        #ext_file <- basename(paste0(dest_basename, ".ext"))

        header <- paste0(
            "library(", trace$pkg, ")\n\n"
        )

        if (include_trace_dump) {
            header <- paste(header, dump_raw_trace(trace), sep="\n")
        }

        fun <- paste0(
            "function_to_run <- function() {\n",
                globals, if (nchar(globals) > 0) '\n' else '',
                call, "\n",
            "}\n"
        )
        
        # loop <- paste0(
        #     "for (i in 1:n_iterations) {\n",
        #         ".Random.seed <<- env$.ext.seed\n",
        #         "t0  <- Sys.time()\n",
        #         "res <- function_to_run()\n",
        #         "times[[i]] <- Sys.time() - t0\n",
        #         "if (! isTRUE(all.equal(res, retv)))\n",
        #             "stop(\"Unexpected result\\n\", res, \"\\nexpected\\n\", retv)\n",
        #     "}\n"
        # )
        


        # footer <- paste0(
        #     "if(length(argv) > 0) { \n",
        #         "saveRDS(times, argv[[1]])\n",
        #     "} else {\n",
        #         "times\n",
        #     "}\n"
        # )

        code <- paste0(
            header, "\n",
            fun, "\n"
        )

        if (format_code) {
            code <- reformat_code(code)
        }

        serializer$externals(externals)
        attr(code, "externals") <- externals

        code
    }, error=function(e) {
        log_debug("Failed to generate_test:")
        log_debug(e$message)
        # this so we can have a systematic prefix for the error message
        # which helps to filter problems by their class
        stop(simpleError(paste("Generate error:", trimws(e$message, which="both")), e$call))
    })
}

#' @export
generate_test.genthat_trace_entry <- function(trace, ...) {
    stop("Trace error: No return value")
}

#' @export
generate_test.genthat_trace_skipped <- function(trace, ...) {
    stop("Trace error: Trace too big (", trace$skipped, ")")
}

#' @export
generate_test.genthat_trace_error <- function(trace, ...) {
    stop(paste("Code error:", trace$error$message))
}

#' @export
generate_test.genthat_trace_failure <- function(trace, ...) {
    stop(paste("Trace error:", trace$failure$message))
}

get_dest_basename <- function(pkg, fun, output_dir) {
    stopifnot(is_chr_scalar(pkg))
    stopifnot(is_chr_scalar(fun))
    stopifnot(is_chr_scalar(output_dir))
    dname <- file.path(output_dir, pkg, fun)

    R_fname <- next_file_in_row(file.path(dname, "test.R"))
    tools::file_path_sans_ext(R_fname)
}


#' @param tests this should be a data.frame with class genthat_tests, a result
#'     from calling `generate_tests`.
#' @export
#'
save_test <- function(dest_basename, code) {

    stopifnot(is.character(code), length(code) > 0)

    dname <- dirname(dest_basename)
    stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

    fname <- paste0(dest_basename, ".R")

    externals <- attr(code, "externals")
    if (length(externals) > 0) {
        fname_ext <- paste0(dest_basename, ".ext")
        saveRDS(externals, fname_ext)
    }

    write(paste(code, collapse="\n\n"), file=fname)
    fname
}

reformat_code <- function(code) {
    tryCatch({
        code <- formatR::tidy_source(
            text=code,
            output=FALSE,
            comment=FALSE,
            blank=TRUE,
            arrow=TRUE,
            brace.newline=FALSE,
            indent=4,
            width.cutoff=120
        )

        code <- code$text.tidy

        paste(code, collapse="\n")
        code
    })
}

#' @title generate test from a trace
#' @description given a trace, it generates a test and stores it in a file
#' @return a data frame with the following
#' pkg        : chr
#' fun        : chr
#' filename   : chr (can be NA in which case error must be a chr)
#' error      : chr (can be NA in which case code must be NA)
#' elapsed    : numeric (can be NA)
#'
#' @export
#'
generate_test_file <- function(trace, output_dir, ...) {
    pkg <- if (is.null(trace$pkg)) "_NULL_" else trace$pkg
    fun <- if (is.null(trace$fun)) "_NULL_" else trace$fun
    
    dest_basename <- get_dest_basename(pkg, fun, output_dir)

    code <- generate_test(trace, dest_basename, ...)

    save_test(dest_basename, code)
}
