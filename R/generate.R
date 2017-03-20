
toStringLiteral <- function (str) {
    concatVec(deparse(str))
}

listToArgumentList <- function(args) {
    elems <- c()
    argNames <- names(args)
    for (i in seq(along.with = args)) {
        src <- if (length(argNames) != 0 && argNames[[i]] != "") {
            concat(argNames[i], " = ", args[[i]])
        } else {
            args[[i]]
        }
        elems <- c(elems, src) 
    }
    concatVec(elems, sep=", ")
}

#' Do we have access to the function in test generation phase, to recompute the return value from the arguments?
#'
#' @param function_name string name of binding
#' @return boolean value signaling whether evaluating the supplied function name in the global scope yields the function or not.
#'
#' @examples
#' isAccessibleFunction("myFn1") == FALSE
#' isAccessibleFunction("ggplot2::ggplot") == TRUE
isAccessibleFunction <- function(function_name) {
    length(grep("::", function_name)) != 0 # TODO handle functions and package-names containing "::"
}

is_valid_trace <- function(trace) {
    if (
        is.list(trace) &&
        length(trace) == 4 &&
        trace$type == "trace" &&
        is.character(trace$func) &&
        length(trace$func) == 1 &&
        is.character(trace$args) &&
        length(trace$args) == 1 &&
        is.character(trace$retv) &&
        length(trace$retv) == 1
    ) {
        TRUE
    } else {
        FALSE
    }
}

deserialize <- function(x) eval(parse(text=x))

#' @title Generate test case from trace
#'
#' @description This function generates a test case from the passed trace.
#' @param trace trace value
#'
generate_tc <- function(trace) {
  if (!is_valid_trace(trace)) stop("Passed invalid trace to generate_tc()")
  func <- trace$func
  args <- trace$args
  retv <- trace$retv

  #cache$warns <- NULL
  #cache$errs <- NULL
  #test_body <- ""
  #warningChecks <- ""

  #if (isAccessibleFunction(func)) {
  #    pkgName <- sub("^(.*):::.*$", "\\1", func)
  #    fn <- eval(parse(text=func))
  #    call <- as.call(if (0 == length(valid_argv)) list(fn) else append(fn, valid_argv))

  #    new_retv <- withCallingHandlers(
  #                  tryCatch(
  #                      eval(call, envir = getNamespace(pkgName)),
  #                      error = function(e) cache$errs <- e$message
  #                  ),
  #                  warning=function(w) {
  #                      cache$warns <- ifelse(is.null(cache$warns), w$message, paste(cache$warns, w$message, sep="; "))
  #                      invokeRestart("muffleWarning")
  #                  }
  #            )

  #    et <- system.time({ sameRetv <- isTRUE(all.equal(new_retv, valid_retv)) })[1]
  #    message(paste0("all.equal time: ", et, " seconds\n"))
  #    if (!sameRetv) {
  #        cache$retv_mismatch_count <- cache$retv_mismatch_count + 1L
  #        return(list(
  #            type = "err",
  #            err_type = "RETV_MISMATCH",
  #            msg = paste(
  #              paste0("cause: ", "RETV_MISMATCH"),
  #              paste0("func: ", func),
  #              paste0("argv: ", argv),
  #              paste0("retv: ", retv),
  #              paste0("computed_retv: ", serialize_r(new_retv)),
  #              "",
  #              sep = "\n"
  #           )
  #        ))
  #    }
  #    
  #}

  # strip 'list(' & ')' from args
  callSource <- paste0(func, "(", substr(args, 6, nchar(args, type="bytes") - 1), ")")

  test_body <- concat(
      "\texpected <- ", retv, "\n",
      "\texpect_equal(", callSource, ", expected)\n"
  )

  src <- concat(
    "test_that(", deparse(func), ", {\n",
    test_body,
    "})"
  )
}

#' @title Generates tests from captured information.
#'
#' @description This function takes the tracing information collected by capture and generates
#' testthat compatible testcases.
#'
#' @param output_dir Directory to which the tests should be generated.
#' @export
gen_tests <- function(output_dir = "generated_tests") {
    if (missing(output_dir)) {
        if (!dir.create(out)) {
            stop("Couldn't create output dir!")
        }
    }

    c <- 0
    test_cases <- map_iterator(traces, function(trace) {
        if (trace$type == "trace") {
            tsources <- generate_tc(trace)
            fname <- file.path(output_dir, paste0("tc-", c, ".R"))
            c <- c + 1
            write(tsources, file = fname)
        }
    })
}

