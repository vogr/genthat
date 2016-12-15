
start_capture <- function(package.name = NULL, capture.dir = "capture") {
    cache$package.name <- package.name
    cache$capture.dir <- capture.dir
    cache$capture_num <- 0
    cache$decorated <- new.env()
    cache$call_id_counter <- as.environment(list(value = 0))
    cache$capture.arguments <- TRUE
}

isDecoratedFun <- function(fn) {
    if (typeof(fn) != "closure") {
        FALSE
    } else {
        b <- body(fn)
        if (length(b) != 3) {
            FALSE
        } else {
            tracer.expr <- b[[2]]
            isTRUE(attr(tracer.expr, 'isGenthatTracerExpression'))
        }
    }
}

#' @title Decorates function to capture calls and return values
#'
#' @description More sane replacement of the base::trace function.
#' @param func function value
#' @param func_name function name
#' @export
#'
decorate_function_val <- function(func, func_name, ..., enter_function, exit_function) {
    if (isDecoratedFun(func)) return(func) #stop("Trying to decorate already decorated function!")

    tracer.expr <- substitute({
        call_id <- call_id_counter$value
        assign("value", call_id + 1, call_id_counter)

        forceArgs <- FALSE

        if (forceArgs) {
            e <- environment()
            arg_names <- names(as.list(match.call())[-1])

            # force evaluation of named args
            named <- lapply(arg_names[arg_names != ""], function(name) { e[[name]] })
            # force evaluation of ... args
            dots <- if (hasDots) list(...) else list()

            args <- list() # returned argument list
            dot_counter <- 1
            for (arg_counter in seq(1, length(arg_names))) {
                name <- arg_names[arg_counter]
                if (name == "") {
                    x <- dots[[dot_counter]]
                    dot_counter <- dot_counter + 1 
                    args[[arg_counter]] <- x
                } else {
                    args[[name]] <- e[[name]]
                }
            }   
        } else {
            args <- lapply(as.list(match.call())[-1], function(e) eval.parent(e, 3))
        }

        enter_function(
            fname,
            args,
            call_id
        )

        exit.expr <- substitute({
            exit_function(c)
        }, list(
            c = call_id
        ))

        on.exit(eval(exit.expr))

    }, as.environment(list(
        fname = func_name,
        hasDots = "..." %in% names(formals(func)),
        call_id_counter = cache$call_id_counter,
        enter_function = if (!missing(enter_function)) enter_function else genthat:::enter_function,
        # the default exit_function is dependent on the state set by the default enter_function
        # so when an enter_function is passed by the user exit_function defaults to noop
        exit_function = if (!missing(exit_function)) exit_function else if (!missing(enter_function)) noop else genthat:::exit_function
    ))) #nolint

    attr(tracer.expr, 'isGenthatTracerExpression') <- TRUE

    body(func) <- substitute({ tracer; original.body }, list(tracer = tracer.expr, original.body = body(func)));

    func
}

#' @title Decorates function to capture calls and return values
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @param package name of package to look for function
#' @param verbose if to print additional output
#' @export
#'
decorate <- function(func, package, verbose, ..., enter_function, exit_function) {
    if (class(func) != "character") stop("Parameter `func` must be a string!")
    if (missing(package)) stop("Missing parameter package!")
    if (class(package) != "character") stop("Parameter `package` must be a string!")

    pkg_namespace <- getNamespace(package)
    func_name <- if (is.na(package)) func else paste(package, escapeNonSyntacticName(func), sep=":::")
    new_function <- decorate_function_val(pkg_namespace[[func]], func_name, enter_function = enter_function, exit_function = exit_function)

    overwrite_export(func, new_function, package)

    hidden <- !func %in% ls(as.environment(if (is.na(package)) .GlobalEnv else paste("package", package, sep=":")))
    cache$decorated[[func]] <- list(func=func, package=package, hidden=hidden)
}

#' @title undecorate function
#'
#' @description Reset previously decorate function
#' @param func function name as a character string
#' @param verbose if to print additional output
#' @export
#' @seealso Decorate
#'
undecorate <- function(func, package, verbose) {
    pkg_namespace <- getNamespace(package)
    fn <- pkg_namespace[[func]]
    if (!is.function(fn)) stop("Trying to undecorate non-function value!")
    if (!isDecoratedFun(fn)) stop("Trying to undecorate non-decorated function!")

    new_body <- body(fn)[[3]]
    new_function <- fn
    body(new_function) <- new_body
    overwrite_export(func, new_function, package)
}


#' @title Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib genthat
#' @importFrom Rcpp evalCpp
#' @export
#'
enter_function <- function(fname, args.env, call_id) {
    if (cache$capture.arguments) {
        cache$capture.arguments <- FALSE
        .Call("genthat_enterFunction_cpp", PACKAGE = "genthat", fname, args.env, call_id)
        cache$capture.arguments <- TRUE
    }
}

#' @title Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib genthat
#' @importFrom Rcpp evalCpp
#' @export
#'
exit_function <- function(call_id) {
    if (cache$capture.arguments) {
        cache$capture.arguments <- FALSE
        .Call("genthat_exitFunction_cpp", PACKAGE = "genthat", call_id, returnValue())
        cache$capture.arguments <- TRUE
    }
}

#' @title Clear decoration
#'
#' @description Clear anything previously decorate
#' @param verbose if to print additional debugging information. Default \code{TRUE}.
#' @seealso undecorate
#' @export
clear_decoration <- function(verbose) {
    decorated_funs <- cache$decorated
    lapply(ls(decorated_funs, all.names = TRUE), function(key) {
        decorated_fn <- decorated_funs[[key]]
        undecorate(decorated_fn$func, decorated_fn$package, verbose = verbose)
    })
}

#' @title Stops capturing the selected functions.
#'
#' @description This function removes the tracing functionality for specified function
#' @param ... Functions whose capture is to be dropped (uses the same format as capture)
#' @param verbose TRUE to display additional information
#' @export
stop_capture <- function(..., verbose = FALSE) {
    for (f in parseFunctionNames(...))
        undecorate(f$name, f$package, verbose = verbose)
    invisible(NULL)
}

#' @title Stops capturing all currently captured functions.
#'
#' @description Remove tracing functionality for all the functions
#' @param verbose TRUE to display additional information
#' @export
stop_capture_all <- function(verbose = FALSE) {
    clear_decoration(verbose = verbose)
}

