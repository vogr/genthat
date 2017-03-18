
######################
# EXPORTED FUNCTIONS #
######################

#' @title Returns traced version of original function.
#'
#' @description More sane replacement of the base::trace function.
#' @param func function value
#' @param func_label name that will be recorded in traces
#' @export
#'
decorate_function_val <- function(func, func_label) {
    if (is.function(func) && is.character(func_label) && length(func_label) == 1) {
        ;
    } else {
        stop("Invalid call of genthat::decorate_function_val()!")
    }

    new_func <- decorate_function_val__(func, func_label)

    listPush(cache$decorated_functions, list(
        type = "value",
        decorated_func = new_func,
        original_func = func
    ))

    new_func
}

#' @title Decorates function bound in environment
#'
#' @param func_name key of binding
#' @param env environment containging binding
#' @export
#'
decorate_function <- function(func_names, env = environment()) {
    if (is.character(func_names) &&
        is.environment(env)) {
        ;
    } else {
        stop("Invalid call to genthat::decorate_function()!")
    }
    
    lapply(func_names, function(func_name) {
        original_func <- env[[func_name]]
        new_function <- decorate_function_val__(original_func, func_name)
        env[[func_name]] <- new_function

        listPush(cache$decorated_functions, list(
            type = "bound",
            name = func_name,
            env = env,
            original_func = original_func
        ))
    })
}

#' @title Decorates package's exported functions.
#'
#' @param package name of package to look for function
#' @param functions character vector of all the functions to decorate
#' @param all decorate all the exported functions
#' @export
#'
decorate_exported <- function(package, functions = NULL, ..., all = FALSE) {
    if (is.character(package) &&
        length(package) == 1 &&
        is.character(functions) &&
        !isTRUE(all)) {
        ;
    } else if (is.character(package) &&
        length(package) == 1 &&
        missing(functions) &&
        isTRUE(all)) {
        ;
    } else {
        stop("Invalid call to genthat::decorate_exported()!")
    }
    
    pkg_namespace <- getNamespace(package)

    if (isTRUE(all)) {
        functions <- listExportedFunctions(package)
    }

    lapply(functions, function(func) {
        label <- paste(package, escapeNonSyntacticName(func), sep=":::")
        original_func <- pkg_namespace[[func]]
        new_function <- decorate_function_val__(original_func, label)
        overwrite_export(func, new_function, package)

        listPush(cache$decorated_functions, list(
            type = "exported",
            name = func,
            package = package,
            original_func = original_func
        ))
    })
}

#' @title Decorates package's exported functions.
#'
#' @param package name of package
#' @param functions character vector of all the functions to decorate
#' @param all decorate all the exported functions
#' @export
#'
decorate_hidden_functions <- function(package) {
    if (is.character(package) && length(package) == 1) {
        ;
    } else {
        stop("Invalid call to genthat::decorate_hidden_functions()!")
    }

    pkg_namespace <- getNamespace(package)
    all_functions <- listEnvFunctions(pkg_namespace)
    exported_function <- listExportedFunctions(package)
    hidden_functions <- setdiff(all_functions, exported_function)

    lapply(hidden_functions, function(func) {
        decorate_function(func, env = pkg_namespace)
    })
}

#' @title Tell you whether the function is a result of genethat's function decoration.
#'
#' @param fn function value
#' @export
#'
is_decorated <- function(fn) {
    if (typeof(fn) != "closure") {
        FALSE
    } else {
        b <- body(fn)
        if (length(b) != 3) {
            FALSE
        } else {
            tracer_expr <- b[[2]]
            isTRUE(attr(tracer_expr, 'isGenthatTracerExpression'))
        }
    }
}

##########################
# NON-EXPORTED FUNCTIONS #
##########################

#' @title Returns traced version of original function.
#'
#' @description More sane replacement of the base::trace function.
#' @param func function value
#' @param func_label name that will be recorded in traces
#' @param enter_function name that will be recorded in traces
#' @param exit_function name that will be recorded in traces
#'
decorate_function_val__ <- function(func, func_label, enter_function, exit_function) {
    if (is_decorated(func)) return(func) #stop("Trying to decorate already decorated function!")

    tracer_expr <- substitute({
        call_id <- call_id_counter$value
        assign("value", call_id + 1, call_id_counter)

        forceArgs <- TRUE

        if (forceArgs) {
            e <- environment()
            arg_names <- names(as.list(match.call())[-1]) # TODO maybe call formals ?

            # force evaluation of named args
            named <- lapply(arg_names[arg_names != ""], function(name) { e[[name]] }) # TODO name "" is only for positional arguments? where are they force?
            # force evaluation of ... args
            dots <- if (hasDots) list(...) else list() # TODO does this force the evaluation of every element?

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

        exit_expr <- substitute({
            exit_function(c)
        }, list(
            c = call_id
        ))

        on.exit(eval(exit_expr))

    }, as.environment(list(
        fname = func_label,
        hasDots = "..." %in% names(formals(func)),
        call_id_counter = cache$call_id_counter,
        enter_function = if (!missing(enter_function)) enter_function else genthat:::enter_function,
        # the default exit_function is dependent on the state set by the default enter_function
        # so when an enter_function is passed by the user exit_function defaults to noop
        exit_function = if (!missing(exit_function)) exit_function else if (!missing(enter_function)) noop else genthat:::exit_function
    )))

    attr(tracer_expr, 'isGenthatTracerExpression') <- TRUE

    body(func) <- substitute({ tracer; original_body }, list(tracer = tracer_expr, original_body = body(func)));

    func
}

start_capture <- function(package_name = NULL, capture_dir = "capture") {
    cache$package_name <- package_name
    cache$capture_dir <- capture_dir
    cache$capture_num <- 0
    cache$decorated <- new.env()
    cache$call_id_counter <- as.environment(list(value = 0))
    cache$capture_arguments <- TRUE
}

#' @title undecorate function
#'
#' @description Reset previously decorate function
#' @param func function name as a character string
#' @param verbose if to print additional output
#' @seealso Decorate
#'
undecorate <- function(func, package, verbose) {
    pkg_namespace <- getNamespace(package)
    fn <- pkg_namespace[[func]]
    if (!is.function(fn)) stop("Trying to undecorate non-function value!")
    if (!is_decorated(fn)) stop("Trying to undecorate non-decorated function!")

    new_body <- body(fn)[[3]]
    new_function <- fn
    body(new_function) <- new_body
    overwrite_export(func, new_function, package)
}


#' @title Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args_env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib genthat
#' @importFrom Rcpp evalCpp
#'
enter_function <- function(fname, args_env, call_id) {
    if (cache$capture_arguments) {
        cache$capture_arguments <- FALSE
        .Call("genthat_enterFunction_cpp", PACKAGE = "genthat", fname, args_env, call_id)
        cache$capture_arguments <- TRUE
    }
}

#' @title Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args_env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib genthat
#' @importFrom Rcpp evalCpp
#'
exit_function <- function(call_id) {
    if (cache$capture_arguments) {
        cache$capture_arguments <- FALSE
        .Call("genthat_exitFunction_cpp", PACKAGE = "genthat", call_id, returnValue())
        cache$capture_arguments <- TRUE
    }
}

#' @title Clear decoration
#'
#' @description Clear anything previously decorate
#' @param verbose if to print additional debugging information. Default \code{TRUE}.
#' @seealso undecorate
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
stop_capture <- function(..., verbose = FALSE) {
    for (f in parseFunctionNames(...))
        undecorate(f$name, f$package, verbose = verbose)
    invisible(NULL)
}

#' @title Stops capturing all currently captured functions.
#'
#' @description Remove tracing functionality for all the functions
#' @param verbose TRUE to display additional information
stop_capture_all <- function(verbose = FALSE) {
    clear_decoration(verbose = verbose)
}

