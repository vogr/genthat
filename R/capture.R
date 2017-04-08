
######################
# EXPORTED FUNCTIONS #
######################

#' @title Decorate functions
#'
#' @description API function to decorate functions.
#' Usages
#' ------
#' decorate_functions(fn1)
#' decorate_functions(fn1, fn2)
#' decorate_functions("fn1", env=environment())
#' decorate_functions("fn1", "fn2", env=environment())
#' decorate_functions("fn1", "fn2", package="package_name")
#' decorate_functions(package="package_name") # all from this package
#' decorate_functions(env=some_environment) # all from some_environment
#' decorate_functions(package="package_name", include_hidden=TRUE) # all including hidden
#' decorate_functions("hidden_fn1", "hidden_fn2", package="package_name", include_hidden=TRUE)
#' @export
#'
decorate_functions <- function(...) {
	args <- list(...)
    arg_names <- if (is.character(names(args))) names(args) else sapply(args, function(x) "")
	if ("package" %in% arg_names) {
		package <- args[["package"]]
		include_hidden <- isTRUE(args[["include_hidden"]])
		fnames <- unlist(args[arg_names == ""], use.names = FALSE)
		if (length(fnames) == 0) {
			decorate_exported(package, all = TRUE)
			if (include_hidden) {
				decorate_hidden_functions(package)
			}
		} else {
			lapply(fnames, function(fname) {
				is_exported <- fname %in% listExportedFunctions(package)
				if (is_exported) {
					decorate_exported(package, fname) 
				} else {
					decorate_function_env(fname, env = getNamespace(package))
				}
			})
		}
	} else if ("env" %in% arg_names || is.character(args[[1]])) {
		fnames <- unlist(args[arg_names == ""], use.names = FALSE)
		env <- if ("env" %in% arg_names) args[["env"]] else sys.frame(-1)
		decorate_function_env(fnames, env = env)
	} else {
        fn_vals <- args
        labels <- sapply(as.list(substitute(list(...))[-1]), as.character)
        pairs <- zipList(fn_vals, labels)
		lapply(pairs, function(pair) {
			decorate_function_val(pair[[1]], pair[[2]])
		})
	}
}

#' @title Undecorate all functions.
#'
#' @description Undecorate all functions.
#' Functions decorated through `decorate_function_val()` are not undecorated.
#' @export
#'
undecorate_all <- function() {
    lapply(cache$decorated_functions, function(decorated_fun) {
        if (decorated_fun$type == "value") {
            ; # do nothing
        } else if (decorated_fun$type == "bound") {
            env <- decorated_fun$env
            name <- decorated_fun$name
            env[[name]] <- decorated_fun$original_func
        } else if (decorated_fun$type == "exported") {
            package <- decorated_fun$package
            name <- decorated_fun$name
            overwrite_export(name, decorated_fun$original_func, package)
        }
    })
    cache$decorated_functions <- list()
}

#' @title Tells you whether the function is a result of genthat's function decoration.
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
#'
decorate_function_val <- function(func, func_label) {
    if (!is.function(func)) stop("Invalid call of genthat::decorate_function_val(): func must be a function!")
    if (!is.character(func_label) || length(func_label) != 1) stop("Invalid call of genthat::decorate_function_val(): func_label must be a character scalar!")

    new_func <- decorate_function_val__(func, func_label)

    add_decorated_function(list(
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
#'
decorate_function_env <- function(func_names, env = sys.frame(-1)) {
    if (!is.character(func_names)) stop(paste0("Invalid call to genthat::decorate_function_env(): func_names must be a character vector (was ", typeof(func_names), ")!"))
    if (!is.environment(env)) stop("Invalid call to genthat::decorate_function_env(): env must be an environment!")
    
    lapply(func_names, function(func_name) {
        original_func <- env[[func_name]]
        new_function <- decorate_function_val__(original_func, func_name)
        env[[func_name]] <- new_function

        add_decorated_function(list(
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

        add_decorated_function(list(
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
#'
decorate_hidden_functions <- function(package) {
    if (is.character(package) && length(package) == 1) {
        ;
    } else {
        stop("Invalid call to genthat::decorate_hidden_functions()!")
    }

    pkg_namespace <- getNamespace(package)
    hidden_functions <- listHiddenFunctions(package)

    lapply(hidden_functions, function(func) {
        decorate_function_env(func, env = pkg_namespace)
    })
}

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
        call_id <- genthat:::gen_cid()
        
        call_args <- as.list(sys.call())[-1]

        is.formula <- function(x) is.call(x) && as.character(x[[1]]) == "~"

        get_exprs_from_args <- function(args, args_filter) {
            filtered <- Filter(args_filter, args)
            exprs <- as.character(c(lapply(filtered, function(x) all.names(x)), recursive = TRUE))
            exprs <- Filter(function(x)!x %in% genthat:::operators, exprs)
            unique(exprs)
        }

        args <- tryCatch({
            e <- environment()

            #separate processing of optional expressions (e.g. exprs inside formulas)
            optional_exprs <- get_exprs_from_args(call_args, function(x) is.formula(x))
            required_exprs <- get_exprs_from_args(call_args, function(x) !is.formula(x))

            #optional exprs which can be recorded
            optional_filter <- function(x)!(x %in% required_exprs) && exists(x, envir = e) && !is.function(get(x, e))
            optional_exprs <- Filter(optional_filter, optional_exprs)

            elem_exprs <- c(required_exprs, optional_exprs)
            elem_vals <- lapply(elem_exprs, function(name) get(name, e))
            if (length(elem_exprs) != 0) {
                names(elem_vals) <- elem_exprs
            }

            list(
                call = call_args,
                vals = elem_vals
            )
        }, error = function(e) {
            print("error recording args!")
            print(e)
            NULL
        })

        call_exit <- enter_function(
            fname,
            args,
            call_id
        )

        if (call_exit) {
            exit_expr <- substitute({
                exit_function(c)
            }, list(
                c = call_id
            ))
            on.exit(eval(exit_expr))
        }
    }, as.environment(list(
        fname = func_label,
        hasDots = "..." %in% names(formals(func)),
        enter_function = if (!missing(enter_function)) enter_function else genthat:::enter_function,
        # the default exit_function is dependent on the state set by the default enter_function
        # so when an enter_function is passed by the user exit_function defaults to noop
        exit_function = if (!missing(exit_function)) exit_function else if (!missing(enter_function)) noop else genthat:::exit_function
    )))

    attr(tracer_expr, 'isGenthatTracerExpression') <- TRUE

    body(func) <- substitute({ tracer; original_body }, list(tracer = tracer_expr, original_body = body(func)));

    func
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
        if (is.null(args_env)) {
            res <- list(type = "error", error_description = "Couldn't force arguments.")
        } else {
            res <- .Call("genthat_enterFunction_cpp", PACKAGE = "genthat", fname, args_env, call_id)
        }
        cache$capture_arguments <- TRUE
        if (typeof(res) != "list") { # success # TODO WHY can't this be res == 0 ?
            TRUE
        } else {
            error_description <- res$error_description
            push_trace(res)
            FALSE
        }
    } else {
        FALSE
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
        res <- .Call("genthat_exitFunction_cpp", PACKAGE = "genthat", call_id, returnValue())
        cache$capture_arguments <- TRUE
        push_trace(res)
    }
}

#' @title Empties the data structure storing the arguments for exit_function.
#'
clear_call_cache <- function() {
    .Call("genthat_clearCallCache_cpp", PACKAGE = "genthat")
}

