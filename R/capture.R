
call_id_counter <- new.env(parent = emptyenv())

call_id_counter$value <- 0

force_rebind <- function(what, value, env) {
    sym <- as.name(what)
    .Internal(unlockBinding(sym, env))
    assign(what, value, env)
    .Internal(lockBinding(sym, env))
}

overwrite_export <- function(name, val, package) {
    pkg.namespace_env <- getNamespace(package)
    pkg.package_env <- as.environment(paste0("package:", package))
    force_rebind(name, val, pkg.namespace_env)
    is.exported <- exists(name, envir = pkg.package_env, inherits = FALSE)
    if (is.exported) {
        force_rebind(name, val, pkg.package_env)
        rev_dep.imports_envs <- tryCatch(getNamespaceUsers(package), error = function(e) c())
        lapply(rev_dep.imports_envs, function(imports_env) force_rebind(name, val, imports_env))
    }
    invisible(NULL)
}

#trace_function <- function(func, func_name, tracer) {
#    pkg_namespace <- getNamespace(package)
#    func <- pkg_namespace[[func_name]]
#    body(func) <- substitute({ tracer; original.body }, list(tracer = tracer, original.body = body(func)));
#    overwrite_export(func_name, func, package)
#}

#' @title Decorates function to capture calls and return values
#'
#' @description More sane replacement of the base::trace function.
#' @param func function value
#' @param func_name function name
#' @export
#'
decorate_function_val <- function(func, func_name) {

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

        testr:::enter_function(
            fname,
            args,
            call_id
        )

        exit.expr <- substitute({
            testr:::exit_function(c)
        }, list(
            c = call_id
        ))

        on.exit(eval(exit.expr))

    }, as.environment(list(
        fname = func_name,
        hasDots = "..." %in% names(formals(func)),
        call_id_counter = call_id_counter
    ))) #nolint

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
decorate <- function(func, package, verbose) {
    if (identical(class(library), "function") && getRversion() < '3.3.0') {
        suppressMessages(trace(library,
                               exit=quote(if (!missing(package)) testr:::refresh_decoration(package)),
                               print = FALSE))
    }
    if (!cache$trace_replaced && getRversion() < '3.3.0') {
        replace_trace()
    }
    if(class(func) != "character" || (!missing(package) && class(package) != "character")){
        stop("wrong argument type!")
    }
    if (is.na(package)){
        package <- utils::find(func)
        if (length(package) == 0) {
            warning(sprintf("Can't determine a package for function '%s'. If function is hidden, use package param",
                            func))
            return(invisible())
        } else {
            if (length(package) > 1) {
                warning("Function found in multiple packages, supply the exact name")
                return(invisible())
            }
        }
        if (package != ".GlobalEnv")
            package <- substr(package, 9, nchar(package))
        else
            package <- NA
    }
    if (is.na(package))
        isS3 <- is_s3_generic(func)
    else
        isS3 <- is_s3_generic(func, getNamespace(package))
    if (isS3) {
        warning("Not decorating S3 generic")
        return(invisible())
    }

    pkg_namespace <- getNamespace(package)
    func_name <- if (is.na(package)) func else paste(package, escapeNonSyntacticName(func), sep=":::")
    new_function <- decorate_function_val(pkg_namespace[[func]], func_name)

    overwrite_export(func, new_function, package)

    hidden <- !func %in% ls(as.environment(if (is.na(package)) .GlobalEnv else paste("package", package, sep=":")))
    .decorated[[func]] <- list(func=func, package=package, hidden=hidden)
}

#' @title undecorate function
#'
#' @description Reset previously decorate function
#' @param func function name as a character string
#' @param verbose if to print additional output
#' @export
#' @seealso Decorate
#'
undecorate <- function(func, verbose) {
    return()
    if (class(func) == "character"){
        fname <- func
    } else {
        stop("wrong argument type!")
    }
    ind <- which(fname %in% ls(.decorated, all.names = TRUE))
    if (length(ind) == 0) {
        stop(sprintf("Function %s was not decorated!", fname))
    }
    package <- .decorated[[func]]$package
    hidden <- .decorated[[func]]$hidden
    params <- list(fname)
    if (hidden)
        params[["where"]] <- call("getNamespace", package)
    if (verbose) {
        do.call(untrace, params)
    } else {
        suppressMessages(do.call(untrace, params))
    }
    rm(list=c(func), envir=.decorated)
}


#' @title Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#'
enter_function <- function(fname, args.env, call_id) {
    if (!testr_options("capture.arguments"))
        return(NULL)
    .Call("testr_enterFunction_cpp", PACKAGE = "testr", fname, args.env, call_id)
}

#' @title Write down capture information
#'
#' @description This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#'
exit_function <- function(call_id) {
    if (!testr_options("capture.arguments"))
        return(NULL)
    .Call("testr_exitFunction_cpp", PACKAGE = "testr", call_id, returnValue())
}

#' @title Setup information capturing for list of function
#'
#' @description This function is respinsible for setting up capturing for functions
#'
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @param package name of the package
#' @param verbose if to print additional status information
#' @seealso Decorate
#' @export
setup_capture <- function(flist, package, verbose = testr_options("verbose")) {
    old <- testr_options("capture.arguments")
    if (old)
        testr_options("capture.arguments", FALSE)
    for (func in flist)
        if (eligible_capture(func))
            # TODO perhaps we want to put base in because these are builtins?
            decorate(func, NA_character_, verbose)
    if (old)
        testr_options("capture.arguments", TRUE)
}

#' @title Check if function is eligible for wrapping to capture arguments and return values
#'
#' @description This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in setup_capture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso setup_capture
eligible_capture <- function(func){
    return (!length(utils::getAnywhere(func)$objs) == 0
            && class(utils::getAnywhere(func)[1]) == "function"
            && !func %in% blacklist
            && !func %in% operators
            && !func %in% keywords
            && !func %in% sys
            && !func %in% env
            && !func %in% primitive_generics_fails)
}


#' @title Clear decoration
#'
#' @description Clear anything previously decorate
#' @param verbose if to print additional debugging information. Default \code{TRUE}.
#' @seealso undecorate
#' @export
clear_decoration <- function(verbose) {
    for (fname in ls(.decorated, all.names = TRUE))
        undecorate(fname, verbose = verbose)
}
