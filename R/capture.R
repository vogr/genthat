#' @title Undecorate all functions.
#'
#' @export
#'
undecorate_all <- function() {

}

#' @title Tells you whether the given function is a result of genthat's function decoration.
#'
#' @param fun function value
#' @export
#'
is_decorated <- function(fun) {
    stopifnot(is.function(fun))
    
    isTRUE(attr(fun, "genthat"))
}

#' @export
decorate_environment <- function(envir) {
    stopifnot(is.environment(envir))

    # TODO: is the all.names actually correct?
    names <- ls(envir, all.names=TRUE)
    vals <- lapply(names, get, envir=envir)
    names(vals) <- names
    
    funs <- filter(vals, is.function)

    decorate_and_replace(funs)
}

#' @export
decorate_functions <- function(...) {
    dots <- substitute(list(...))[-1]
    names <- sapply(dots, deparse)
    funs <- list(...)
    
    names(funs) <- names
    
    decorate_and_replace(funs)
}

get_function_name_str <- function(name, fun) {
    env <- environment(fun)
    pkg_name <- environmentName(env)

    names <- get_function_name(name)
    if (length(names) == 1) {
        paste0(pkg_name, ":::", names)
    } else {
        # TODO: test
        if (nchar(pkg_name) > 0 && names[1] != pkg_name) {
            warning("Mismatch with name and environment. Function ", name, " is defined in ", pkg_name)
        }
        name
    }
}

##' @param funs a list of name, fun pairs for all functions to be decorated and replaced
##' @return 
##' @author Filip Krikava
decorate_and_replace <- function(funs) {
    stopifnot(is.list(funs))
    stopifnot(!is.null(names(funs)))
        
    funs <- zip(name=names(funs), fun=funs)
    
    replacements <- lapply(funs, function(x) {
        # TODO: test
        if (!is.function(x$fun)) {
            stop(x$name, ": not a function")
        }

        # TODO: test
        if (is_empty_str(x$name)) {
            stop(x$fun, ": does not have name")
        }

        # TODO: inline
        name <- get_function_name_str(x$name, x$fun)
        d <- create_duplicate(x$fun)
        create_replacement(
            name=name,
            env=environment(x$fun),
            # TODO: useDynlib
            orig_fun=d,
            fun=x$fun,
            new_fun=decorate_function(name=name, fun=x$fun)
        )
    })

    lapply(replacements, do.call, what=replace)
    lapply(replacements, add_replacement)
    
    lapply(replacements, `[[`, "new_fun")
}

decorate_function <- function(name, fun,
                             .entry=substitute(genthat:::on_function_entry),
                             .exit=substitute(genthat:::on_function_exit)) {
    stopifnot(is.character(name))
    stopifnot(is.function(fun))

    if (is_decorated(fun)) {
        return(fun)
    }

    # TODO: should we also wrap the whole function in a try/catch
    # and record erros/warnings?
    new_fun <- add_function_hook(fun, substitute({
        `__call_id` <- genthat:::get_next_call_id() 
        if (ENTRY(call_id=`__call_id`, name=NAME, args=as.list(match.call())[-1])) {                    
            on.exit(EXIT(call_id=`__call_id`, retv=returnValue()))
        }
    }, list(NAME=name, ENTRY=.entry, EXIT=.exit)))

    # replace environment so the new function can actually run
    environment(new_fun) <- environment(fun)
    # tag so we do not create decorations of decorations
    attr(new_fun, "genthat") <- TRUE
    new_fun
}

# TODO: move to replacements

#' @export
reset_functions <- function(...) {
    dots <- substitute(list(...))[-1]
    names <- sapply(dots, deparse)
    
    lapply(names, reset_function)
}

reset_function <- function(name) {
    r <- remove_replacement(name)

    replace(name=r$name, env=r$env, fun=r$fun, new_fun=r$orig_fun)
}

replace <- function(name, env, orig_fun, fun, new_fun) {
    stopifnot(is.character(name))
    stopifnot(is.environment(env))
    stopifnot(is.function(fun))
    stopifnot(is.function(new_fun))

    # TODO: debug option
    message("Replacing: ", name)

    fun_name <- strsplit(name, split=":")[[1]]
    fun_name <- fun_name[length(fun_name)]

    # TODO: clean the function interface
    # TODO: us ethe create function
    .Call("genthat_reassign_function", PACKAGE="genthat", as.name(fun_name), env, fun, new_fun)

    invisible(NULL)
}

add_function_hook <- function(fun, hook) {
    stopifnot(is.function(fun))
    stopifnot(is.language(hook))
    
    # TODO: get rid of the ifs
    body <- substitute({
        HOOK
        BODY
    }, list(HOOK=hook, BODY=body(fun)))

    eval(call("function", as.pairlist(formals(fun)), body), parent.frame())
}

on_function_entry <- function(call_id, name, args) {
    parent <- new.env(parent=parent.frame(2)) # 2 means 2 generations back

    # TODO: different name - not cache
    if (!cache$capture_arguments) {
        return(FALSE)
    }

    # TODO: no need for function(x)
    args_vals <- lapply(args, function(x) eval(x, envir=parent))

    tryCatch({
        cache$capture_arguments <- FALSE

        args_str <- lapply(args, serialize_value)
        set_call_trace(call_id, create_trace(name, args_str))
        
        return(TRUE)
    }, error=function(e) {
        set_call_trace(call_id, create_trace_error(name, args_vals, format(e)))
        return(FALSE)
    }, finally={
        cache$capture_arguments <- TRUE
    })
}

on_function_exit <- function(call_id, retv) {
    if (!cache$capture_arguments) {
        return(NULL)
    }
    
    trace <- get_call_trace(call_id)

    if (is.null(trace)) {
        warning("Unknown call ID: ", call_id)
        return(NULL)
    }

    if (!("genthat_trace" %in% class(trace))) {
        return(NULL)
    }
    
    cache$capture_arguments <- FALSE

    tryCatch({                    
        trace$retv <- serialize_value(retv)        
    }, error=function(e) {
        trace <- create_trace_error(trace$fun, trace$args, format(e))
    })
    
    set_call_trace(call_id, trace)
    
    cache$capture_arguments <- TRUE
}

