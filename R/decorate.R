#' @title Decorates functions in an environment
#'
#' @param envir an environment that shall be decorated or a character scalar
#'     that will be resolved using `as.environment("package:....")`. In the
#'     latter case, the package will also be loaded in the case it has not been
#'     loaded yet.
#'
#' @description Decorates all symbols form the given environment for which
#'     `is.function` is `TRUE`.
#' @export
#'
decorate_environment <- function(env, type=c("exported", "private", "all"), exclude=character(), ...) {
    env <- resolve_env(env)
    stopifnot(is.environment(env))

    funs <- get_functions_from_env(env, type=type)
    funs <- funs[!(names(funs) %in% exclude)]

    res <- lapply(names(funs), function(name) {
        tryCatch({
            fun <- funs[[name]]
            decorate_function(name, ..., env=env)
            fun
        }, error=function(e) {
            e$message
        })
    })

    names(res) <- names(funs)

    invisible(res)
}

#' Decorates given functions
#'
#' Given function will be decorated in their defining environment.
#'
#' Primitive and S3 generic functions (the ones calling \code{UseMethod}) cannot
#' be traced. If a function has been already decorated, it will be reset and
#' decorated again.
#'
#' @param fun the function to decorate as a function reference or as function
#'     name as string, in which case it will be looked up in the given \code{env}
#' @param onentry the function to be called on the \code{fun} entry
#' @param onexit the function to be called on the \code{fun} exit
#' @param onerror the function to be called on the \code{fun} error
#' @param env if \code{fun} is not given, resolve \code{name} in the given
#'     \code{env} and up
#'
#'
#'
#' @export
#'
decorate_function <- function(fun, onentry=NULL, onexit=NULL, onerror=NULL,
                              env=parent.frame()) {

    if (is_tracing_enabled()) {
        disable_tracing()
        on.exit(enable_tracing())
    }

    resolved_fun <- resolve_function(fun, substitute(fun), env)
    fun <- resolved_fun$fun
    fqn <- resolved_fun$fqn
    name <- resolved_fun$name
    package <- resolved_fun$package
    idx <- get_decoration_idx(fun, name, env)

    if (is.primitive(fun)) {
        stop(fqn, ": is a primitive function")
    }

    if (is_s3_generic(fun)) {
        stop(fqn, ": is a S3 generic function")
    }

    if (exists(idx, envir=.decorations)) {
        reset_function(fqn, env=env)
    }

    log_debug("Decorating function: ", name)

    ofun <- create_duplicate(fun)
    nfun <- create_decorated_function(
        fun=fun,
        name=name,
        package=package,
        onentry=onentry,
        onexit=onexit,
        onerror=onerror
    )

    reassign_function(fun, nfun)

    assign(idx, list(fqn=fqn, fun=fun, ofun=ofun), envir=.decorations)

    invisible(fqn)
}


#' Resets decorated function back to its original
#'
#' Reverts decorated function back to their state they were before calling
#' \code{decorate_function}.
#'
#' @param fun function or function name
#'
#' @export
#'
reset_function <- function(fun, env=parent.frame()) {
    if (!is_decorated(fun, env)) {
        warning(substitute(fun), ": is not decorated")
        invisible(return(NULL))
    } else {
        idx <- get_decoration_idx(fun, substitute(fun), env)
        do_reset_function(idx)
    }
}

do_reset_function <- function(idx) {
    decoration <- get(idx, envir=.decorations)
    stopifnot(!is.null(decoration))

    log_debug("Resetting decorated function: ", decoration$fqn)

    reassign_function(decoration$fun, decoration$ofun)
    rm(list=idx, envir=.decorations)

    invisible(decoration$fqn)
}

#' @export
#'
reset_functions <- function() {
    for (x in ls(envir=.decorations)) {
        do_reset_function(x)
    }
}

#' @export
#'
is_decorated <- function(fun, env=parent.frame()) {
    exists(get_decoration_idx(fun, substitute(fun), env), envir=.decorations)
}

#' Returns decorated functions
#'
#' Return a list of the decorated functions.
#'
#' @return a list where names are fully-qualified function names and values are
#'     the decorated functions
#'
#' @export
#'
get_decorations <- function() {
    vals <- as.list(.decorations)

    names <- lapply(vals, `[[`, "fqn")
    funs <- lapply(vals, `[[`, "fun")
    names(funs) <- names
    funs
}

get_decoration_idx <- function(fun, name, env=parent.frame()) {
    if (!is.function(fun)) {
        resolved_fun <- resolve_function(fun, name, env)
        fun <- resolved_fun$fun
    }

    sexp_address(fun)
}
