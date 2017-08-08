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
decorate_environment <- function(envir) {
    if (is.character(envir)) {
        stopifnot(length(envir) == 1)
        library(envir, character.only=TRUE)
        envir <- as.environment(paste0("package:", envir))
    }

    stopifnot(is.environment(envir))

    # TODO: is the all.names actually correct?
    names <- ls(envir, all.names=TRUE)
    vals <- lapply(names, get, envir=envir)
    names(vals) <- names

    funs <- filter(vals, is.function)
    funs <- filter_not(funs, is.primitive)

    invisible(decorate_functions(funs))
}

#' @title Decorates given functions
#'
#' @param ... the functions that shall be decorated
#'
#' decorate_functions(list(ls, ...))
#' decorate_functions(list("ls", ...))
#' decorate_functions(c("ls", ...))
#' decorate_functions(ls, ...)
#' decorate_functions("ls", ...)
#'
#' @description Given functions will be decorated in their defining environment.
#' @export
#'
decorate_functions <- function(..., in_env=parent.frame(),
                              .recorder=substitute(genthat:::record_trace)) {
    xs <- resolve_decorating_fun_args(..., in_env=in_env)

    decorations <- lapply(xs, function(x) {
        tryCatch({
            decorate_function(x$fun, x$name, .recorder)
        }, error=function(e) {
            warning("Unable to decorate `", x$name, "`: ", e$message)
        })
    })

    invisible(decorations)
}

decorate_function <- function(fun, name, .recorder) {
    stopifnot(is.function(fun))
    stopifnot(is.character(name) && length(name) == 1)

    # TODO: test
    if (!is.function(fun)) {
        stop(name, ": is not a function")
    }

    if (is.primitive(fun)) {
        stop(name, ": is a primitive function")
    }

    # TODO: test
    if (is_empty_str(name)) {
        stop(fun, ": does not have name")
    }

    if (is_decorated(fun)) {
        fun
    }

    pkg <- get_function_package_name(fun)

    if (is_debug_enabled()) {
        message("Decorating function: ", name)
    }

    new_fun <- do_decorate_function(name=name, pkg=pkg, fun=fun, .recorder=.recorder)

    reassign_function(fun, new_fun)

    invisible(fun)
}


do_decorate_function <- function(name, pkg, fun, .recorder) {
    stopifnot(is.character(name))
    stopifnot(is.function(fun))

    if (is_decorated(fun)) {
        return(fun)
    }

    # The `retv <- BODY` is OK because in the case of multiple expressions, it will be surrounded with `{}`.
    # The as.list(match.call(...)) will force the arguments.
    # TODO: is that OK?
    # - guess so if it fails, it would fail when we create the test
    # - need to find the exact cases...
    new_fun <- create_function(
        params=formals(fun),
        body=substitute({
            if (genthat::is_tracing_enabled()) {
                on.exit(genthat::enable_tracing())

                genthat::disable_tracing()

                frame <- new.env(parent=parent.frame())
                frame$original <- attr(sys.function(), "__genthat_original_fun")

                call <- sys.call()
                call[[1]] <- as.name("original")

                tryCatch({
                    genthat::enable_tracing()
                    retv <- eval(call, envir=frame)
                    genthat::disable_tracing()

                    RECORDER(
                        name=NAME,
                        pkg=PKG,
                        args=as.list(match.call())[-1],
                        retv=retv,
                        env=parent.frame()
                    )

                    retv
                },  error=function(e) {
                    genthat::disable_tracing()

                    depth <- getOption("genthat.tryCatchDepth")
                    env <- parent.frame(depth + 2)
                    match_call <- match.call(
                        definition=sys.function(-depth - 1),
                        call=sys.call(-depth - 1),
                        envir=env
                    )

                    RECORDER(
                        name=NAME,
                        pkg=PKG,
                        args=as.list(match_call)[-1],
                        error=e,
                        env=env
                    )

                    stop(e)
                })
            } else {
                call <- sys.call()
                call[[1]] <- attr(sys.function(), "__genthat_original_fun")
                eval(call)
            }
        }, list(
            NAME=name,
            PKG=pkg,
            RECORDER=.recorder
        )),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        ))

    invisible(new_fun)
}

#' @title Resets decorated function back to its original
#'
#' @description Reverts decorated functions back to their state they were before calling `decorate_functions`.
#' @export
#'
reset_functions <- function(..., in_env=parent.frame()) {
    xs <- resolve_decorating_fun_args(..., in_env=in_env)

    resets <- lapply(xs, function(x) {
        tryCatch({
            reset_function(x$fun, x$name)
        }, error=function(e) {
            warning("Unable to decorate `", x$name, "`: ", e$message)
        })
    })

    invisible(resets)
}

reset_function <- function(fun, name) {
    stopifnot(is.function(fun))
    stopifnot(is.character(name) && length(name) == 1)

    if (!is_decorated(fun)) {
        warning("Function ", name, " is not decorated")
        return(fun)
    }

    if (is_debug_enabled()) {
        message("Resetting decorated function: ", name)
    }

    orig_fun <- attr(fun, "__genthat_original_fun")
    reassign_function(fun, orig_fun)
    invisible(fun)
}

#' @title Checks whether a function has been already decorated
#' @description Checks whether the given function has been decorated by `decorate_functions` call.
#'
#' @param fun function value
#' @export
#'
is_decorated <- function(fun) {
    stopifnot(is.function(fun))

    # TODO: make it a constant
    !is.null(attr(fun, "__genthat_original_fun"))
}

resolve_decorating_fun_args <- function(..., in_env=parent.frame()) {
    dots <- list(...)

    if (length(dots) == 1 && is.list(dots[[1]]) && !is.null(names(dots[[1]]))) {
        names <- names(dots[[1]])
        funs <- dots[[1]]
    } else if (length(dots) == 1 && (is.character(dots[[1]]) || is.name(dots[[1]]))) {
        names <- dots[[1]]
        funs <- lapply(names, resolve_function, in_env=in_env)
    } else {
        xs <- substitute(list(...))[-1]
        xs <- sapply(xs, deparse)
        names <- lapply(zip(v=dots, s=xs), function(x) {
            if (is.character(x$v) || is.name(x$v)) x$v else x$s
        })
        funs <- lapply(names, resolve_function, in_env=in_env)
    }

    ret <- zip(name=names, fun=funs)
    names(ret) <- names
    ret
}
