#' @export
#'
create_decorator <- function(method="on.exit") {
    fun <- if (is.character(method)) {
        method <- match.arg(arg=method, choices=c("onentry", "onexit", "on.exit", "onboth", "trycatch", "count-entry", "count-exit", "noop"), several.ok=FALSE)

        switch(method,
            onentry=decorate_with_onentry,
            onexit=decorate_with_onexit,
            on.exit=decorate_with_on.exit,
            onboth=decorate_with_onboth,
            trycatch=decorate_with_trycatch,
            `count-entry`=decorate_with_count_entry,
            `count-exit`=decorate_with_count_exit,
            noop=decorate_with_noop
        )
    } else if (is.function(method)) {
        method
    } else {
        stop("Unknown type of method: ", typeof(method))
    }

    structure(list(
        method=fun,
        decorations=new.env(parent=emptyenv(), hash=TRUE)
    ), class="decorator")
}

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
decorate_environment <- function(envir, decorator=get_decorator(),
                                 record_fun=substitute(genthat:::record_trace),
                                 exclude=character()) {
    stopifnot(is.decorator(decorator))

    if (is.character(envir)) {
        stopifnot(length(envir) == 1)
        envir <- getNamespace(envir)
    }

    stopifnot(is.environment(envir))

    # TODO: is the all.names actually correct?
    names <- ls(envir, all.names=TRUE)
    vals <- lapply(names, get, envir=envir)
    names(vals) <- names

    funs <- filter(vals, is.function)
    funs <- filter_not(funs, is.primitive)
    funs <- filter_not(funs, is_s3_generic)
    funs <- funs[!(names(funs) %in% exclude)]

    invisible(decorate_functions(funs, decorator=decorator, record_fun=record_fun))
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
                              decorator=get_decorator(),
                              record_fun=substitute(genthat:::record_trace)) {
    if (is_tracing_enabled()) {
        disable_tracing()
        on.exit(enable_tracing())
    }

    stopifnot(is.decorator(decorator))

    xs <- resolve_decorating_fun_args(..., in_env=in_env)

    decorations <- lapply(xs, function(x) {
        tryCatch({
            decorate_function(decorator, fun=x$fun, name=x$name, record_fun=record_fun)
        }, error=function(e) {
            warning("Unable to decorate `", x$name, "`: ", e$message)
        })
    })

    invisible(decorations)
}

decorate_function <- function(decorator, fun, name, record_fun) {
    stopifnot(is.decorator(decorator))

    # TODO: test
    if (!is.function(fun)) {
        stop(name, ": is not a function")
    }

    if (is.primitive(fun)) {
        stop(name, ": is a primitive function")
    }

    if (is_s3_generic(fun)) {
        stop(name, ": is a S3 generic function")
    }

    # TODO: test
    if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
        stop(fun, ": does not have name")
    }

    if (is_decorated(fun)) {
        reset_function(decorator, fun, name)
    }

    if (is_debug_enabled()) {
        message("Decorating function: ", name)
    }

    pkg <- get_function_package_name(fun, name)
    new_fun <- decorator$method(fun, name, pkg, record_fun)

    if (is.null(attr(new_fun, "__genthat_original_fun"))) {
        attr(new_fun, "__genthat_original_fun") <- create_duplicate(fun)
    }

    reassign_function(fun, new_fun)

    fqn <- paste0(pkg, ":::", name)
    assign(fqn, fun, envir=decorator$decorations)

    invisible(fun)
}

#' @title Resets decorated function back to its original
#'
#' @description Reverts decorated functions back to their state they were before calling `decorate_functions`.
#' @export
#'
reset_functions <- function(..., in_env=parent.frame(), decorator=get_decorator()) {
    stopifnot(is.decorator(decorator))

    xs <- resolve_decorating_fun_args(..., in_env=in_env)

    resets <- lapply(xs, function(x) {
        tryCatch({
            reset_function(decorator, fun=x$fun, name=x$name)
        }, error=function(e) {
            stop("Unable to reset `", x$name, "`: ", e$message)
        })
    })

    invisible(resets)
}

reset_function <- function(decorator, fun, name) {
    stopifnot(is.decorator(decorator))
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
    reassign_function(fun, orig_fun, keep_only_new_attributes=TRUE)
    attr(fun, "__genthat_original_fun") <- NULL

    pkg <- get_function_package_name(fun, name)
    fqn <- paste0(pkg, ":::", name)
    if (exists(fqn, envir=decorator$decorations)) {
        rm(list=fqn, envir=decorator$decorations)
    }

    invisible(fun)
}

#' @export
#'
is_decorated <- function(fun) {
    stopifnot(is.function(fun))

    # TODO: make it a constant
    !is.null(attr(fun, "__genthat_original_fun"))
}

#' @export
#'
set_decorator <- function(decorator) {
    stopifnot(is.decorator(decorator))

    options(genthat.decorator=decorator)

    invisible(decorator)
}

#' @export
#'
get_decorator <- function() {
    decorator <- getOption("genthat.decorator")

    if (is.null(decorator)) {
        decorator <- create_decorator()
        set_decorator(decorator)
    }

    decorator
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
        # TODO: allow fqn 
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

is.decorator <- function(x) {
    inherits(x, "decorator")
}

is_s3_generic <- function(fun) {
    stopifnot(is.function(fun))

    globals <- codetools::findGlobals(fun, merge = FALSE)$functions
    any(globals == "UseMethod")
}
