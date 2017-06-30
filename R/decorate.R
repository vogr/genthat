#' @title Decorates functions in an environment
#'
#' @param envir an environment that shall be decorated
#'
#' @description Decorates all symbols form the given environment for which `is.function` is `TRUE`.
#' @export
#'
decorate_environment <- function(envir) {
    stopifnot(is.environment(envir))

    # TODO: is the all.names actually correct?
    names <- ls(envir, all.names=TRUE)
    vals <- lapply(names, get, envir=envir)
    names(vals) <- names

    funs <- filter(vals, is.function)
    funs <- filter_not(funs, is.primitive)

    invisible(decorate_and_replace(funs))
}

#' @title Decorates given functions
#'
#' @param ... the functions that shall be decorated
#'
#' @description Given functions will be decorated in their defining environment.
#' @export
#'
decorate_functions <- function(...) {
    dots <- substitute(list(...))[-1]
    names <- sapply(dots, deparse)
    funs <- list(...)
    names(funs) <- names

    invisible(decorate_and_replace(funs))
}

#' @title Decorates given function
#'
#' @param fun the function that shall be decorated
#' @param name name of the function
#'
#' @description Given function will be decorated in its defining environment.
#' @export
#'
decorate_function <- function(fun, name=substitute(fun)) {
    stopifnot(is.function(fun))
    name <- as.character(name)

    fs <- list()
    fs[[name]] <- fun

    invisible(decorate_and_replace(fs)[[name]])
}

#' @title Resets decorated function back to its original
#'
#' @description Reverts decorated functions back to their state they were before calling `decorate_functions`.
#' @export
#'
reset_functions <- function(...) {
    dots <- substitute(list(...))[-1]
    names <- sapply(dots, deparse)

    # TODO: define contract

    # TODO: if empty - reset all decorated functions

    lapply(names, reset_function)
}

#' @title Checks whether a function has been already decorated
#' @description Checks whether the given function has been decorated by `decorate_functions` call.
#'
#' @param fun function value
#' @export
#'
is_decorated <- function(fun) {
    stopifnot(is.function(fun))

    isTRUE(attr(fun, "genthat"))
}

do_decorate_function <- function(name, pkg, fun,
                             .entry=substitute(genthat:::on_function_entry),
                             .exit=substitute(genthat:::on_function_exit)) {
    stopifnot(is.character(name))
    stopifnot(is.function(fun))

    if (is_decorated(fun)) {
        return(fun)
    }

    # the `retv <- BODY` is OK because in the case of multiple expressions, it will be surrounded with `{}`.
    new_fun <- create_function(
        params=formals(fun),
        body=substitute({
            `__genthat_cache` <- .Internal(get("cache", .Internal(getRegisteredNamespace("genthat")), "any", FALSE))
            if (.Internal(get("tracing", `__genthat_cache`, "any", FALSE)) == TRUE) {
                on.exit(.Internal(assign("tracing", TRUE, `__genthat_cache`, FALSE)))

                .Internal(assign("tracing", FALSE, `__genthat_cache`, FALSE))
                `__trace_index` <- ENTRY(name = NAME, pkg = PKG, args = as.list(match.call())[-1], env = parent.frame())
                .Internal(assign("tracing", TRUE, `__genthat_cache`, FALSE))

                retv <- BODY

                .Internal(assign("tracing", FALSE, `__genthat_cache`, FALSE))
                EXIT(index = `__trace_index`, retv = retv)
                .Internal(assign("tracing", TRUE, `__genthat_cache`, FALSE))

                retv
            } else {
                BODY
            }
        }, list(
            NAME=name,
            PKG=pkg,
            BODY=body(fun),
            ENTRY=.entry,
            EXIT=.exit
        )),
        env=environment(fun),
        attributes=list(genthat=TRUE))

    new_fun
}

decorate_and_replace <- function(funs) {
    xs <- zip(name=names(funs), fun=funs)

    lapply(xs, function(x) {
        tryCatch({
            decorate_and_replace_one(x$name, x$fun)
        }, error=function(e) {
            warning("Unable to decorate `", x$name, "`: ", e$message)
        })
    })
}

decorate_and_replace_one <- function(name, fun) {
    stopifnot(is.character(name) && length(name) == 1)
    stopifnot(is.function(fun))

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

    new_fun <- do_decorate_function(name=name, pkg=pkg, fun=fun)
    replacement <- create_replacement(
        name=name,
        env=environment(fun),
        orig_fun=create_duplicate(fun),
        fun=fun,
        new_fun=new_fun
    )

    add_replacement(replacement)
    reassign_function(fun, new_fun)

    invisible(new_fun)
}

reset_function <- function(name) {
    # allows both a name reference or name as string
    if (!is.character(name)) {
        name <- substitute(name)
        if (is.name(name)) {
            name <- as.character(name)
        }
    }

    stopifnot(is.character(name))

    r <- remove_replacement(name)

    if (is_debug_enabled()) {
        message("Resetting decorated function: ", name)
    }

    reassign_function(r$fun, r$orig_fun)
    invisible(r$orig_fun)
}

reset_all_functions <- function() {
    lapply(get_replacements(), function(x) {
        reset_function(x$name)
    })
}

get_function_package_name <- function(fun) {
    stopifnot(is.function(fun))

    env <- environment(fun)
    if (identical(env, .BaseNamespaceEnv)) {
        return("base")
    }

    pkg_name <- get_package_name(env)

    if (is_empty_str(pkg_name) || identical(env, globalenv())) {
        NULL
    } else {
        pkg_name
    }
}

#' @export
#'
get_replacements <- function() {
    cache$replacements
}

create_replacement <- function(name, env, orig_fun, fun, new_fun) {
    stopifnot(is.character(name))
    stopifnot(is.environment(env))
    stopifnot(is.function(orig_fun))
    stopifnot(is.function(fun))
    stopifnot(is.function(new_fun))

    structure(
        list(
            name=name,
            env=env,
            orig_fun=orig_fun,
            fun=fun,
            new_fun=new_fun
        ),
        class="genthat_replacement")
}

#' @importFrom methods is
add_replacement <- function(r) {
    stopifnot(methods::is(r, "genthat_replacement"))

    if (r$name %in% names(cache$replacements)) {
        stop(r$name, ": already exists in the replacement table")
    }

    cache$replacements[[r$name]] <- r
}


get_replacement <- function(name) {
    stopifnot(is.character(name))

    r <- cache$replacements[[name]]

    if (is.null(r)) {
        stop(name, ": does not exist in the replacement table")
    }

    r
}

remove_replacement <- function(name) {
    r <- get_replacement(name)
    rm(list=name, envir=cache$replacements)
    r
}

reset_replacements <- function() {
    if (length(get_replacements()) != 0) {
        warning("There are still decorated functions")
    }

    cache$replacements <- new.env(parent=emptyenv())
}
