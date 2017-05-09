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

    # TODO: call decorate_functions instead
    decorate_and_replace(funs)
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

    # TODO: define contract

    names(funs) <- names

    decorate_and_replace(funs)
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

    decorate_and_replace(fs)[[name]]
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

do_decorate_function <- function(name, fun,
                             .call_id_gen=substitute(genthat:::get_next_call_id),
                             .entry=substitute(genthat:::on_function_entry),
                             .exit=substitute(genthat:::on_function_exit)) {
    stopifnot(is.character(name))
    stopifnot(is.function(fun))

    if (is_decorated(fun)) {
        return(fun)
    }

    new_fun <- create_function(
        params=formals(fun),
        body=substitute({
            `__call_id` <- CALL_ID_GEN()
            ENTRY(call_id=`__call_id`, name=NAME, args=as.list(match.call())[-1])
            retv <- BODY
            EXIT(call_id=`__call_id`, retv=retv)
            retv
        }, list(NAME=name, BODY=body(fun), CALL_ID_GEN=.call_id_gen, ENTRY=.entry, EXIT=.exit)),
        env=environment(fun),
        attributes=list(genthat=TRUE))

    new_fun
}

## funs a list of name, fun pairs for all functions to be decorated and replaced
# TODO: rewrite to one-by-one so we can rollback in the case there is an error on the way
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

        if (is_decorated(x$fun)) {
            stop(x$name, ": is already decorated")
        }

        name <- get_function_fqn(x$name, x$fun)

        if (is_debug_enabled()) {
            message("Tracing: ", name)
        }

        create_replacement(
            name=name,
            env=environment(x$fun),
            orig_fun=create_duplicate(x$fun),
            fun=x$fun,
            new_fun=do_decorate_function(name=name, fun=x$fun)
        )
    })

    lapply(replacements, function(x) reassign_function(x$fun, x$new_fun))
    lapply(replacements, add_replacement)

    lapply(replacements, `[[`, "new_fun")
}

reset_function <- function(name) {
    stopifnot(is.character(name))

    r <- remove_replacement(name)

    if (is_debug_enabled()) {
        message("Resetting: ", name)
    }

    reassign_function(r$fun, r$orig_fun)
}

get_function_fqn <- function(name, fun) {
    stopifnot(is.character(name))
    stopifnot(is.function(fun))

    env <- environment(fun)
    pkg_name <- get_package_name(env)

    names <- split_function_name(name)
    if (is_empty_str(names$package)) {
        if (is_empty_str(pkg_name) || identical(env, globalenv())) {
            names$name
        } else {
            paste0(pkg_name, ":::", names$name)
        }
    } else {
        # TODO: test
        if (nchar(pkg_name) > 0 && names$package != pkg_name) {
            warning("Mismatch with name and environment. Function ", name, " is defined in ", pkg_name)
        }
        name
    }
}

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
