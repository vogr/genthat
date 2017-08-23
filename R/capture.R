# Default function entry decorator.
# Creates the trace record and stores it into the trace vector.
record_trace <- function(name, pkg=NULL, args, retv, error,
                        env=parent.frame(), tracer=get_tracer()) {

    # TODO: (performance) all this makes sense only if there are symbols anywhere in args
    # get callee globals (free variables) that we need to capture
    # we do that by abusing the extract closure
    # TODO: will this help us with promises?

    trace <- tryCatch({
        callee <- as.function(c(alist(), as.call(c(quote(`{`), args))), envir=env)
        globals <- as.list(environment(extract_closure(callee)), all.names=TRUE)
        globals <- lapply(globals, create_duplicate)

        create_trace(name, pkg, args=args, globals=globals, retv=retv, error=error)
    }, error=function(e) {
        message(paste0("GENTHAT: Error during recording: ", pkg, ":::", name, ": ", e$message))

        create_trace(name, pkg, args=args, failure=e)
    }, warning=function(e) {
        message(paste0("GENTHAT: Warning during recording: ", pkg, ":::", name, ": ", e$message))

        create_trace(name, pkg, args=args, failure=e)
    })

    store_trace(tracer, trace)
}

find_symbol_env <- function(name, env=parent.frame()) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.environment(env))

    if (identical(env, emptyenv())) {
        NULL
    } else if (exists(name, env, inherits=FALSE)) {
        env
    } else {
        find_symbol_env(name, parent.env(env))
    }
}

get_symbol_names <- function(exprs) {
    stopifnot(is.list(exprs))

    unique(unlist(lapply(exprs, all.names)))
}

is_base_env <- function(env) {
    isBaseNamespace(env) || identical(env, baseenv())
}

get_symbol_values <- function(names, env=parent.frame(), include_base_symbols=FALSE) {
    if (length(names) == 0) {
        return(list())
    }

    stopifnot(is.character(names))
    stopifnot(is.environment(env))

    envs <- lapply(names, find_symbol_env, env=env)
    vars <- zip(name=names, env=envs)
    vars <- filter(vars, function(x) {
        if (is.null(x$env)) {
            # this can easily happen in the calls like dplyr::filter(data, x >
            # 0) since the `x` referes to an `x` in `data` not in environment
            # this we cannot know and thus we have to ignore it
            FALSE
        } else {
            TRUE
        }
    })

    if (!include_base_symbols) {
        vars <- filter_not(vars, function(x) is_base_env(x$env))
    }

    # get reference to package environments
    lapply(vars, function(x) {
        pkg <- get_package_name(x$env)
        if (!is.null(pkg)) {
            # create reference
            substitute(PKG:::NAME, list(PKG=as.name(pkg), NAME=as.name(x$name)))
        } else {
            # get the value
            get(x$name, envir=x$env, inherits=FALSE)
        }
    })
}

#' @importFrom codetools findGlobals
extract_closure <- function(fun, name=substitute(fun), .visited=list()) {
    stopifnot(is.closure(fun))

    if (isTRUE(attr(fun, "genthat_extracted_closure"))) {
        return(fun)
    }

    env <- environment(fun)
    if (identical(env, baseenv())
        || identical(env, .BaseNamespaceEnv)
        || identical(env, emptyenv())
        || environment_name(env) != "") {

        copy <- fun

        attr(copy, "genthat_extracted_closure") <- TRUE

        return(copy)
    }

    if (is.name(name) || is.character(name)) {
        .visited <- bag_add(.visited, name, env)
    }

    names <- codetools::findGlobals(fun)
    new_fun <-
        if (length(names) == 0) {
            copy <- fun
            environment(copy) <- new.env(parent=baseenv())
            copy
        } else {
            needs_link <- length(filter(names, bag_contains_value, bag=.visited, value=env)) > 0
            unknowns <- filter_not(names, bag_contains_value, bag=.visited, value=env)
            vals <- get_symbol_values(unknowns, env)

            vars <- filter_not(vals, is.closure)
            funs <- filter(vals, is.closure)

            # mark variables as visited so the consecutive serialization will not consider them
            .visited <- reduce(names(vars), function(b, x) b <- bag_add(b, x, env), init=.visited)

            # now we can process functions
            funs <- zip(name=names(funs), val=funs)
            funs <- lapply(funs, function(x) extract_closure(x$val, x$name, .visited))

            globals <- c(vars, funs)
            new_env <-
                if (length(globals) == 0) {
                    new.env(parent=baseenv())
                } else {
                    e <- list2env(globals, parent=baseenv())
                    # we should only link the environments in the case it is necessary
                    # i.e. any of the global functions need access to this environment
                    # the idea is demonstrated in the test-capture.R
                    link_environments(e, .fun_filter=function(x) is.local_closure(x) && isTRUE(attr(x, "genthat_needs_link")))
                    e
                }

            f <- as.function(c(formals(fun), body(fun)), envir=new_env)
            if (needs_link) {
                attr(f, "genthat_needs_link") <- TRUE
            }
            f
        }

    attr(new_fun, "genthat_extracted_closure") <- TRUE
    new_fun
}
