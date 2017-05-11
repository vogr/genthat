on_function_entry <- function(call_id, name, args, fun=sys.function(-1), env=parent.frame()) {
    stopifnot(is.numeric(call_id))
    stopifnot(is.character(name))
    stopifnot(is.list(args))

    # get callee globals (free variables) that we need to capture
    # we do that by abusing the extract closure
    dummy <- as.function(c(name_unnamed_args(args), 0), envir=env)

    callee <- extract_closure(dummy)$globals

    set_call_trace(call_id, create_trace(name, args, callee))
}

#' @importFrom methods is
on_function_exit <- function(call_id, retv) {
    stopifnot(is.numeric(call_id))

    trace <- get_call_trace(call_id)
    stopifnot(methods::is(trace, "genthat_trace_entry"))

    cache$capture_arguments <- FALSE

    tryCatch({
        trace <- create_trace(trace$fun, args=trace$args, globals=trace$globals, retv=retv)
    }, error=function(e) {
        trace <- create_trace_error(trace$fun, trace$args, format(e))
    })

    set_call_trace(call_id, trace)

    cache$capture_arguments <- TRUE
}

name_unnamed_args <- function(args) {
    stopifnot(is.list(args))

    args_names <- names(args)

    if (length(args) == 0) {
        args
    } else if (is.null(args_names)) {
        names(args) <- paste0("___", 1:length(args))
        args
    } else {
        args_names[args_names == ""] <- paste0("___", 1:length(args_names[args_names == ""])) 
        names(args) <- args_names
        args
    }
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
            warning("Symbol ", x$name, " could not be found in ", environmentName(env))
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

extract_closure <- function(fun, name=substitute(fun), .visited=list()) {
    stopifnot(is.function(fun))

    env <- environment(fun)

    if (is.name(name) || is.character(name)) {
        .visited <- bag_add(.visited, name, env)
    }

    names <- codetools::findGlobals(fun)
    unknowns <- filter_not(names, bag_contains_value, bag=.visited, value=env)
    vals <- get_symbol_values(unknowns, env)

    vars <- filter_not(vals, is.function)
    funs <- filter(vals, is.function)

    # mark variables as visited so the consecutive serialization will not consider them
    .visited <- reduce(names(vars), function(b, x) b <- bag_add(b, x, env), init=.visited)

    # now we can process functions
    funs <- zip(name=names(funs), val=funs)
    funs <- lapply(funs, function(x) extract_closure(x$val, x$name, .visited))

    globals <- c(vars, funs)

    structure(
        list(
            args=formals(fun),
            body=body(fun),
            globals=globals
        ),
        class="genthat_closure"
    )
}

create_trace <- function(fun, args=list(), globals=list(), retv) {
    stopifnot(is.character(fun))

    xs <- list(fun=fun, args=as.list(args), globals=as.list(globals))

    if (!missing(retv)) {
        xs$retv <- retv
        clazz <- "genthat_trace"
    } else {
        clazz <- "genthat_trace_entry"
    }

    structure(xs, class=clazz)
}

create_trace_error <- function(fun, args, msg) {
    stopifnot(is.character(fun))
    stopifnot(is.character(msg))

    structure(list(fun=fun, args=as.list(args), msg=msg), class="genthat_trace_error")
}

get_next_call_id <- function() {
    length(cache$traces)
}

reset_call_traces <- function() {
    cache$traces <- list()

    # TODO: do we need this?
    cache$capture_arguments <- TRUE
}

get_call_traces <- function() {
    cache$traces
}

set_call_trace <- function(call_id, trace) {
    stopifnot(is.numeric(call_id))
    stopifnot(any(sapply(class(trace), startsWith, prefix="genthat_trace")))

    cache$traces[[as.character(call_id)]] <- trace
}

get_call_trace <- function(call_id) {
    stopifnot(is.numeric(call_id))

    idx <- as.character(call_id)

    if (contains_key(cache$traces, idx)) {
        cache$traces[[idx]]
    } else {
        stop("Call ID: ", call_id, " does not exist")
    }
}
