on_function_entry <- function(call_id, name, args) {
    stopifnot(is.numeric(call_id))
    stopifnot(is.character(name))
    stopifnot(is.list(args))

    parent <- new.env(parent=parent.frame(2)) # 2 generations back - the callee of the original function
    args_vals <- lapply(args, eval, envir=parent)

    tryCatch({
        # TODO: do we need this?
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
    stopifnot(is.numeric(call_id))

    trace <- get_call_trace(call_id)
    stopifnot(methods::is(trace, "genthat_trace_entry"))

    cache$capture_arguments <- FALSE

    tryCatch({
        trace <- create_trace(trace$fun, trace$args, serialize_value(retv))
    }, error=function(e) {
        trace <- create_trace_error(trace$fun, trace$args, format(e))
    })

    set_call_trace(call_id, trace)

    cache$capture_arguments <- TRUE
}

create_trace <- function(fun, args=list(), retv) {
    stopifnot(is.character(fun))
    stopifnot(is.list(args))

    xs <- list(fun=fun, args=args)

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

    structure(list(fun=fun, args=args, msg=msg), class="genthat_trace_error")
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

    cache$traces[[as.character(call_id)]]
}
