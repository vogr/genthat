create_trace <- function(fun, args, retv=NULL) {
    stopifnot(is.character(fun))

    structure(list(fun=fun, args=args, retv=retv), class="genthat_trace")
}

create_trace_error <- function(fun, args, msg) {
    stopifnot(is.character(fun))
    stopifnot(is.character(msg))

    structure(list(fun=fun, args=args, msg=msg), class="genthat_trace_error")
}

#' @title Returns the next available call id.
#'
#'
get_next_call_id <- function() {
    call_id <- cache$call_id_counter
    cache$call_id_counter <- cache$call_id_counter + 1
    call_id
}

reset_call_traces <- function() {
    cache$traces <- list()
    #cache$capture_file_size <- 50 * 1000 * 1000
    # TODO: rename cache
    cache$capture_arguments <- TRUE
    #cache$capture_dir = NULL
    #cache$capture_num <- 0
    #cache$output_dir <- NA
    #cache$generated_tests <- 0L
    #cache$retv_mismatch_count <- 0L
    #cache$unparsable_count <- 0L
    #cache$arguments <- list()
    cache$call_id_counter <- 0
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
