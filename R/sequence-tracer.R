#' @export
#'
create_sequence_tracer <- function() {
    sequence_tracer_create()
}

#' @export
#'
store_trace.sequence_tracer <- function(tracer, trace) {
    invisible(sequence_tracer_store_trace(tracer, trace))
}

#' @export
#'
reset_traces.sequence_tracer <- function(tracer) {
    sequence_tracer_reset_traces(tracer)
}

#' @export
#'
copy_traces.sequence_tracer <- function(tracer) {
    sequence_tracer_copy_traces(tracer)
}
