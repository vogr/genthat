#' @export
#'
create_set_tracer <- function() {
    structure(list(traces=new.env(parent=emptyenv(), hash=TRUE)), class="set_tracer")
}

#' @export
#'
store_trace.set_tracer <- function(tracer, trace) {
    key <- digest::digest(trace, algo="sha1")
    if (is.null(tracer$traces[[key]])) {
        tracer$traces[[key]] <- trace
    }
    invisible(trace)
}

#' @export
#'
reset_traces.set_tracer <- function(tracer) {
    rm(list=ls(envir=tracer$traces, sort=FALSE, all.names=TRUE), envir=tracer$traces)
}

#' @export
#'
copy_traces.set_tracer <- function(tracer) {
    traces <- as.list(tracer$traces)
    names(traces) <- NULL
    traces
}
