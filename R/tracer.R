#' Stores trace
#'
#' Stores the given \code{trace} to the given \code{tracer}.
#'
#' @param tracer the tracer to store the \code{trace} into
#' @param trace the trace to be store
#'
#' @export
#'
store_trace <- function(tracer, trace) {
    UseMethod("store_trace")
}

#' Reset traces
#'
#' Clears the captured traces
#'
#' @export
#'
reset_traces <- function(tracer) {
    UseMethod("reset_traces")
}

#' Copy call traces
#'
#' Creates a copy of traces captured so far and returns them as R list.
#'
#' @export
#'
copy_traces <- function(tracer) {
    UseMethod("copy_traces")
}

#' @export
#'
copy_traces.default <- function(tracer) {
    tracer <- get_tracer()
    stopifnot(!is.null(tracer))

    copy_traces(tracer)
}

#' @export
#'
reset_traces.default <- function(tracer) {
    tracer <- get_tracer()
    stopifnot(!is.null(tracer))

    reset_traces(tracer)
}

#' Creates a new tracer of the given type.
#'
#' Creates either a set tracer that only stores unique traces (unique
#' combination of function/package name, argument values and return value) or
#' sequence tracer that stores all traces.
#'
#' @param type the type of the tracer, can e either \code{set} of
#'     \code{sequence}. The default is what is set in
#'     \code{genthat.tracer_type} option.
#'
#' @param ... additional parameters to be passed to the function creating the
#'    #actual tracer
#'
#' @seealso \code{\link{create_set_tracer}}
#' @seealso \code{\link{create_sequence_tracer}}
#'
#' @export
#'
create_tracer <- function(type=getOption("genthat.tracer_type"), ...) {
    type <- match.arg(arg=type, choices=c("sequence", "set"), several.ok=FALSE)
    fun <- switch(type,
        sequence=create_sequence_tracer,
        set=create_set_tracer
    )

    fun(...)
}

#' Makes the given \code{tracer} default
#'
#' Sets the given tracer to be the one used for tracing from now on.
#'
#' @param tracer the tracer that shall become the default one
#'
#' @return the previous tracer or \code{NULL} if no tracer has been set
#'     yet.
#'
#' @export
#'
set_tracer <- function(tracer) {
    stopifnot(!is.null(tracer))

    prev <- .genthat$tracer
    .genthat$tracer <- tracer

    invisible(prev)
}

#' Gets the currently used tracer
#'
#' Returns the current tracer. If no such tracer exists, it will create one by
#' calling \code{create_tracer()} with no arguments, i.e. using the default
#' settings.
#'
#' @return the current tracer
#'
#' @seealso \code{\link{create_tracer()}}
#'
#' @export
#'
get_tracer <- function() {
    tracer <- .genthat$tracer

    if (is.null(tracer)) {
        tracer <- create_tracer()
        set_tracer(tracer)
    }

    tracer
}
