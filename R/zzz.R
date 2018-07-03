Rcpp::loadModule("SerializerModule", TRUE)

# used to compare return value from returnValue()
.genthat_default_retv <- new.env(parent=emptyenv())

# store all functions decorations
.decorations <- new.env(parent=emptyenv())

# used for some private values
# - tracer: current tracer used to store traces
.genthat <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    # this is just initialize the random generator so the .Random.seed is available
    set.seed(42)

    # it will be create by need when tracing
    .genthat$tracer <- NULL

    options(
        genthat.debug=getOption("genthat.debug", default=FALSE),
        genthat.tracer_type=getOption("genthat.tracer_type", default="set"),
        genthat.keep_failed_tests=getOption("genthat.keep_failed_tests", FALSE),
        genthat.keep_failed_traces=getOption("genthat.keep_failed_traces", FALSE),
        genthat.keep_all_traces=getOption("genthat.keep_all_traces", FALSE),
        genthat.max_trace_size=getOption("genthat.max_trace_size", 512*1024)
    )

    enable_tracing()

    invisible()
}
