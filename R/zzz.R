Rcpp::loadModule("SerializerModule", TRUE)

# private genthat space
`__genthat_default_retv` <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    # this is just initialize the random generator so the .Random.seed is available
    set.seed(42)
    options(genthat.debug=getOption("genthat.debug", default=FALSE))
    options(genthat.tryCatchDepth=try_catch_stack_depth())
    options(genthat.keep_failed_tests=getOption("genthat.keep_failed_tests", FALSE))
    options(genthat.max_trace_size=getOption("genthat.max_trace_size", 128*1024))
    options(genthat.source_paths=getOption("genthat.source_paths", Sys.getenv("GENTHAT_SOURCE_PATHS")))

    enable_tracing()

    invisible()
}

# finds how depth is try catch call
try_catch_stack_depth <- function() {
    tryCatch({
        stop()
    }, error=function(e) {
        call_stack <- sapply(sys.calls(), '[[', 1)
        idx <- max(which(call_stack == as.name("tryCatch")))
        stopifnot(length(idx) == 1)
        abs(idx - sys.nframe())
    })
}
