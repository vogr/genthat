# private genthat space
`__genthat_private` <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    `__genthat_private`$on_exit_original <- create_duplicate(on.exit)

    options(genthat.debug=getOption("genthat.debug", default=FALSE))
    options(genthat.tryCatchDepth=try_catch_stack_depth())

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
