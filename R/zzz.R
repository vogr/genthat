# TODO: rename to a better name
cache <- new.env(parent=emptyenv())


set_genthat_options <- function(..., force=FALSE) {
    ops <- list(...)
    names <- names(ops)

    stopifnot(is.character(names))
    names <- paste0("genthat.", names)

    to_set <- !(names %in% names(options()))
    if (any(to_set)) {
        options(ops[to_set])
    }
}

is_debug_enabled <- function(name) {
    isTRUE(getOption(paste0("genthat.debug")))
}

.onLoad <- function(libname, pkgname) {
    reset_call_traces()
    reset_replacements()

    cache$tracing <- TRUE

    set_genthat_options(debug=FALSE)

    invisible()
}
