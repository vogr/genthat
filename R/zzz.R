# TODO: rename to a better name
cache <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    reset_call_traces()
    reset_replacements()
}
