#' @export
my_public <- function(x) {
    paste("public:", x)
}

my_private <- function(x) {
    paste("private:", x)
}

my_add <- function(a, b) a + b

my_call <- function(fn, ...) {
    fn(...)
}

my_warning <- function() {
    warning("my warning")
}

my_error <- function() {
    stop("my error")
}
