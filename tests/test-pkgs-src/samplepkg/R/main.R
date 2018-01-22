#' @name My public
#' @title A public function
#'
#' @export
#' @examples
#' 1+1 # just some code which does not use any code from this package
my_public <- function(x) {
    paste("public:", x)
}

my_private <- function(x) {
    paste("private:", x)
}

#' @name My add
#' @title Adds two numbers
#'
#' @export
#' @examples
#' my_add(1,2)
#' my_add(2,3)
my_add <- function(a, b) a + b

#' @name My call
#' @title Calls another function
#'
#' @export
#' @examples
#' my_call(my_add, 10, 11)
#' my_call(my_add, 11, 12)
my_call <- function(fn, ...) {
    fn(...)
}

my_warning <- function() {
    warning("my warning")
}

my_error <- function() {
    stop("my error")
}

#' @export
"gg<-" <- function(v, a, value) {
    v[v < a] <- value
    v
}

