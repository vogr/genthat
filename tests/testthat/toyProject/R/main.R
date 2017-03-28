
public_fn <- function(x) {
    x <- paste("this is public: ", x, sep="")
    message(paste0("returning ", x));
    return(x)
}

private_fn <- function(x) {
    paste("this is private: ", x, sep="")
}

my_apply <- function(fn, x) {
    fn(x)
}

my_add <- function(a, b) a + b

applyFirst <- function(fn, x) {
    function(y) fn(x, y)
}

causes_warning <- function() {
    warning("this is a warning")
    character()
}

variadic <- function(x, ...) {
}

fnWithDefaults <- function(x, y = N) {
    if (x < 10) {
        N <- 3 * x
    } else {
        N <- 4 * x
    }
    message(N)
    N <- 99
}

argsOrder <- function(a, b = 3, c, ...) {
    rest <- list(...)
    return(paste0(a , b , c , rest[[1]] , rest[[2]] , rest[[3]]))
}
