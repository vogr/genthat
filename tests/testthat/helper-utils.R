# returns a function that will save parent function's arguments into an environment
save_calling_args <- function(env, return_value=TRUE, ...) {
    stopifnot(is.environment(env))
    names <- c(...)

    function(...) {
        args <- list(...)
        
        if (length(names) > 0) {
            stopifnot(length(names) == length(args))
            names(args) <- names
        }

        args$retv <- returnValue()

        l <- paste0("c", length(env) + 1)
        assign(l, args, envir=env)

        return_value
    }
}
