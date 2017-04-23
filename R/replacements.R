get_replacements <- function() {
    cache$replacements
}

create_replacement <- function(name, env, orig_fun, fun, new_fun) {
    stopifnot(is.character(name))
    stopifnot(is.environment(env))
    stopifnot(is.function(orig_fun))
    stopifnot(is.function(fun))
    stopifnot(is.function(new_fun))

    structure(
        list(
            name=name,
            env=env,
            orig_fun=orig_fun,
            fun=fun,
            new_fun=new_fun
        ),
        class="genthat_replacement")
}

add_replacement <- function(r) {
    stopifnot(is(r, "genthat_replacement"))
    stopifnot(!(r$name %in% names(cache$replacements)))
    
    cache$replacements[[r$name]] <- r
}

remove_replacement <- function(name) {
    r <- cache$replacements[[name]]

    if (is.null(r)) {
        stop(name, ": is not a replaced")
    }
    
    rm(list=name, envir=cache$replacements)
    r
}

reset_replacements <- function() {
    if (length(get_replacements()) != 0) {
        warning("There are still decorated functions")
    }

    cache$replacements <- new.env(parent=emptyenv())
}
