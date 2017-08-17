check_decorate_args <- function(fun, name, pkg, record_fun) {
    stopifnot(is.function(fun))
    stopifnot(is.character(name) && length(name) == 1)
    stopifnot(is.null(pkg) || (is.character(pkg) && length(pkg) == 1))
}

decorate_with_onboth <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    # The `retv <- BODY` is OK because in the case of multiple expressions, it will be surrounded with `{}`.
    create_function(
        params=formals(fun),
        body=substitute({
            if (genthat::is_tracing_enabled()) {
                genthat::disable_tracing()
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1])
                genthat::enable_tracing()

                `__retv` <- BODY

                genthat::disable_tracing()
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], retv=`__retv`)
                on.exit(genthat::enable_tracing(), add=TRUE)

                `__retv`
            } else {
                BODY
            }
        }, list(NAME=name, PKG=pkg, RECORD_FUN=record_fun, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_onentry <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    create_function(
        params=formals(fun),
        body=substitute({
            if (genthat::is_tracing_enabled()) {
                genthat::disable_tracing()
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1])
                genthat::enable_tracing()
            }

            BODY
        }, list(NAME=name, PKG=pkg, RECORD_FUN=record_fun, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_onexit <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    # The `retv <- BODY` is OK because in the case of multiple expressions, it will be surrounded with `{}`.
    create_function(
        params=formals(fun),
        body=substitute({
            `__retv` <- BODY

            if (genthat::is_tracing_enabled()) {
                genthat::disable_tracing()
                RECORD_FUN(name=NAME, pkg=PKG, args=as.list(match.call())[-1], retv=`__retv`)
                on.exit(genthat::enable_tracing(), add=TRUE)
            }

            `__retv`
        }, list(NAME=name, PKG=pkg, RECORD_FUN=record_fun, BODY=body(fun))),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}

decorate_with_trycatch <- function(fun, name, pkg, record_fun) {
    check_decorate_args(fun, name, pkg, record_fun)

    create_function(
        params=formals(fun),
        body=substitute({
            if (genthat::is_tracing_enabled()) {
                genthat::disable_tracing()

                on.exit(genthat::enable_tracing())

                frame <- new.env(parent=parent.frame())
                assign(NAME, attr(sys.function(), "__genthat_original_fun"), envir=frame)
                call <- sys.call()
                call[[1]] <- as.name(NAME)

                tryCatch({
                    genthat::enable_tracing()
                    retv <- eval(call, envir=frame)
                    genthat::disable_tracing()

                    RECORD_FUN(
                        name=NAME,
                        pkg=PKG,
                        args=as.list(match.call())[-1],
                        retv=retv,
                        env=parent.frame()
                    )

                    return(retv)
                },  error=function(e) {
                    genthat::disable_tracing()

                    depth <- getOption("genthat.tryCatchDepth")
                    env <- parent.frame(depth + 2)
                    match_call <- match.call(
                        definition=sys.function(-depth - 1),
                        call=sys.call(-depth - 1),
                        envir=env
                    )

                    RECORD_FUN(
                        name=NAME,
                        pkg=PKG,
                        args=as.list(match_call)[-1],
                        error=e,
                        env=env
                    )

                    stop(e)
                })
            }

            frame <- new.env(parent=parent.frame())
            assign(NAME, attr(sys.function(), "__genthat_original_fun"), envir=frame)

            call <- sys.call()
            call[[1]] <- as.name(NAME)

            eval(call, envir=frame)
        }, list(
            NAME=name,
            PKG=pkg,
            RECORD_FUN=record_fun
        )),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}
