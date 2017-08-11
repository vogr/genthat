decorate_with_trycatch <- function(fun, name, pkg, record_fun) {
    stopifnot(is.function(fun))
    stopifnot(is.character(name) && length(name) == 1)
    stopifnot(is.null(pkg) || (is.character(pkg) && length(pkg) == 1))

    # The `retv <- BODY` is OK because in the case of multiple expressions, it will be surrounded with `{}`.
    # The as.list(match.call(...)) will force the arguments.
    # TODO: is that OK?
    # - guess so if it fails, it would fail when we create the test
    # - need to find the exact cases...
    create_function(
        params=formals(fun),
        body=substitute({
            if (genthat::is_tracing_enabled()) {
                on.exit(genthat::enable_tracing())

                genthat::disable_tracing()

                frame <- new.env(parent=parent.frame())
                frame$original <- attr(sys.function(), "__genthat_original_fun")

                call <- sys.call()
                call[[1]] <- as.name("original")

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

                    retv
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
            } else {
                call <- sys.call()
                call[[1]] <- attr(sys.function(), "__genthat_original_fun")
                eval(call)
            }
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
