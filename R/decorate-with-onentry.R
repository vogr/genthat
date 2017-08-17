decorate_with_onentry <- function(fun, name, pkg, record_fun) {
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
                genthat::disable_tracing()
                on.exit(genthat::enable_tracing())

                RECORD_FUN(
                    name=NAME,
                    pkg=PKG,
                    args=as.list(match.call())[-1],
                    env=parent.frame()
                )

                genthat::enable_tracing()
                `__retv` <- BODY
                genthat::disable_tracing()

                RECORD_FUN(
                    name=NAME,
                    pkg=PKG,
                    args=as.list(match.call())[-1],
                    retv=`__retv`,
                    env=parent.frame()
                )

                genthat::enable_tracing()

                `__retv`
            } else {
                BODY
            }
        }, list(
            NAME=name,
            PKG=pkg,
            RECORD_FUN=record_fun,
            BODY=body(fun)
        )),
        env=environment(fun),
        attributes=list(
            `__genthat_original_fun`=create_duplicate(fun)
        )
    )
}
