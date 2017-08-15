# TODO: move to the private env
`__default_value_marker` <- new.env(parent=emptyenv())

decorate_with_onexit <- function(fun, name, pkg, record_fun) {
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
                on.exit({
                    genthat::disable_tracing()
                    retv <- returnValue(default=genthat:::`__default_value_marker`)
                    if (identical(retv, genthat:::`__default_value_marker`) ||
                            (is.list(retv) && length(retv) == 3 &&
                                 (is.null(retv[[1]]) || inherits(retv[[1]], "condition")) &&
                                 is.language(retv[[2]]) &&
                                 is.function(retv[[3]]))) {
                        RECORD_FUN(
                            name=NAME,
                            pkg=PKG,
                            args=as.list(match.call())[-1],
                            error=simpleError(geterrmessage(), call=(if (is.list(retv)) retv[[2]] else NULL)),
                            env=parent.frame()
                        )
                    } else {
                        RECORD_FUN(
                            name=NAME,
                            pkg=PKG,
                            args=as.list(match.call())[-1],
                            retv=retv,
                            env=parent.frame()
                        )
                    }
                }, add=TRUE)
                on.exit(genthat::enable_tracing(), add=TRUE)
            }
            BODY
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
