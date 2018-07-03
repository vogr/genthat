create_decorated_function <- function(fun, name, package,
                                      onentry=NULL, onexit=NULL, onerror=NULL) {

    stopifnot(is.function(fun))
    stopifnot(is_chr_scalar(name))
    stopifnot(is.null(package) || is_chr_scalar(package))
    stopifnot(is.null(onentry) || is.function(onentry))
    stopifnot(is.null(onexit) || is.function(onexit))
    stopifnot(is.null(onerror) || is.function(onerror))
    stopifnot(!all(is.null(onentry), is.null(onexit), is.null(onerror)))

    prologue <- if (!is.null(onentry)) {
        substitute(
            ONENTRY(
                list(
                    name=NAME,
                    package=PACKAGE,
                    seed=`__genthat_captured_seed`,
                    env=parent.frame(),
                    decorator="onentry"
                )
            ),
            list(ONENTRY=onentry, NAME=name, PACKAGE=package)
        )
    } else {
        NULL
    }

    onexit_call <- if (!is.null(onexit)) {
        substitute(
            ONEXIT(
                list(
                    name=NAME,
                    package=PACKAGE,
                    seed=`__genthat_captured_seed`,
                    env=parent.frame(),
                    args=args,
                    retv=retv,
                    decorator="onexit"
                )
            ),
            list(ONEXIT=onexit, NAME=name, PACKAGE=package)
        )
    } else {
        NULL
    }

    onerror_call <- if (!is.null(onerror)) {
        substitute(
            ONERROR(
                list(
                    name=NAME,
                    package=PACKAGE,
                    seed=`__genthat_captured_seed`,
                    env=parent.frame(),
                    args=args,
                    message=geterrmessage(),
                    decorator="onerror"
                )
            ),
            list(ONERROR=onerror, NAME=name, PACKAGE=package)
        )
    } else {
        NULL
    }

    epilogue <- if (!is.null(onexit_call) || !is.null(onerror_call)) {

        expr <- if (!is.null(onexit_call) && !is.null(onerror_call)) {
            substitute(
                if (!identical(retv, default)) {
                    onexit_call
                } else {
                    onerror_call
                }
            )
        } else if (!is.null(onexit_call)) {
            substitute(
                if (!identical(retv, default)) {
                    onexit_call
                }
            )
        } else {
            substitute(
                if (identical(retv, default)) {
                    onerror_call
                }
            )
        }

        substitute(
            on.exit({
                if (.Internal(getOption("genthat.tracing"))) {
                    .Internal(options(genthat.tracing=FALSE))

                    default <- genthat:::.genthat_default_retv
                    retv <- returnValue(default=default)
                    args <- as.list(match.call())[-1]

                    expr

                    .Internal(options(genthat.tracing=TRUE))
                }
            })
        )
    } else {
        NULL
    }

    # this one will capture the first argument passed to `fun<-`
    genthat_tmp <- if (endsWith(name, "<-")) {
        substitute(`__genthat_tmp` <- ARG_NAME, list(ARG_NAME=as.name(names(formals(fun))[1])))
    } else {
        NULL
    }

    obody <- body(fun)

    # the epilogue is above the body since it will be run in the on.exit handler
    nbody <- if (!is.null(prologue) && !is.null(epilogue)) {
        if (is.null(genthat_tmp)) {
            substitute({
                `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())

                prologue
                epilogue
                obody
            })
        } else {
            substitute({
                `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())
                genthat_tmp

                prologue
                epilogue
                obody
            })
        }
    } else if (!is.null(prologue)) {
        if (is.null(genthat_tmp)) {
            substitute({
                `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())

                prologue
                obody
            })
        } else {
            substitute({
                `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())
                genthat_tmp

                prologue
                obody
            })
        }
    } else if (!is.null(epilogue)) {
        if (is.null(genthat_tmp)) {
            substitute({
                `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())
                genthat_tmp

                epilogue
                obody
            })
        } else {
            substitute({
                `__genthat_captured_seed` <- get(".Random.seed", envir=globalenv())
                genthat_tmp

                epilogue
                obody
            })
        }
    } else {
        stop("Not possible")
    }

    # we do not copy attributes since we are only interested in
    # the body of the function
    create_function(params=formals(fun), body=nbody, env=environment(fun))
}
