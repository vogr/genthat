patched_on_exit <- function(expr=NULL, add) {
    invisible(.Primitive("on.exit")(expr, TRUE))
}

patch_on_exit <- function() {
    if (!identical(on.exit, patched_on_exit)) {
        unlockBinding("on.exit", baseenv())
        assign("on.exit", patched_on_exit, envir=baseenv())
        lockBinding("on.exit", baseenv())
    }

    invisible(NULL)
}

reset_on_exit <- function() {
    if (identical(on.exit, patched_on_exit)) {
        unlockBinding("on.exit", baseenv())
        assign("on.exit", `__genthat_private`$on_exit_original, baseenv())
        lockBinding("on.exit", baseenv())
    }

    invisible(NULL)
}
