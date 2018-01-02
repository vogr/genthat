#' @export
`format.C++Error` <- function(x, ...) {
    x$message
}

clean_objects <- function(path) {
    files <-
        list.files(
            file.path(path, "src"),
            pattern="\\.[o|so|dylib|a|dll]$",
            full.names=TRUE,
            recursive = TRUE)
    unlink(files)
    invisible(files)
}

create_std_package_hook <- function(type, hook) {
    stopifnot(type %in% c("onLoad", "onAttach", "onDetach", "onUnload"))
    stopifnot(is.language(hook))

    deparse(
        substitute({
            setHook(packageEvent(pkg, EVENT), function(...) HOOK)
        }, list(EVENT=type, HOOK=hook))
    )
}

create_gc_finalizer_hook <- function(hook) {
    stopifnot(is.language(hook))

    deparse(
        substitute(
        reg.finalizer(ns, function(...) HOOK, onexit=TRUE)
        , list(HOOK=hook))
    )
}

# from covr
add_package_hook <- function(pkg_name, lib, on_load, on_gc_finalizer) {
    stopifnot(is.character(pkg_name))
    stopifnot(dir.exists(lib))

    r_dir <- file.path(lib, pkg_name, "R")
    if (!dir.exists(r_dir)) {
        stop("Unable to add package hook to ", pkg_name, ", the package is missing the `R` folder")
    }

    load_script <- file.path(r_dir, pkg_name)
    lines <- readLines(load_script)

    lines <- append(lines, "options(error=function() {traceback(3); if(!interactive()) quit(status=1, save='no')})", 0)

    if (!missing(on_load)) {
        lines <- append(lines, create_std_package_hook("onLoad", on_load), after=length(lines) - 1L)
    }
    if (!missing(on_gc_finalizer)) {
        lines <- append(lines, create_gc_finalizer_hook(on_gc_finalizer), after=length(lines) - 1L)
    }

    writeLines(text=lines, con=load_script)
}

# from covr
env_path <- function(...) {
  paths <- c(...)
  paste(paths[nzchar(paths)], collapse = .Platform$path.sep)
}

filter_idx <- function(X, FUN, ...) {
    matches <- sapply(X, FUN, ...)

    if (length(matches) == 0) {
        matches <- numeric()
    }

    matches
}

filter <- function(X, FUN, ...) {
    X[filter_idx(X, FUN, ...)]
}

filter_not <- function(X, FUN, ...) {
    X[!filter_idx(X, FUN, ...)]
}

zip <- function(...) {
    mapply(list, ..., SIMPLIFY=FALSE)
}

list_contains <- function(l, x) {
    any(sapply(l, identical, x))
}

reduce <- function(X, FUN, init, ...) {
    Reduce(function(a, b) FUN(a, b, ...), init=init, X)
}

is_empty_str <- function(s) {
    !is.character(s) || nchar(s) == 0
}

split_function_name <- function(name) {
    stopifnot(!is_empty_str(name))

    x <- strsplit(name, ":")[[1]]
    if (length(x) == 1) {
        list(package=NULL, name=x[1])
    } else {
        list(package=x[1], name=x[length(x)])
    }
}

create_function <- function(params, body, env=parent.frame(), attributes=list()) {
    stopifnot(is.pairlist(params))
    stopifnot(is.language(body))
    stopifnot(is.environment(env))

    fun <- as.function(c(as.list(params), list(body)))

    environment(fun) <- env
    attributes(fun) <- attributes

    fun
}

# TODO: rename to list_contains_key
contains_key <- function(x, name) {
    name <- as.character(name)

    stopifnot(!is_empty_str(name), length(name) == 1)
    stopifnot(is.list(x) || is.vector(x))
    names <- names(x)

    if (length(names) == 0) {
        FALSE
    } else {
        name %in% names(x)
    }
}

bag_add <- function(bag, key, value) {
    stopifnot(is.list(bag))

    name <- as.character(key)

    if (contains_key(bag, name)) {
        bag[[name]] <- append(bag[[name]], value)
    } else {
        bag[[name]] <- list(value)
    }

    bag
}

bag_contains_value <- function(bag, key, value) {
    stopifnot(is.list(bag))

    name <- as.character(key)

    if (contains_key(bag, name)) {
        list_contains_value(bag[[name]], value)
    } else {
        FALSE
    }
}

list_contains_value <- function(l, value) {
    any(sapply(l, function(x) isTRUE(all.equal(x, value))))
}

is.closure <- function(f) {
    typeof(f) == "closure"
}

#' @importFrom method is
is.formula <- function(f) {
    is.language(f) && is(f, "formula")
}

is.local_closure <- function(f) {
    is.closure(f) && is.null(get_package_name(environment(f)))
}

#' @importFrom methods getPackageName
#'
get_package_name <- function(env) {
    stopifnot(is.environment(env))

    if (identical(env, globalenv())) {
        NULL
    } else if (environmentName(env) == "") {
        NULL
    } else {
        name <- methods::getPackageName(env, create=FALSE)
        if (isNamespaceLoaded(name)) {
            name
        } else {
            NULL
        }
    }
}

#' @title Links the environments of the surrounding functions
#' @description Sets the parent environment of all the functions defined in the given environment `env` to `parent`.
#'
#' @param env the environment in which to look for functions
#' @param parent the environment to use as the parent environment of the functions
#'
link_environments <- function(env=parent.frame(), parent=parent.env(env), .fun_filter=is.local_closure) {
    vars <- as.list(env)
    funs <- filter(vars, .fun_filter)

    lapply(funs, function(x) {
        f_env <- environment(x)
        if (!identical(f_env, emptyenv())) {
            parent.env(f_env) <- env
            link_environments(env=f_env)
        }
    })
}

#' @export
#'
with_env <- function(f, env) {
    link_environments(env)
    environment(f) <- env
    f
}

#' @export
#'
capture <- function(expr, split=FALSE) {
    out <- tempfile()
    err <- tempfile()
    on.exit(file.remove(c(out, err)))

    fout = file(out, open="wt")
    ferr = file(err, open="wt")

    sink(type="message", file=ferr)
    sink(type="output", file=fout, split=split)

    tryCatch({
        time <- stopwatch(expr)
    }, finally={
        sink(type="message")
        sink(type="output")
        close(fout)
        close(ferr)
    })

    list(
        elapsed=time,
        stdout=paste(readLines(out), collapse="\n"),
        stderr=paste(readLines(err), collapse="\n")
    )
}

resolve_package_name <- function(package) {
    # TODO: no a very good heuristics (read manually description using read.dcf)
    if (file.exists(package) && endsWith(package, ".tar.gz")) {
        basename(untar(package, list=TRUE)[1])
    } else if (dir.exists(package)) {
        basename(package)
    } else {
        tryCatch({
            # this throws an exception if it does not exist
            find.package(package)
            package
        }, error=function(e) {
            stop("Unsupported / non-existing package: ", package)
        })
    }
}

resolve_function <- function(name, in_env=parent.frame()) {
    fun <- if (is.function(name)) {
        fun
    } else if (is.name(name) || is.character(name)){
        get(name, envir=in_env)
    }

    if (!is.function(fun)) {
        stop("Not a function: ", name)
    }

    fun
}

get_function_package_name <- function(fun, name) {
    stopifnot(is.null(name) ||(is.character(name) && length(name) == 1))
    stopifnot(is.function(fun))

    env <- environment(fun)
    pkg_name <- get_package_name(env)

    # A function's environment does not need to be a named environment. For
    # example if a package in a function is defined using a call to another
    # higher-order function, it will have a new environment whose parent will be
    # named environment. This can obviously nest. A concrete example is `%>%`
    # from magrittr (cf.
    # https://github.com/tidyverse/magrittr/blob/master/R/pipe.R#L175 ).
    if (is.null(pkg_name) && length(env) == 0 && !is.null(name)) {
        env <- find_symbol_env(name, env)
        pkg_name <- get_package_name(env)
    }

    if (identical(env, .BaseNamespaceEnv)) {
        return("base")
    }


    if (is_empty_str(pkg_name) || identical(env, globalenv())) {
        NULL
    } else {
        pkg_name
    }
}

stopwatch <- function(expr) {
    time <- Sys.time()
    force(expr)
    Sys.time() - time
}

next_file_in_row <- function(path) {
    stopifnot(nchar(path) > 0)

    dname <- dirname(path)
    fname <- basename(path)

    ext <- tools::file_ext(fname)
    name <- tools::file_path_sans_ext(fname)

    n <- length(Sys.glob(path=file.path(dname, paste0(name, "*", ".", ext))))

    file.path(dname, paste0(name, "-", n, ifelse(nchar(ext) > 0, paste0(".", ext), "")))
}

is_interesting_namespace <- function(env, prefix) {
    stopifnot(is.environment(env))
    stopifnot(is.character(prefix) && length(prefix) == 1)

    env_name <- environmentName(env)
    if (is.null(env_name)) {
        return(FALSE)
    }

    # naive as it looks, this is the same check the bytecode compiler does
    # in https://github.com/wch/r-source/blob/trunk/src/library/compiler/R/cmp.R#L107
    startsWith(env_name, prefix)
}

is_imports_namespace <- function(env) {
    is_interesting_namespace(env, "imports:")
}

is_package_environment <- function(env) {
    is_interesting_namespace(env, "package:")
}

is_package_namespace <- function(env) {
    isNamespace(env)
}

is_base_env <- function(env) {
    isBaseNamespace(env) || identical(env, baseenv())
}

log_debug <- function(...) {
    if (is_debug_enabled()) {
        msg <- paste0(...)
        cat(msg, "\n")
    }
}

is_exception_returnValue <- function(retv) {
    is.list(retv) &&
        length(retv) == 3 &&
        (is.null(retv[[1]]) || is(retv[[1]], "condition")) &&
        is.language(retv[[2]]) &&
        is.function(retv[[3]])
}

is_chr_scalar <- function(s) {
    is.character(s) && length(s) == 1 && nchar(s) > 0
}
