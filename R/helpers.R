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

    load_script <- file.path(lib, pkg_name, "R", pkg_name)
    lines <- readLines(load_script)

    lines <- append(lines, "options(error = function() traceback(2))", 0)

    if (!missing(on_load)) {
        lines <- append(lines, create_std_package_hook("onLoad", on_load), after=length(lines) - 1L)
    }
    if (!missing(on_gc_finalizer)) {
        lines <- append(lines, create_gc_finalizer_hook(on_gc_finalizer), after=length(lines) - 1L)
    }

    writeLines(text=lines, con=load_script)
}

#' @importFrom utils head
# from covr
show_failures <- function(dir) {
  fail_files <- list.files(dir, pattern = "fail$", recursive = TRUE, full.names = TRUE)
  for (file in fail_files) {
    lines <- readLines(file)
    # Skip header lines (until first >)
    lines <- lines[seq(head(which(grepl("^>", lines)), n = 1), length(lines))]
    stop("Failure in `", file, "`\n", paste(lines, collapse = "\n"), call. = FALSE)
  }
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

list_merge <- function(xs, ys) {
    xs <- as.list(xs)
    xs_names <- names(xs)

    if (is.null(xs_names)) {
        return(c(xs, ys))
    }

    ys <- as.list(ys)
    ys_names <- names(ys)

    if (is.null(ys_names)) {
        return(c(xs, ys))
    }

    # remove empty names in xs
    xs_names <- xs_names[xs_names != ""]
    stopifnot(all.equal(xs_names, unique(xs_names)))

    # remove empty names in xs
    ys_names <- ys_names[ys_names != ""]
    stopifnot(all.equal(ys_names, unique(ys_names)))

    xs_names_in_ys <- xs_names[which(xs_names %in% ys_names)]
    ys_names_not_in_xs <- ys_names[which(!(ys_names %in% xs_names))]

    rs <- xs
    # replace existing
    rs[xs_names_in_ys] <- ys[xs_names_in_ys]
    # copy the rest of the names elements
    rs <- c(rs, ys[ys_names_not_in_xs])
    rs <- c(rs, ys[names(ys) == ""])
    rs
}



#' @export
link_environments <- function(parent=globalenv(), env=parent.frame()) {
    vars <- as.list(env)
    funs <- filter(vars, is.function)
    lapply(funs, function(x) {
        f_env <- environment(x)
        parent.env(f_env) <- env
        link_environments(f_env)
    })

    new_env <- new.env(parent=parent)
    list2env(vars, new_env)
    new_env
}
