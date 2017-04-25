#' @export
`format.C++Error` <- function(e) {
    e$message
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
        matches <- c()
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

is_empty_str <- function(s) {
    !is.character(s) || nchar(s) == 0
}

get_function_name <- function(name) {
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

contains <- function(x, name) {
    stopifnot(!is_empty_str(name))
    stopifnot(is.character(names(x)))
    
    name %in% names(x)
}
