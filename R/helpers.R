
noop <- function(...) {}

#' @title Check if function is S3 generic
#'
#' @description Determine if function has a call to UseMethod. In that case there is no need to capture it.
#' @param fname function name
#' @param env environment to check aganist. Default \code{.GlobalEnv}
#' @seealso Decorate
is_s3_generic <- function(fname, env=.GlobalEnv) {
    f <- get(fname, mode = "function", envir = env)
    if (is.null(body(f))) return(FALSE)
    uses <- codetools::findGlobals(f, merge = FALSE)$functions
    any(uses == "UseMethod")
    #"UseMethod" %in% uses
}

#' @title Removes prefixes and quote from line
#'
#' @description Used for processing capture file information. Deletes prefixes to get essential information
#' @param prefix prefix
#' @param line input line
#' @seealso ProcessClosure
strip_prefix <- function(prefix, line){
    substr(line, nchar(prefix) + 1, nchar(line))
}

#' @title Check line's starting prefix
#' @description Check if line starts with prefix
#'
#' @param prefix prefix
#' @param x text to be checked
#' @seealso GenerateTC
starts_with <- function(prefix, x) {
    grepl(paste("^", prefix, sep=""), x)
}

#' @title Find test directory for package
#'
#' @description Find a known test location for the package
#' @param path package path
#' @seealso CapturePackage
find_tests <- function(path) {
    testthat <- file.path(path, "tests", "testthat")
    if (file.exists(testthat) && file.info(testthat)$isdir) {
        return(testthat)
    }
    inst <- file.path(path, "inst", "tests")
    if (file.exists(inst) && file.info(inst)$isdir) {
        return(inst)
    }
    warning("No testthat directories found in ", path, call. = FALSE)
    return(NULL)
}

#' @title Get function name without special characters
#'
#' @description This function is respinsible for extractng function name from test file name and removing special characters
#' @param filename filename to be processed
#' @param modify.characters if special characters should be removed
#'
extract_func_name <- function(filename, modify.characters = TRUE){
    fname <- filename
    if (grepl(".[rR]$", filename)) {
        fname <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
    }
    if (fname %in% operators) {
        fname <- "operators"
    }
    if (modify.characters){
        fname <- gsub("\\.", "", fname)
        fname <- gsub("<-", "assign_", fname)
        fname <- gsub("\\[", "extract_parentasis_", fname)
        fname <- gsub("\\$", "extract_dollar_", fname)
        fname <- gsub("\\+", "plus_", fname)
        fname <- gsub("\\-", "minus_", fname)
        fname <- gsub("&", "and_", fname)
        fname <- gsub("\\*", "times_", fname)
    }
    fname
}


#' @title Parse function names from objects
#' @description  Parses given function names to a list of name, package characters.
#' If package is not specified, NA is returned instead of its name.
#'
#' @param ... Functions either as character vectors, or package:::function expressions.
#' @return List of parsed package and function names as characters.
parseFunctionNames <- function(...) {
    recover <- function(e) {
        a <- args[[i]]
        if (is.name(a)) {
            result[[i]] <<- c(name = as.character(a), package = NA)
        } else if (is.language(a) && length(a) == 3 && as.character(a[[1]]) %in% c(":::", "::")) {
            result[[i]] <<- c(name = as.character(a[[3]]), package = as.character(a[[2]]))
        } else {
            print("error")
            stop(paste("Invalid argument index", i));
        }
    }
    args <- as.list(substitute(list(...)))[-1]
    i <- 1
    result <- list()
    result[length(args)] <- NULL
    while (i <= length(args)) {
        tryCatch({
            x <- eval(as.name(paste("..",i,sep="")))
            if (is.character(x)) {
                # it is a character vector, use its value
                x <- strsplit(x, ":::")[[1]]
                if (length(x) == 1) {
                    x <- strsplit(x, "::")[[1]]
                    if (length(x) == 1)
                        x <- list(NA, x)
                }
                if (x[[2]] == "")
                    x[[2]] <- ":::"
                result[[i]] <- c(name = x[[2]], package = x[[1]])
            } else {
                stop("Use substitured value")
            }
        }, error = recover)
        i <- i + 1
    }
    result
}

#' @title Returns names of functions defined in given file(s)
#'
#' @description Analyses given file, or files if directory
#' is supplied for all functions defined in global scope and returns their names as character vector.
#'
#' @param src.root A source file to be analyzed, or a directory containing source files (*.R or *.r) to be analyzed.
#' @param recursive TRUE if subdirectories should be scanned too.
#' @return Character vector of function names defined in the file.
list_functions <- function(src.root, recursive = TRUE) {
    functions = character()
    if (file.info(src.root)$isdir)
        src.root <- list.files(src.root, pattern = "\\.[rRsS]$", recursive = recursive, full.names = T)
    for (src.file in src.root) {
        exp <- parse(src.file)
        for (e in exp) {
            if (typeof(e) == "language" && (e[[1]] == as.name("<-") || e[[1]] == as.name("=")) && is.name(e[[2]])) {
                name <- e[[2]]
                what <- e[[3]]
                if (typeof(what) == "language" && what[[1]] == as.name("function")) {
                    functions = c(functions, as.character(name))
                }
            }
        }
    }
    functions
}

split_path <- function(path) {
    setdiff(strsplit(path,"/|\\\\")[[1]], "")
}

extract_example <- function(ex) {
    sapply(ex, function(x) x[[1]])
}

example_code <- function(fromFile) {
    code <- tools::parse_Rd(fromFile)
    code <- Filter(function(x) attr(x, "Rd_tag") == "\\examples", code)
    result = ""
    for (cc in code)
        result = c(result, extract_example(cc))
    result
}

e_msg <- function(x) { TRUE }

#' @title Expects string arguments and returns the concatenation of all of them.
#'
#' @description Every argument must me a single string.
#'
#' @param sep optional separator
#' @return character scalar
concat <- function(..., sep="") {
    for (x in as.list(...)) {
        stopifnot(length(x) == 1 && e_msg("Got vector argument to concat!"))
    }
    ret <- paste0(..., sep=sep, collapse="")
    stopifnot(length(ret) == 1 && e_msg("Result of concat() is not single string!"))
    ret
}

#' @title Expects exactly one character vector argument and returns all the elements concatenated together.
#'
#' @description Every argument must me a single string.
#'
#' @param sep optional separator
#' @return character scalar
concatVec <- function(xs, sep="") {
    ret <- paste(xs, collapse=sep)
    stopifnot(length(ret) == 1 && e_msg("Result of concatVec() is not single string!"))
    ret
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/Quotes.html
testIsSyntacticName <- function(name) {
    syntacticNameRegex <- '^([a-zA-Z][a-zA-Z0-9._]*|[.]([a-zA-Z._][a-zA-Z0-9._]*)?)$'
    reservedWords <- c(
        'if', 'else', 'repeat', 'while', 'function', 'for', 'in', 'next',
        'break', 'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_',
        'NA_real_', 'NA_complex_', 'NA_character_'
    )
    isReservedWord <- is.element(name, reservedWords)
    matchesRegex <- length(grep(syntacticNameRegex, name, perl=TRUE))
    !isReservedWord && matchesRegex
}

escapeNonSyntacticName <- function(name) {
    isSyntacticName <- testIsSyntacticName(name)
    if (isSyntacticName) {
        name
    } else {
        concat('`', name, '`')
    }
}

force_rebind <- function(what, value, env) {
    sym <- as.name(what)
    .Internal(unlockBinding(sym, env))
    assign(what, value, env)
    .Internal(lockBinding(sym, env))
}

getImportsEnvironment <- function(pkgName) {
    parent.env(getNamespace(pkgName))
}

overwrite_export <- function(name, val, package) {
    pkg.namespace_env <- getNamespace(package)
    pkg.package_env <- as.environment(paste0("package:", package))
    force_rebind(name, val, pkg.namespace_env)
    is.exported <- exists(name, envir = pkg.package_env, inherits = FALSE)
    if (is.exported) {
        force_rebind(name, val, pkg.package_env)
        if (package != "base") {
            reverse_dependencies <- tryCatch(getNamespaceUsers(package), error = function(e) c())
            lapply(reverse_dependencies, function(depPackage) {
                importsEnv <- getImportsEnvironment(depPackage)
                force_rebind(name, val, importsEnv)
            })
        }
    }
    invisible(NULL)
}

formatTime <- function(secs) {
    x <- secs
    hours <- floor(x / 3600)
    x <- x - hours * 3600
    minutes <- floor(x / 60)
    x <- x - minutes * 60
    x <- floor(x)

    parts <- Filter(function(x) !is.null(x), c(
        if (hours!=0) paste0(hours, 'h') else NULL,
        if (minutes!=0) paste0(minutes, 'm') else NULL,
        if (x!=0) paste0(x, 's') else NULL
    ))
    if (length(parts) == 0) '0s' else paste(parts, collapse=" ")
}

test_pkg_env <- function(package) {
  list2env(as.list(getNamespace(package), all.names = TRUE),
    parent = parent.env(getNamespace(package)))
}

run_package_tests <- function(src.root, verbose) {
    if (package_uses_testthat(src.root)) {
        package <- devtools::as.package(src.root)
        test_path <- file.path(src.root, "tests", "testthat")
        testthat::test_dir(test_path, filter = NULL, env = test_pkg_env(package$package))
    } else if (file.exists(file.path(src.root, "tests"))) {
        run_R_tests(src.root, verbose = TRUE)
    } else {
        if (verbose) message("Package has no tests!")
    }
}

package_uses_testthat <- function(package.dir) {
    test_path <- file.path(package.dir, "tests", "testthat")
    file.exists(test_path)
}

