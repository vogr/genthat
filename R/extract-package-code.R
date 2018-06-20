#' Extracts code from examples, tests and/or vignettes for a given package.
#'
#' This function extracts code from package examples, tests and/or vignettes as
#' runnable R scripts. It can extract the code for already installed packages,
#' given that it has been installed with \code{INSTALL_opts=c("--examples",
#' "--install-tests")} and with built vignettes. Alternatively, it can extract
#' the same from a package source. In such a case it will attempt to temporarily
#' install the package.
#'
#' @param package the name of the package.
#' @param dir the path to the directory containing the source of the package
#' @param lib_paths a character vector describing the location of R library
#'     trees to search through when locating `package`, or `NULL`. The default
#'     value of `NULL`` corresponds to checking the loaded namespace, then all
#'     libraries currently known in `.libPaths()`.
#' @param types which code artifacts to extract, `tests`, `vignettes`,
#'     `examples`, `all`.
#' @param output_dir target directory where to put the extracted source. Default
#'     is the current directory.
#' @param filter a regular expression matching desired filenames without the
#'     '.R' extension.
#'
#' @return Returns a list where each element is a character vector of file names
#'     where the code has been extracted. The element names correspond to the
#'     elements in the types parameter.
#'
#' @importFrom devtools as.package install_local
#' @importFrom tools file_path_sans_ext
#' @importFrom withr with_temp_libpaths
#'
#' @export
#'
# TODO: support commentDontrun, commentDonttest
extract_package_code <- function(package, dir, lib_paths=NULL,
                                 types=c("examples", "tests", "vignettes", "all"),
                                 output_dir=".",
                                 filter=NULL) {

    stopifnot(is.null(lib_paths) || is.character(lib_paths))
    stopifnot(is_chr_scalar(output_dir))
    stopifnot(is.null(filter) || is_chr_scalar(filter))

    if ((missing(package) && missing(dir))
        || (!missing(package) && !missing(dir))) {
        stop("Either package or dir must be provided")
    }

    if ("all" %in% types) {
        types <- c("examples", "tests", "vignettes")
    }
    types <- match.arg(types, c("examples", "tests", "vignettes"), several.ok=TRUE)
    # so the return list is named
    names(types) <- types

    files <- if (!missing(package)) {
        # extracting from package / lib_paths

        if (!missing(dir)) stop("you must specify either 'package' or 'dir'")

        extract_code(package, lib_paths=lib_paths, types=types, output_dir=output_dir)
    } else if (!missing(dir)) {
        # extracting from source

        if (!missing(lib_paths)) stop("'lib_paths' are only used with 'package' option")
        pkg <- devtools::as.package(dir)

        withr::with_temp_libpaths({
            # install package to the temp lib with all code
            devtools::install_local(
                dir,
                build_vignettes=("vignettes" %in% types),
                keep_source=TRUE,
                INSTALL_opts=c(
                    if ("examples" %in% types) "--example" else character(),
                    if ("tests" %in% types) "--install-tests" else character()
                ),
                quiet=TRUE
            )

            # by passing NULL we rely on the .libPaths() which should be set to
            # a temp dir
            extract_code(pkg$package, lib_paths=NULL, types=types, output_dir=output_dir)
        })
    } else {
        stop("you must specify 'package' or 'dir'")
    }

    if (!is.null(filter)) {
        files <- lapply(files, function(x) x[grepl(filter, tools::file_path_sans_ext(x))])
    }

    files
}

extract_code <- function(package, lib_paths, types, output_dir) {
    lapply(types, function(type) {
        fun <- switch(
            type,
            examples=extract_package_examples,
            tests=extract_package_tests,
            vignettes=extract_package_vignettes
        )

        # each type has its own folder not to clash with one another
        output <- file.path(output_dir, type)
        path <- find.package(package, lib.loc=lib_paths)
        stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))

        files <- fun(
            package,
            path=path,
            lib_paths=lib_paths,
            output_dir=output
        )

        names(files) <- NULL
        files
    })
}


#' @importFrom tools Rd_db Rd2ex
#' @importFrom stats na.omit
extract_package_examples <- function(package, path, lib_paths, output_dir) {
    db <- tryCatch({
        tools::Rd_db(package, lib.loc=lib_paths)
    }, error=function(e) {
        c()
    })

    if (!length(db)) {
        return(character())
    }

    files <- names(db)

    examples <- sapply(files, function(x) {
        f <- file.path(output_dir, paste0(basename(x), ".R"))
        tools::Rd2ex(db[[x]], f, defines=NULL)

        if (!file.exists(f)) {
            if (is_debug_enabled()) {
                log_debug("Rd file `", x, "' does not contain any code to be run")
            }
            NA
        } else {
            # prepend the file with library call
            txt <- c(
                paste0("library(", package, ")"),
                "",
                "",
                readLines(f)
            )
            writeLines(txt, f)
            f
        }
    })

    na.omit(examples)
}

extract_package_tests <- function(package, path, lib_paths, output_dir) {
    test_dir <- file.path(path, "tests")

    if (!dir.exists(test_dir)) {
        return(character())
    }

    files <- Sys.glob(file.path(test_dir, "*"))
    file.copy(files, output_dir, recursive=TRUE)

    tests <- file.path(output_dir, basename(files))
    tests <- tests[!dir.exists(tests)]
    tests <- tests[grepl("\\.R$", tests)]

    tests
}

#' @importFrom tools pkgVignettes checkVignettes
extract_package_vignettes <- function(package, path, lib_paths, output_dir) {
    vinfo <- tools::pkgVignettes(package, source=T, lib.loc=lib_paths)
    if (length(vinfo$docs) == 0) {
        return(character())
    }

    if (length(vinfo$sources) == 0) {
        # so far no sources. The following should generate them if there are any
        # sources in the R code. It might actually run the vignettes as well.
        # That is a pity, but there is no way to tell it not to (the tangle is
        # needed to it extracts the R code)
        tools::checkVignettes(
            package,
            lib.loc=lib_paths,
            tangle=TRUE,
            weave=FALSE,
            workdir="src"
        )
    }

    # check if there are some sources
    vinfo <- tools::pkgVignettes(package, source=T, lib.loc=lib_paths)
    files <- as.character(unlist(vinfo$sources))
    if (length(files) == 0) {
        return(character())
    }

    file.copy(files, to=output_dir)
    vignettes <- file.path(output_dir, basename(files))

    vignettes
}
