#' @title Gets the latest package version
#'
#' @description Returns the latest version of a given package that is available in the given repository.
#' @export
#'
get_package_version <- function(package, repos=getOption("repos"), type="source") {
    contrib_url <- utils::contrib.url(repos, type)
    available_pkgs <- available.packages(contrib_url)

    if (package %in% row.names(available_pkgs)) {
        available_pkgs[package, "Version"]
    } else {
        NULL
    }
}

#' @title Downloads R package
#'
#' @description Downloads R package.
#' @export
#' @importFrom utils available.packages contrib.url download.file untar
#'
download_package <- function(package, destdir, version=NULL, repos=getOption("repos"),
                            type="source", extract=TRUE, force=FALSE, quiet=TRUE, ...) {

    contrib_url <- contrib.url(repos, "source")
    latest_version <- get_package_version(package, repos, "source")

    if (is.null(version)) {
        version <- latest_version
        if (is.null(version)) {
            stop("Package ", package, " is not in the CRAN available packages (", repos, ") and no version was specified")
        }
    } else if (is.null(latest_version) || version != latest_version) {
        # only the latest versions are kept in the CRAN top level
        # the rest is in the Archive
        contrib_url <- sprintf("%s/Archive/%s", contrib_url, package)
    }

    archive <- sprintf("%s_%s.tar.gz", package, version)
    url <- sprintf("%s/%s", contrib_url, archive)

    if (!dir.exists(destdir)) {
        if (!quiet) {
            cat("Creating ", destdir, "\n")
        }
        dir.create(destdir)
    }

    destfile <- file.path(destdir, archive)
    if (file.exists(destfile)) {
        if (force) {
            if (!quiet) {
                cat("Removing package archive", destfile)
            }
            file.remove(destfile)
        } else {
            cat("File already exists ", destfile, "\n")
        }
    }

    if (quiet) {
        cat("Downloading package", package, "version", version ," from ", url, "\n")
    }

    if (utils::download.file(url, destfile=destfile, quiet=quiet, ...)) {
        stop("Download from ", url, " failed")
    }

    if (extract) {
        pkgdir <- file.path(destdir, package)
        if (dir.exists(pkgdir)) {
            if (force) {
                if (!quiet) {
                    cat("Removing directory ", pkgdir, "\n")
                }
                unlink(pkgdir, recursive=TRUE)
            } else {
                cat("Destination directory for extracting exists ", pkgdir, "\n")
            }
        }

        if (!quiet) {
            cat("Extracing", archive ," to ", pkgdir, "\n")
        }

        utils::untar(destfile, exdir=destdir, verbose=!quiet)
        pkgdir
    } else {
        archive
    }
}
