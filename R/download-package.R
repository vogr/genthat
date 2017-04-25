#' @export
get_package_version <- function(package, repos=getOption("repos"), type="source") {
    contrib_url <- contrib.url(repos, type)
    available_pkgs <- available.packages(contrib_url)

    if (package %in% row.names(available_pkgs)) {
        available_pkgs[package, "Version"]
    } else {
        NULL
    }
}

#' @export
download_package <- function(package, destdir, version=NULL, repos=getOption("repos"),
                            type="source", extract=TRUE, force=FALSE, quiet=TRUE, ...) {        
    contrib_url <-
        if (is.null(version)) {
            version <- get_package_version(package, repos, type)
            if (is.null(version)) {
                stop("Package ", package, " is not in the available in ", repos, " (perhaps it has been archived?)")
            }
            contrib.url(repos, type)
        } else if (is.null(latest_version) || version != latest_version) {
            ## only the latest versions are kept in the CRAN top level
            ## the rest is in the Archive
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
            stop("File already exists ", destfile)
        }
    }

    if (quiet) {
        cat("Downloading package", package, "version", version ," from ", url, "\n")
    }

    if (download.file(url, destfile=destfile, quiet=quiet, ...)) {
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
                stop("Destination directory for extracting exists ", pkgdir)
            }
        }

        if (!quiet) {
            cat("Extracing", archive ," to ", pkgdir, "\n")
        }

        untar(destfile, exdir=destdir, verbose=!quiet)
    }

    # TODO: can we remove this dependency
    devtools::as.package(pkgdir)
}
