#' Fetches a package from CRAN and installs all the dependencies.
#'
#' @export
#' @param pkg_name Name of package in CRAN repo.
#' @param version Optional version name. If not specified, the most recent version is fetched.
#' @param archive.dir Where to download the archive files. If not specified a temporary folder is used and it is deleted upon completion.
#' @param dest.dir Destination folder
#' @param quiet Suppress messages.
#' @return Returns a value of type \code{\link{fetchResult}}:
#'
#' @examples
#' get_package_from_cran("vcrpart")
#' get_package_from_archive("vcrpart", "0.3-3", dest.dir="./package_sources")
get_package_from_cran <- function(pkg_name, version, archive.dir = tempdir(), dest.dir = "sources", quiet = TRUE, CRAN_repo = 'http://cran.us.r-project.org') {

    stateInfo <- list(
        pkgName = pkg_name,
        sourceDownload.succesfull = FALSE,
        unpacking.succesfull = FALSE,
        depenencyInstall.succesfull = FALSE,
        build.succesfull = FALSE,
        install.succesfull = FALSE,
        package.dir = NULL
    )

    shouldRemoveArchiveDir <- FALSE

    tryCatch({
        if (missing(archive.dir)) {
            if (!file.exists(archive.dir)) {
                if (!dir.create(archive.dir)) {
                    stop("Can't create archive.dir!")
                }
                shouldRemoveArchiveDir <- TRUE
            }
        } else {
            if (!file.exists(archive.dir)) {
                stop("Archive dir doesn't exist!")
            }
        }
        if (missing(dest.dir)) {
            if (!dir.exists(dest.dir)) {
                if (!dir.create(dest.dir)) {
                    stop("Can't create dest.dir!")
                }
            }
        } else {
            if (!file.exists(dest.dir)) {
                stop("Destination directory ", dest.dir," does not exist.")
            }
        }

        contrib_url <- contrib.url(CRAN_repo, "source")
        available_pkgs <- available.packages(contrib_url)
        versions <- available_pkgs[available_pkgs[,1] == pkg_name,,drop = FALSE][,2]
        if (length(versions) == 0) {
            stop("Package unavailable in this repository.")
        }

        fetchedVersion <- if (missing(version)) Reduce(function(a,b) if (compareVersion(a, b) == -1) b else a, versions) else {
            if (!(version %in% versions))
                stop("Specified version unavailable!")
            version
        }
        archive <- sprintf("%s_%s.tar.gz", pkg_name, fetchedVersion)
        url <- sprintf("%s/%s", contrib_url, archive)

        archiveFile <- file.path(dest.dir, archive)
        message(paste0("Downloading sources for ", pkg_name, " into ", archive.dir, " \n"))
        if (0 != download.file(url, destfile = archiveFile, quiet = quiet)) {
            stop("Failed to download sources!")
        }
        stateInfo$sourceDownload.succesfull <- TRUE

        message(paste0("Unpacking ", pkg_name, " from file: ", archiveFile, "\n"))
        untar(archiveFile, exdir=dest.dir)
        stateInfo$unpacking.succesfull <- TRUE

        package_dir <- file.path(dest.dir, pkg_name)
        message(paste0("Installing dependencies for ", pkg_name, "\n"))
        devtools::install_dev_deps(package_dir, quiet = quiet);
        stateInfo$depenencyInstall.succesfull <- TRUE

        message(paste0("Building ", pkg_name, "\n"))
        devtools::build(package_dir, quiet = quiet)
        stateInfo$build.succesfull <- TRUE

        message(paste0("Installing ", pkg_name, "\n"))
        install.packages(package_dir, repos = NULL, quiet = quiet)
        stateInfo$install.succesfull <- TRUE

        message(paste0("Output path: ", dest.dir, "\n"))
        stateInfo$package.dir <- package_dir

        get_package_result(stateInfo$pkgName, stateInfo$package.dir)
    }, error = function(e) {
        print(e)
        stop(fetchError(e, stateInfo))
    }, finally = function() {
        if (createdArchiveDir) unlink(archive.dir, recursive = TRUE, force = TRUE)
    })
}

#' Lists the available versions of a package on the given CRAN mirror.
#'
#' @export
#' @param package Package name.
#' @param CRAN_repo CRAN mirror url.
#' @return Returns a list of version names.
#'
#' @examples
#' list_cran_package_versions("ggplot2")
list_cran_package_versions <- function(package, CRAN_repo = 'http://cran.us.r-project.org') {
    contrib_url <- contrib.url(CRAN_repo, "source")
    available_pkgs <- available.packages(contrib_url)
    available_pkgs[available_pkgs[,1] == package,,drop = FALSE][,2]
}

