
fetchResult <- function(stateInfo) {
    structure(list(
        pkgName = stateInfo$pkgName,
        package.dir = stateInfo$package.dir
    ), class = "fetchResult")
}

print.fetchResult <- function(x) {
    print(toString(x))
}

toString.fetchResult <- function(x) {
    paste(
        paste0("package: ", x$pkgName),
        paste0("package.dir: ", x$package.dir),
        sep = "\n"
    )
}

fetchError <- function(message, stateInfo) {
    structure(list(message = message, call = sys.call(-1)), class = c("fetchError", "error", "condition"))
}

print.fetchError <- function(x) {
    print(toString(x))
}

toString.fetchError <- function(x) {
    if (!x$sourceDownload.succesfull) return(paste0("Couldn't download sources for package: ", x$pkgName))
    if (!x$unpacking.succesfull) return(paste0("Couldn't unpack sources for package: ", x$pkgName))
    if (!x$depenencyInstall.succesfull) return(paste0("Couldn't install package dependencies for: ", x$pkgName))
    if (!x$build.succesfull) return(paste0("Couldn't build package: ", x$pkgName))
    if (!x$install.succesfull) return(paste0("Couldn't install package: ", x$pkgName))
    else return("Unknown error while fetching package.")
}

maxByComparison <- function(elems, compFn) {
    if (length(elems) == 0) stop("max of empty array")
    maxElem <- elems[1]
    for (i in range(2, length(elems))) {
        if (compFn(maxElem, elems[i]) == -1) {
            maxElem <- elems[i]
        }
    }
    maxElem
}

fetchCRANPackage <- function(pkg_name, version, archive.dir = tempdir(), dest.dir = "sources", quiet = TRUE, CRAN_repo = 'http://cran.us.r-project.org') {

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
        fetchedVersion <- if (missing(version)) maxByComparison(versions, compareVersion) else {
            if (!(version %in% versions))
                stop("Specified version unavailable!")
            version
        }
        archive <- sprintf("%s_%s.tar.gz", pkg_name, fetchedVersion)
        url <- sprintf("%s/%s", contrib_url, archive)

        stop("moje chyba!")

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

        fetchResult(stateInfo)
    }, error = function(e) {
        print(e)
        stop(fetchError(e, stateInfo))
    }, finally = function() {
        if (createdArchiveDir) unlink(archive.dir, recursive = TRUE, force = TRUE)
    })
}
