
#' Extracts package from an archive and installs all the dependencies.
#'
#' @export
#' @param path Path to \emph{.tar.gz} file.
#' @param dest.dir Destination folder
#' @param quiet Suppress messages.
#' @return Returns a value of type \code{\link{fetchResult}}:
#'
#' @examples
#' get_package_from_archive("./vcrpart_0.3-3.tar.gz")
#' get_package_from_archive("./vcrpart_0.3-3.tar.gz", dest.dir="./package_sources", quiet=FALSE)
get_package_from_archive <- function(path, dest.dir = "sources", quiet = TRUE) {

    stateInfo <- list(
        pkgName = NULL,
        unpacking.succesfull = FALSE,
        depenencyInstall.succesfull = FALSE,
        build.succesfull = FALSE,
        install.succesfull = FALSE,
        package.dir = NULL
    )

    tryCatch({
        if (!file.exists(path)) {
            stop("Specified archive file doesn't exist!")
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

        message(paste0("Unpacking file: ", path, " to: ", dest.dir, "\n"))
        files <- untar(path, exdir=dest.dir, list = TRUE)
        untar(path, exdir=dest.dir)
        stateInfo$unpacking.succesfull <- TRUE

        print(files)
        source_path <- file.path(dest.dir, files[1])
        pkg_name <- devtools::as.package(source_path)$package
        package_dir <- file.path(dest.dir, pkg_name)
        stateInfo$pkgName <- pkg_name
        stateInfo$package.dir <- package_dir

        message(paste0("Output path: ", dest.dir, "\n"))

        message(paste0("Installing dependencies for ", pkg_name, "\n"))
        devtools::install_dev_deps(package_dir, quiet = quiet);
        stateInfo$depenencyInstall.succesfull <- TRUE

        message(paste0("Building ", pkg_name, "\n"))
        devtools::build(package_dir, quiet = quiet)
        stateInfo$build.succesfull <- TRUE

        message(paste0("Installing ", pkg_name, "\n"))
        install.packages(package_dir, repos = NULL, quiet = quiet)
        stateInfo$install.succesfull <- TRUE

        get_package_result(stateInfo$pkgName, stateInfo$package.dir)
    }, error = function(e) {
        print(e)
        stop(fetchError(e, stateInfo))
    })
}
