
#' Result of a \code{get_package_...(...)} function.
#'
#' @param pkgName Package name.
#' @param packageDir Location of package sources.
#' @return Returns a \code{get_package_result} instance.
#'
#' @examples
#' get_package_result("vcrpart", "./sources/vcrpart")
get_package_result <- function(pkgName, packageDir) {
    structure(list(
        pkgName = pkgName,
        package.dir = packageDir
    ), class = "get_package_result")
}

toString.get_package_result <- function(x) {
    paste(
        paste0("package: ", x$pkgName),
        paste0("package.dir: ", x$package.dir),
        sep = "\n"
    )
}

print.get_package_result <- function(x) {
    print(toString(x))
}

