

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

