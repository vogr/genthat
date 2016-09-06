

#' @title Converts a string to a value
#'
#' @description TODO
#'
#' @param str string to parse
#' @export
deserialize <- function(str) {
    x <- eval(parse(text=str))
    traverse(x)
}

mapList <- function(lst, fn) {
    ret.val <- list()
    keys <- names(lst)
    for (i in seq(along.with = lst)) {
        if (length(keys) != 0 && keys[[i]] != "") {
            key <- keys[[i]]
            val <- lst[[key]]
            ret.val[[key]] <- fn(val)
        } else {
            key <- i
            val <- lst[[key]]
            ret.val[[key]] <- fn(val)
        }
    }
    ret.val
}

traverse <- function(x) {
    if (is.list(x)) {
        listImage <- mapList(x, traverse)

        if ("__GENTHAT_TYPE" %in% names(attributes(x))) {
            if (attr(x, "__GENTHAT_TYPE") == "<ENVIRONMENT>") {
                as.environment(listImage)
            } else {
                stop("Unknown genthat_type!")
            }
        } else {
            listImage
        }

    } else { # TODO this branch should handle only primitive values
        x
    }
}
