
GENTHAT_SERIALIZE_ERROR <- "GENTHAT_SERIALIZE_ERROR"

isVisited <- function(x, visited) {
    any(as.logical(Map(function(e) { identical(e, x) }, visited)))
}

deparse <- function(obj) concatVec(base::deparse(obj))

serialize.isValid <- function(res) {
    classes <- class(res)
    any(classes == GENTHAT_SERIALIZE_ERROR)
}

serialize.getError <- function(res) {
    msg <- res$message
    if ("<cyclic_structure>" == msg)
        "tried to serialize cyclic structure"
    if ("<func>" == msg)
        "tried to serialize a function"
    if ("<unknown_type>" == msg)
        "tried to serialize a value of unhandled type"
    stop("not a valid error code")
}


# maybe fmap
`%>>=%` <- function(res, fn) {
    if (serialize.isValid(res)) {
        fn(res)
    } else {
        res
    }
}

# [Maybe a] -> Maybe [a]
merge <- function(xs) {
    invalid.elements <- !sapply(xs, serialize.isValid)
    if (any(invalid.elements)) {
        xs[invalid.elements][1]
    } else {
        xs
    }
}

errorResult <- function(type) {
    structure(list(message = type, then = ), class = GENTHAT_SERIALIZE_ERROR)
}


#' @title Converts a value to a string
#'
#' @description TODO
#'
#' @param obj Value to serialize
#' @export
serialize <- function(obj) {
    .serialize(obj)
}

.serialize <- function(obj, visited = list()) {
    if (is.environment(obj)) {
        if (isVisited(obj, visited)) {
            errorResult("<cyclic_structure>")
        }
        visited <- c(visited, obj, recursive = F)

        serializeEnvironment(obj, visited)
    } else if (is.list(obj)) {
        serializeList(obj, visited)
    } else if (is.function(obj)) {
        errorResult("<func>")
    } else if (is.vector(obj) || is.array(obj)) {
        if (length(obj) <= 1) {
            deparse(obj)
        } else {
            serialized.elements <- sapply(obj, function(x) .serialize(x, visited))
            merge(serialized.elements) %>>=% function(xs) {
                concat("c(", concatVec(serialized.elements, sep=", "), ")")
            }
        }
    } else if (is.factor(obj)) {
        deparse(obj)
    } else if (is.null(obj)) {
        "NULL"
    } else {
        errorResult("<unknown_type>")
    }
}

serializeList <- function(lst, visited) {
    keys <- names(lst)
    pairs <- c()
    for (i in seq(along.with=lst)) {
        val <- lst[[i]]
        .serialize(val, visited) %>>=% function(serialized.val) {
            if (length(keys) != 0 && keys[[i]] != "") {
                key <- keys[[i]]
                pairs[key] <- concat(key, "=", serialized.val)
            } else {
                key <- i
                pairs[key] <- serialized.val
            }
        }
    }

    listLiteral <- paste(c("list(", paste(pairs, collapse=", "), ")"), collapse="")

    merge(lapply(attributes(lst), function(x) .serialize(x, visited))) %>>=% function(atts) {
        atts[['names']] <- NULL
        if (length(atts) < 1) {
            listLiteral
        } else {
            att.names <- lapply(names(atts), escapeNonSyntacticName)
            attributesAsString <- paste(att.names, atts, sep="=", collapse=", ")
            paste(c("structure(", listLiteral, ", ", attributesAsString, ")"), collapse="")
        }
    }
}

serializeEnvironment <- function(env, visited) {
    lst <- as.list(env, all.names=T)
    attr(lst, "__GENTHAT_TYPE") <- "<ENVIRONMENT>"
    serializeList(lst, visited)
}

