
GENTHAT_SERIALIZE_ERROR <- "GENTHAT_SERIALIZE_ERROR"

isVisited <- function(x, visited) {
    any(as.logical(Map(function(e) { identical(e, x) }, visited)))
}

deparse <- function(obj) concatVec(base::deparse(obj))

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

#' @title Converts a value to a string
#'
#' @description TODO
#'
#' @param obj Value to serialize
#' @export
serialize <- function(obj) {
    tryCatch(.serialize(obj), error = function(e) { .Call("testr_serialize_error_handler", e$message) })
}

#' @title Changes the error message in the global buffer
#'
#' @description TODO
#'
#' @param errMsg new error message
#' @export
replaceError <- function(errMsg) {
    tryCatch(stop(errMsg), error = function(e) {})
}

.serialize <- function(obj, visited = list()) {
    if (is.environment(obj)) {
        if (isVisited(obj, visited)) {
            stop("<cyclic_structure>")
        }
        visited <- c(visited, obj, recursive = F)

        serializeEnvironment(obj, visited)
    } else if (is.list(obj)) {
        serializeList(obj, visited)
    } else if (is.function(obj)) {
        stop("<func>")
    } else if (is.vector(obj) || is.array(obj)) {
        if (length(obj) <= 1) {
            deparse(obj)
        } else {
            serialized.elements <- sapply(obj, function(x) .serialize(x, visited))
            vec <- concat("c(", concatVec(serialized.elements, sep=", "), ")")
            if (is.null(attributes(obj))) {
                vec
            } else {
                attr.assignments <- Map(function(att.name) {
                    att.value <- .serialize(attr(obj, att.name), visited = visited)
                    paste0(", ", att.name, " = ", att.value)
                }, names(attributes(obj)))
                paste0("structure(", vec, paste(attr.assignments, collapse=""), ")")
            }
        }
    } else if (is.factor(obj)) {
        deparse(obj)
    } else if (is.null(obj)) {
        "NULL"
    } else {
        stop("<unknown_type>")
    }
}

serializeList <- function(lst, visited) {
    keys <- names(lst)
    pairs <- c()
    for (i in seq(along.with=lst)) {
        val <- lst[[i]]
        serialized.val <- .serialize(val, visited)
        if (length(keys) != 0 && keys[[i]] != "") {
            # TODO keys containing special chars must be quoted
            key <- keys[[i]]
            pairs[key] <- concat(key, "=", serialized.val)
        } else {
            key <- i
            pairs[key] <- serialized.val
        }
    }

    listLiteral <- paste(c("list(", paste(pairs, collapse=", "), ")"), collapse="")

    atts <- lapply(attributes(lst), function(x) .serialize(x, visited))

    atts[['names']] <- NULL
    if (length(atts) < 1) {
        listLiteral
    } else {
        att.names <- lapply(names(atts), escapeNonSyntacticName)
        attributesAsString <- paste(att.names, atts, sep="=", collapse=", ")
        paste(c("structure(", listLiteral, ", ", attributesAsString, ")"), collapse="")
    }
}

serializeEnvironment <- function(env, visited) {
    lst <- as.list(env, all.names=T)
    attr(lst, "__GENTHAT_TYPE") <- "<ENVIRONMENT>"
    serializeList(lst, visited)
}

