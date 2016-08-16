
e_msg <- function(x) TRUE 

concat <- function(...) {
    for (x in as.list(...)) {
        stopifnot(length(x) == 1 && e_msg("Got vector argument to concat!"))
    }
    ret <- paste0(..., collapse="")
    stopifnot(length(ret) == 1 && e_msg("Result of concat() is not single string!"))
    ret
}

concatVec <- function(xs) {
    ret <- paste(xs, collapse="")
    stopifnot(length(ret) == 1 && e_msg("Result of concatVec() is not single string!"))
    ret
}

#' @title Converts a value to a string
#'
#' @description TODO
#'
#' @param obj Value to serialize
#' @export
serialize <- function(obj) {
    if (is.environment(obj)) {
        serializeEnvironment(obj)
    } else if (is.list(obj)) {
        serializeList(obj)
    } else {
        concatVec(deparse(obj))
    }
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/Quotes.html
testIsSyntacticName <- function(name) {
    syntacticNameRegex <- '^([a-zA-Z][a-zA-Z0-9._]*|[.]([a-zA-Z._][a-zA-Z0-9._]*)?)$'
    reservedWords <- c(
        'if', 'else', 'repeat', 'while', 'function', 'for', 'in', 'next',
        'break', 'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA', 'NA_integer_',
        'NA_real_', 'NA_complex_', 'NA_character_'
    )
    isReservedWord <- is.element(name, reservedWords)
    matchesRegex <- length(grep(syntacticNameRegex, name, perl=TRUE))
    !isReservedWord && matchesRegex
}

escapeNonSyntacticName <- function(name) {
    isSyntacticName <- testIsSyntacticName(name)
    if (isSyntacticName) {
        name
    } else {
        concat('`', name, '`')
    }
}

serializeList <- function(lst) {
    keys <- names(lst)
    pairs <- c()
    for (i in seq(along.with=lst)) {
        if (length(keys) != 0 && keys[[i]] != "") {
            key <- keys[[i]]
            val <- lst[[key]]
            pairs[key] <- concat(key, "=", serialize(val))
        } else {
            key <- i
            val <- lst[[key]]
            pairs[key] <- serialize(val)
        }
    }

    listLiteral <- paste(c("list(", paste(pairs, collapse=", "), ")"), collapse="")

    atts <- lapply(attributes(lst), serialize)
    atts[['names']] <- NULL
    if (length(atts) < 1) {
        listLiteral
    } else {
        att.names <- lapply(names(atts), escapeNonSyntacticName)
        attributesAsString <- paste(att.names, atts, sep="=", collapse=", ")
        paste(c("structure(", listLiteral, ", ", attributesAsString, ")"), collapse="")
    }
}

serializeEnvironment <- function(env) {
    lst <- as.list(env, all.names=T)
    attr(lst, "__GENTHAT_TYPE") <- "<ENVIRONMENT>"
    serialize(lst)
}

