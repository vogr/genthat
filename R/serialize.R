
serialize <- function(obj) {
    if (is.environment(obj)) {
        serializeEnvironment(obj)
    } else if (is.list(obj)) {
        serializeList(obj)
    } else {
        deparse(obj)
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
        paste('`', name, '`', sep="")
    }
}

serializeList <- function(lst) {
    keys <- names(lst)
    pairs <- c()
    for (i in seq(along.with=lst)) {
        if (length(keys) != 0 && keys[[i]] != "") {
            key <- keys[[i]]
            val <- lst[[key]]
            pairs[key] <- paste(key, "=", serialize(val))
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

