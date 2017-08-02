#!/usr/bin/env Rscript

options(error=function() { traceback(3); quit(status=1, save="no") })

suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

tryCatch(library(genthat), error=function(e) devtools::load_all())

default_or_val <- function(val, default) {
    if (is.null(val)) {
        default
    } else {
        val
    }
}

do_trace_package <- function(package, type, clean) {
    time_stamp <- Sys.time()

    run <- genthat::run_package(package, types=type, quiet=FALSE, clean=clean)

    row <- data_frame(
        time_stamp,
        type,
        package,
        status=run[[type]]$status,
        output=run[[type]]$output,
        time=run[[type]]$elapsed)

    field_types <- as.list(sapply(names(row), function(x) dbDataType(RMySQL::MySQL(), row[[x]])))
    field_types$output <- "longtext"
    field_types$status <- "integer"

    attr(row, "types") <- field_types
    row
}

option_list <-
    list(
        make_option("--db-host", type="character", help="DB hostname", metavar="HOST"),
        make_option("--db-port", type="integer", help="DB port", metavar="PORT"),
        make_option("--db-name", type="character", help="DB name", metavar="NAME"),
        make_option("--db-user", type="character", help="DB username", metavar="USER"),
        make_option("--db-password", type="character", help="DB password", metavar="PASSWORD"),
        make_option("--package", type="character", help="Package to trace", metavar="PATH"),
        make_option("--type", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
        make_option("--no-clean", help="Leave the temporary files in place", action="store_true", default=FALSE),
        make_option(c("-d", "--debug"), help="Debug output", action="store_true", default=FALSE)
    )

parser <- OptionParser(option_list=option_list)
opt <- {
    tryCatch({
        parse_args(parser)
    }, error=function(e) {
        message("Error: ", e$message)
        print_help(parser)
        quit(save="no", status=1)
    })
}

package <- opt$package
type <- match.arg(opt$type, c("examples", "tests", "vignettes"), several.ok=FALSE)
clean <- !opt$`no-clean`

options(genthat.debug=opt$`debug`)

db <-
    dbConnect(
        RMySQL::MySQL(),
        dbname=opt$`db-name`,
        host=opt$`db-host`,
        port=opt$`db-port`,
        user=opt$`db-user`,
        password=opt$`db-password`
    )

db_info <- dbGetInfo(db)
message("Connected to ", db_info$dbname, "@", db_info$conType)

tryCatch({
    message("Running ", package, " ", type)

    time <- system.time(row <- do_trace_package(package, type, clean))

    i <- 0
    while(i < 3) {
        tryCatch({
            dbWriteTable(db, name="runs", value=row, append=TRUE, row.names=FALSE, field.types=attr(row, "types"))
            break()
        }, error=function(e) {
            message("Storing to DB did not work: ", e$message, " - retrying")
            i <<- i + 1
        })
    }

    message("\n\n Running of ", package, " ", type, " finished in ", time["elapsed"])
}, error=function(e) {
    message("Running of ", package, " ", type, " failed with: ", e$message, "\n")
    stop(e$message)
})
