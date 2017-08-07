run_r_code <- function(code_to_run, save_image=FALSE, quiet=TRUE, ...) {
    # TODO: warn if there are any free variables

    code <- substitute(code_to_run)
    script <-
        if (typeof(code) == "language") {
            deparse(code)
        } else {
            deparse(code_to_run)
        }

    # TODO: save_image should be moved to run_r_script and it should be a filename
    if (save_image) {
        image_file <- tempfile(pattern="run_r_script-", fileext=".Rimage")
        script <- c(script, deparse(substitute(save.image(file=FILE), list(FILE=image_file))))
    }

    script_file <- tempfile(pattern="run_r_code-", fileext=".R")
    writeLines(script, script_file)

    run <- run_r_script(script_file, quiet=quiet, ...)

    if (run$status == 0 && save_image) {
        e <- new.env(parent=emptyenv())
        load(file=image_file, envir=e)
        run$image <- e
    }

    run
}

run_r_script <- function(script_file, args=character(), .lib_paths=NULL, quiet=TRUE) {
    stopifnot(file.exists(script_file))
    stopifnot(is.null(.lib_paths) || all(dir.exists(.lib_paths)))

    out_file <- tempfile(pattern="run_r_script-", fileext=".out")
    retval_file <- tempfile(pattern="run_r_script-", fileext=".ret")
    input_file <- tempfile(pattern="run_r_script-", fileext=".R")
    wd <- dirname(script_file)
    frame_dump_file <- paste0(input_file, ".dump")

    writeLines(
        c(
            paste0('options(error=function() { traceback(3); dump.frames("', frame_dump_file, '", TRUE); quit(status=1, save="no")})'),
            paste0('setwd("', wd, '")'),
            paste0('source("', script_file, '", echo=TRUE)')
        ),
        input_file
    )

    env <-
        if (is.null(.lib_paths)) {
            character()
        } else {
            # This is fairly counter intuitive, but
            # the problem is that in some cases some R
            # code (e.g. tools::testInstalledPackage)
            # sets R_LIBS to "" and therefore the .lib_paths
            # won't work. Therefore reset the variables
            # to be the same. This might break other things I guess
            # but so far so good.
            paths <- paste(shQuote(c(.lib_paths, .libPaths())), collapse=.Platform$path.sep)
            c(
                paste("R_LIBS", paths, sep="=")
                ## paste("R_LIBS_USER", paths, sep="="),
                ## paste("R_LIBS_SITE", paths, sep="=")
            )
        }

    env <- paste(env, collapse=" ")

    Rbin <- file.path(R.home("bin"), "R")
    command <-
        paste("(", env, " ", Rbin, "--vanilla", "--silent", "<", shQuote(input_file), "; echo $? >", shQuote(retval_file), ")",
            if (!quiet) {
                paste("2>&1 | tee", shQuote(out_file))
            } else {
                paste(">", shQuote(out_file), "2>&1")
            },
            collapse=" ")

    if (is_debug_enabled()) {
        message("run_r_script: command: ", command)

        if (!quiet) {
            message("run_r_script: source file to run: ", input_file)
            message("run_r_script: actual script: ", script_file)
            message("run_r_script: output: ", out_file)
            message("run_r_script: retval: ", retval_file)
        }
    }

    time <- system.time(status <- system(command))

    if (status != 0) {
        stop("Unable to run: ", command, " exit: ", status)
    }

    list(
        command=command,
        script=read_text_file(script_file),
        status=as.numeric(read_text_file(retval_file)),
        output=read_text_file(out_file),
        elapsed=time["elapsed"]
    )
}
