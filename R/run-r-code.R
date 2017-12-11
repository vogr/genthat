run_r_fun <- function(fun, ...) {
    stopifnot(is.function(fun))
    stopifnot(length(formals(fun)) == 0)

    code <- deparse(body(fun))
    if (code[1] == "{") {
        code <- code[-c(1, length(code))]
    }

    script <- tempfile()
    on.exit(file.remove(script))

    writeLines(code, script)
    run_r_script(script, ...)
}

run_r_script <- function(script_file,
                         args=c("--silent", "--no-environ", "--no-init-file", "--no-restore", "--no-save"),
                         site_file=NULL, env=c(), quiet=TRUE, lib_paths=NULL) {

    stopifnot(file.exists(script_file))
    stopifnot(is.null(lib_paths) || all(dir.exists(lib_paths)))

    if (is.null(lib_paths)) {
        lib_paths <- .libPaths()
    }

    # This is fairly counter intuitive, but the problem is that in some cases
    # some R code (e.g. tools::testInstalledPackage) sets R_LIBS to "" and
    # therefore the .lib_paths won't work. Therefore reset the variables to
    # be the same. This might break other things I guess but so far so good.
    paths <- paste(shQuote(lib_paths), collapse=.Platform$path.sep)
    env <- c(env,
        paste("R_LIBS", paths, sep="=")

        # TODO: not sure if we need this as well - probably on windows?
        ## paste("R_LIBS_USER", paths, sep="="),
        ## paste("R_LIBS_SITE", paths, sep="=")
    )

    if (!is.null(site_file)) {
        env <- c(env, paste0("R_PROFILE=", site_file))
    }

    # When R CMD check runs tests, it sets R_TESTS. When the tests
    # themeselves run R CMD xxxx, as is the case with the tests in
    # devtools, having R_TESTS set causes errors because it confuses
    # the R subprocesses. Unsetting it here avoids those problems.
    if (Sys.getenv("R_TESTS") != "") {
        env <- c(env, "R_TESTS=''")
    }

    owd <- setwd(dirname(script_file))
    on.exit(setwd(owd))

    Rbin <- file.path(R.home("bin"), "R")

    stdout <- FALSE
    stderr <- FALSE
    if (!quiet) {
        stdout <- ""
        stderr <- ""

        log_debug(
            "Running: ",
            paste(env, collapse=" "), " ",
            Rbin, " ",
            paste(args, collapse=" "), " < ", script_file
        )
    }

    system2(Rbin, args, stdout=stdout, stderr=stderr, stdin=script_file, env=env)
}
