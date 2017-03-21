
#' Returns the root of the folder containing all the
#' testthat tests.
#' This works because testthat starts running
#' the test code in the folder where those tests are located.
testthat_folder <- getwd()
get_testthat_folder <- function() {
    # TODO find better way to get path to sources
    testthat_folder
}

get_example_package_path <- function() {
    file.path(get_testthat_folder(), "example-package")
}

#' Asserts that the array is of length 1 and returns its
#' only element.
get_only <- function(xs) {
    expect_equal(length(xs), 1)
    xs[[1]]
}

#' Runs a test file with a mock of testthat and returns a
#' boolean value signaling whether all the assertions
#' encountered during the execution were satisfied.
run_test_file <- function(path, locals = list()) {
    asserts <- logical(0)
    gen_testing_env <- function(locals) {
        testthat_bindings <- list(
            context = function(ctx) {},
            test_that = function(desc, expr) { expr },
            expect_equal = function(a, b) { asserts <<- c(asserts, identical(a, b)) }
        )
        list2env(c(testthat_bindings, locals), parent = globalenv())
    }
    eval(parse(path), gen_testing_env(locals))
    all(asserts)
}

#' Creates a temporary directory that exists only during the
#' execution of the passed function. The name of the
#' directory is passed as an arguement to the function.
with_tempdir <- function(fn) {
    tname <- tempfile(pattern = "genthat-tempdir-")
    if (!dir.create(tname)) {
        stop("Couldn't create temporary directory.")
    }
    oldwd <- getwd()
    setwd(tname)
    retVal <- tryCatch({
        fn(tname)
    }, error = function(e) {
        stop(e)
    }, finally = {
        setwd(oldwd)
        unlink(tname, recursive = TRUE, force = TRUE)
    })
    retVal
}

#' Creates a pair consisting of an expression
#' \code{expression} that when evaluated increments a counter,
#' and a function \code{getCount} that returns the current counter value.
get_spy_expression <- function() {
    counter <- 0
    inc_expr <- substitute(
        { eval(quote(counter <- counter + 1), envir=e) },
        as.environment(list(e = environment()))
    )
    list(
        expression = inc_expr,
        fn = function(...) eval(inc_expr),
        getCount = function() counter
    )
}

read_file <- function(fileName) {
    readChar(fileName, nchars = file.info(fileName)$size)
}

