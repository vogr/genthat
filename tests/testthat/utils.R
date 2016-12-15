
# TODO find better way to get path to sources
testthat_folder <- getwd()
get_testthat_folder <- function() {
    testthat_folder
}

get_only <- function(xs) {
    expect_equal(length(xs), 1)
    xs[[1]]
}

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

test_capturing <- function(fn) {
    tname <- tempfile(pattern = "genthat-sandbox-")
    if (!dir.create(tname)) {
        stop("Couldn't create sandbox directory.")
    }
    oldwd <- getwd()
    setwd(tname)
    retVal <- tryCatch({
        fn(tname)
    }, error = function(e) {
        print("error!: ")
        print(e)
        stop(e)
    }, finally = {
        setwd(oldwd)
        unlink(tname, recursive = TRUE, force = TRUE)
    })
    retVal
}

#' Creates a pair consisting of an expression \code{expression} that when evaluated increments a counter.
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

