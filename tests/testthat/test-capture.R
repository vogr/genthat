library(testr)
library(testthat)

context("Capture")

test_capture_builtin <- function(functions) {
    testr_options("verbose", F)
    suppressWarnings(setup_capture(functions))
    check.dec <- sapply(functions, function(x) {
        if (!exists(x, envir = getNamespace('base')))
            return(TRUE)
        obj <- get(x, envir = getNamespace('base'))
        if (is.function(obj)) {
            if (class(obj) == "functionWithTrace")
                return(TRUE)
            if (!testr:::eligible_capture(x))
                return(TRUE)
            testr:::is_s3_generic(x)
        } else {
            TRUE
        }
    })
    expect_true(length(testr:::.decorated) > 0)
    expect_true(all(check.dec))
    stop_capture_all()
    check.dec <- sapply(functions, function(x) {
        if (!exists(x, envir = getNamespace('base')))
            return(FALSE)
        obj <- get(x, envir = getNamespace('base'))
        if (is.function(obj))
            class(obj) == "functionWithTrace" && x != "library"
        else
            FALSE
    })
    expect_true(length(testr:::.decorated) == 0)
    expect_false(any(check.dec))
}

test_that('unknown error', {
    setup_capture("any")
    testr:::is_s3_generic('sort')
})

test_that('Can decorate functions (long)', {
    skip_on_cran()
    # TODO reenable!!
    #functions <- builtins()
    #test_decoration(functions)
})

test_that('Can decorate functions', {
    # to get rid of randomness
    functions <- c("any", "alist", "double", "deparse", "is.logical", "isOpen", "log2", "mean.Date", "pmax", "sort", "sweep", "unsplit")
    test_capture_builtin(functions)
})

test_that('Capture writes down all the calls for testthat:::comparison', {
    capture_dir <- file.path("temp", "capture")
    unlink(capture_dir, recursive = TRUE, force = TRUE)
    dir.create(capture_dir, recursive = TRUE)
    testr::testr_options("capture.folder", capture_dir)
    trace("comparison", where = asNamespace("testthat"))
    trace_example <- capture.output(suppressWarnings(example(compare)))
    untrace("comparison", where = asNamespace("testthat"))
    testr::start_capture(testthat:::comparison)
    out <- capture.output(suppressWarnings(example(compare)))
    testr::stop_capture_all()
    lines <- readLines(file.path(capture_dir, "capture.0"))
    expect_equal(length(grep(pattern = "Tracing comparison", trace_example)),
                 length(grep(pattern = "func", lines)))
    unlink(capture_dir, recursive = TRUE, force = TRUE)
})

