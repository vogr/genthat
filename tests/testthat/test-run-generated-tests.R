context("run-generated-tests")

run_generated_code <- function(code) {
    tmp <- tempfile()
    on.exit(file.remove(tmp))

    writeLines(code, tmp)
    run_generated_test(tmp)
}

test_that("empty test does not run", {
    x <- run_generated_code("")

    expect_true(is.na(x$test))
    expect_true(is.na(x$nb))
    expect_true(is.na(x$failed))
    expect_true(is.na(x$error))
    expect_true(is.na(x$warning))
    expect_true(is.na(x$elapsed))
    expect_true(is.na(x$output))
    expect_equal(x$run_error, "testthat::test_file result was empty")
})

test_that("test failure", {
    x <- run_generated_code("test_that('x', expect_false(TRUE))")

    expect_equal(x$test, "x")
    expect_equal(x$nb, 1)
    expect_equal(x$failed, 1)
    expect_false(x$error)
    expect_equal(x$warning, 0)
    expect_true(is.na(x$run_error))
    expect_true(grepl("TRUE isn't false", x$output))
})

test_that("test exception", {
    x <- run_generated_code("test_that('x', stop('problem'))")

    expect_equal(x$test, "x")
    expect_equal(x$nb, 0)
    expect_equal(x$failed, 0)
    expect_true(x$error)
    expect_equal(x$warning, 0)
    expect_true(is.na(x$run_error))
})

test_that("test no tests", {
    x <- run_generated_code("test_that('x', 1+1)")

    expect_equal(x$test, "x")
    # it sets skipped
    expect_equal(x$nb, 1)
    expect_equal(x$failed, 0)
    expect_false(x$error)
    expect_equal(x$warning, 0)
    expect_true(is.na(x$run_error))
})

test_that("test warning", {
    x <- run_generated_code("test_that('x', {warning('w1'); expect_true(TRUE)})")

    expect_equal(x$test, "x")
    expect_equal(x$nb, 2)
    expect_equal(x$failed, 0)
    expect_false(x$error)
    expect_equal(x$warning, 1)
    expect_true(is.na(x$run_error))
})

test_that("test success", {
    x <- run_generated_code("test_that('x', expect_true(TRUE))")

    expect_equal(x$test, "x")
    expect_equal(x$nb, 1)
    expect_equal(x$failed, 0)
    expect_false(x$error)
    expect_equal(x$warning, 0)
    expect_true(is.na(x$run_error))
})
