context("run-generated-tests")

test_that("empty test does not run", {
    x <- run_generated_test("")

    expect_equal(x$status, -1)
    expect_equal(x$error, "testthat::test_file result was empty")
})

test_that("expectation failure", {
    x <- run_generated_test("expect_false(TRUE)")

    expect_equal(x$status, -2)
    expect_equal(x$error, "testthat::test_file expectation failure:  TRUE isn't false.\n")
})

test_that("test failure", {
    x <- run_generated_test("test_that('x', expect_false(TRUE))")

    expect_equal(x$status, 1)
    expect_true(is.na(x$error))
})

test_that("test exception", {
    x <- run_generated_test("test_that('x', stop('problem'))")

    expect_equal(x$status, 2)
    expect_true(is.na(x$error))
})

test_that("test no tests", {
    x <- run_generated_test("test_that('x', 1+1)")

    expect_equal(x$status, 3)
    expect_true(is.na(x$error))
})

test_that("test no success", {
    x <- run_generated_test("test_that('x', expect_true(TRUE))")

    expect_equal(x$status, 0)
    expect_true(is.na(x$error))
})

