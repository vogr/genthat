
library(testthat)
library(devtools)

source("./utils.R")

context("Function decoration API")

test_that("undecorate_all()", {
    load_all("./example-package", TRUE,  export_all = FALSE, quiet = TRUE)
    fn1 <- function() {}

    decorate_function_env("fn1", env = environment())
    decorate_exported("examplePackage", all = TRUE)
    decorate_hidden_functions("examplePackage")

    expect_true(is_decorated(fn1))
    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_true(is_decorated(examplePackage:::private_fn))

    undecorate_all()

    expect_false(is_decorated(fn1))
    expect_false(is_decorated(examplePackage::my_add))
    expect_false(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

# TODO filip - what is the expected behaviour of this ?
#decorate_functions("hidden_fn1", "hidden_fn2", package="package_name", include_hidden=TRUE)

test_that("decorate_functions() - function value", {
    fn1 <- function() {}
    fn2 <- decorate_functions(fn1)[[1]]
    expect_false(is_decorated(fn1))
    expect_true(is_decorated(fn2))
})

test_that("decorate_functions() - bound in environment", {
    fn1 <- function() {}
    decorate_functions("fn1", env = environment())
    expect_true(is_decorated(fn1))
})

test_that("decorate_functions() - list of exported functions", {
    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)

    decorate_functions(package = "examplePackage", c("my_add", "public_fn"))

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

test_that("decorate_functions() - all exported functions", {
    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)

    decorate_functions(package = "examplePackage")

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

test_that("decorate_functions() - all functions", {
    load_all("./example-package", TRUE,  export_all = FALSE, quiet = TRUE)

    decorate_functions(package = "examplePackage", include_hidden = TRUE)

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_true(is_decorated(examplePackage:::private_fn))
})

