
library(testthat)
library(devtools)
load_all("../..", export_all = FALSE, quiet = TRUE) # genthat

source("./utils.R")

context("Function decoration API")

test_that("decorate_function_val()", {
    fn1 <- function() {}
    fn2 <- decorate_function_val(fn1, "fn1_label")
    expect_false(is_decorated(fn1))
    expect_true(is_decorated(fn2))
})

test_that("decorate_function()", {
    fn1 <- function() {}
    decorate_function("fn1", env = environment())
    expect_true(is_decorated(fn1))
})

test_that("decorate_exported()", {
    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)

    decorate_exported("examplePackage", c("my_add", "public_fn"))

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

test_that("decorate_exported()", {
    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)

    decorate_exported("examplePackage", all = TRUE)

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

test_that("decorate_hidden_functions()", {
    load_all("./example-package", TRUE,  export_all = FALSE, quiet = TRUE)

    decorate_hidden_functions("examplePackage")

    expect_false(is_decorated(examplePackage::my_add))
    expect_false(is_decorated(examplePackage::public_fn))
    expect_true(is_decorated(examplePackage:::private_fn))
})

test_that("undecorate_all()", {
    load_all("./example-package", TRUE,  export_all = FALSE, quiet = TRUE)
    fn1 <- function() {}

    decorate_function("fn1", env = environment())
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

