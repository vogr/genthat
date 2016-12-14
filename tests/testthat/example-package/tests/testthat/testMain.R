library(testthat)
library(toyProject)

context("Capture")

test_that("Can test public functions.", {
    expected <- public_fn("42")
    expect_equal("this is public: 42", expected)
})

test_that("Can test private functions.", {
    expected <- private_fn("42")
    expect_equal("this is private: 42", expected)
})

test_that("Functions are not processed", {
    y <- my_apply(function(x) x + 1, 5)
    expect_equal(y, 6)
})

test_that("Addition", {
    add3 <- applyFirst(my_add, 3)
    y <- add3(4)
    #expect_equal(y, 7)
})

test_that("causes_warning() can run", {
    causes_warning()
})

test_that("Attributes on arguments", {
    a <- structure(1:4, .Dim = 4, .Dimnames = list(c("a1", "a2", "a3", "a4")))
    b <- structure(5:8, .Dim = 4, .Dimnames = list(c("b1", "b2", "b3", "b4")))
    my_add(a, b)
})

test_that("Variadic", {
    variadic(5, myVarArgName = 10, 11, myVarArgName2 = 12)
})

test_that("DefaultArgs", {
    fnWithDefaults(5)
})

test_that("ArgsOrder", {
    argsOrder(4,5,6, a = 1, b = 2, c = 3)
})

