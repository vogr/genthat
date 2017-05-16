context("samplepkg tests")

test_that("exported function", {
    expect_equal(my_public("42"), "public: 42")
})

test_that("non-exported function", {
    expect_equal(my_private("42"), "private: 42")
})

test_that("indirect call", {
    expect_equal(my_call(my_add, 1, 2), 3)
})

test_that("indirect call", {
    my_mul <- function(a, b, c) a * b * c

    a1 <- 1
    b1 <- 2
    c1 <- 3

    expect_equal(my_call(my_mul, a1, b1, c1), 6)
})

test_that("indirect call with two closures", {
    x <- 10
    d <- function() x
    my_mul <- function(a, b, c) a * b * c * x * d()

    a1 <- 1
    b1 <- 2
    c1 <- 3

    expect_equal(my_call(my_mul, a1, b1, c1), 600)
})

test_that("warnings", {
    expect_warning(my_warning())
})

test_that("errors", {
    expect_error(my_error())
})
