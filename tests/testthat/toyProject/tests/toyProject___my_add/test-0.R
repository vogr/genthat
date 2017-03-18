library(testthat)
library(genthat)

context("toyProject:::my_add")

test_that("toyProject:::my_add", {
	expected <- deserialize("structure(c(6L, 8L, 10L, 12L), dim = 4L, dimnames = list(c(\"a1\", \"a2\", \"a3\", \"a4\")))")
	expect_equal(toyProject:::my_add(a = deserialize("structure(c(1L, 2L, 3L, 4L), dim = 4L, dimnames = list(c(\"a1\", \"a2\", \"a3\", \"a4\")))"), b = deserialize("structure(c(5L, 6L, 7L, 8L), dim = 4L, dimnames = list(c(\"b1\", \"b2\", \"b3\", \"b4\")))")), expected)
})
