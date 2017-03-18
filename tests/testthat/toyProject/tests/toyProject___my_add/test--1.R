library(testthat)
library(genthat)

context("toyProject:::my_add")

test_that("toyProject:::my_add", {
	expected <- deserialize("7")
	expect_equal(toyProject:::my_add(a = deserialize("3"), b = deserialize("4")), expected)
})
