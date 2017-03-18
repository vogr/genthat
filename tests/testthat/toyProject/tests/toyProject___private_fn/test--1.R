library(testthat)
library(genthat)

context("toyProject:::private_fn")

test_that("toyProject:::private_fn", {
	expected <- deserialize("\"this is private: 42\"")
	expect_equal(toyProject:::private_fn(x = deserialize("\"42\"")), expected)
})
