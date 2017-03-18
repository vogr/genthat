library(testthat)
library(genthat)

context("toyProject:::public_fn")

test_that("toyProject:::public_fn", {
	expected <- deserialize("\"this is public: 42\"")
	expect_equal(toyProject:::public_fn(x = deserialize("\"42\"")), expected)
})
