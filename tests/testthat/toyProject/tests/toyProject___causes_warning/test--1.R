library(testthat)
library(genthat)

context("toyProject:::causes_warning")

test_that("toyProject:::causes_warning", {
	expected <- deserialize("character(0)")
	expect_equal(toyProject:::causes_warning(), expected)
	expect_warning(toyProject:::causes_warning(), "this is a warning")
})
