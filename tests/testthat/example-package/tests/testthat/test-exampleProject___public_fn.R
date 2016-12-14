library(testthat)

context("exampleProject:::public_fn")

test_that("exampleProject:::public_fn", {
	expected <- "this is public: 32"
	expect_equal(exampleProject:::public_fn(x = readBin(as.raw(c(0,0,0,0,0,0,0x40,0x40)), n=1, "double")), expected)
})

test_that("exampleProject:::public_fn", {
	expected <- "this is public: 32"
	expect_equal(exampleProject:::public_fn(x = readBin(as.raw(c(0,0,0,0,0,0,0x40,0x40)), n=1, "double")), expected)
})

test_that("exampleProject:::public_fn", {
	expected <- "this is public: 32"
	expect_equal(exampleProject:::public_fn(x = readBin(as.raw(c(0,0,0,0,0,0,0x40,0x40)), n=1, "double")), expected)
})
