library(genthat)
library(testthat)

context("Helpers")

test_that('parseFunctionNames works correctly', {
    expect_equal(genthat:::parseFunctionNames(start_capture)[[1]][["name"]], "start_capture")
    expect_true(is.na(genthat:::parseFunctionNames(start_capture)[[1]][["package"]]))
    expect_equal(genthat:::parseFunctionNames("genthat:::filter_tests")[[1]][["name"]], "filter_tests")
    expect_equal(genthat:::parseFunctionNames("genthat:::filter_tests")[[1]][["package"]], "genthat")
    expect_equal(genthat:::parseFunctionNames("genthat::decorate")[[1]][["name"]], "decorate")
    expect_equal(genthat:::parseFunctionNames("genthat::decorate")[[1]][["package"]], "genthat")
    expect_equal(genthat:::parseFunctionNames(genthat:::decorate)[[1]][["name"]], "decorate")
    expect_equal(genthat:::parseFunctionNames(genthat:::decorate)[[1]][["package"]], "genthat")
    expect_equal(genthat:::parseFunctionNames(genthat::decorate)[[1]][["name"]], "decorate")
    expect_equal(genthat:::parseFunctionNames(genthat::decorate)[[1]][["package"]], "genthat")
    expect_equal(genthat:::parseFunctionNames(decorate, generate)[[2]][["name"]], "generate")
})
