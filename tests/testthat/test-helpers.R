library(testr)
library(testthat)

context("Helpers")

test_that('parseFunctionNames works correctly', {
    expect_equal(testr:::parseFunctionNames(start_capture)[[1]][["name"]], "start_capture")
    expect_true(is.na(testr:::parseFunctionNames(start_capture)[[1]][["package"]]))
    expect_equal(testr:::parseFunctionNames("testr:::filter_tests")[[1]][["name"]], "filter_tests")
    expect_equal(testr:::parseFunctionNames("testr:::filter_tests")[[1]][["package"]], "testr")
    expect_equal(testr:::parseFunctionNames("testr::decorate")[[1]][["name"]], "decorate")
    expect_equal(testr:::parseFunctionNames("testr::decorate")[[1]][["package"]], "testr")
    expect_equal(testr:::parseFunctionNames(testr:::decorate)[[1]][["name"]], "decorate")
    expect_equal(testr:::parseFunctionNames(testr:::decorate)[[1]][["package"]], "testr")
    expect_equal(testr:::parseFunctionNames(testr::decorate)[[1]][["name"]], "decorate")
    expect_equal(testr:::parseFunctionNames(testr::decorate)[[1]][["package"]], "testr")
    expect_equal(testr:::parseFunctionNames(decorate, generate)[[2]][["name"]], "generate")
})
