#
#library(testthat)
#
#context("Testcase parser")
#
#test_that("parseGeneratedTest() works", {
#    ret <- parseGeneratedTest('
#        test_that("test42", {
#            expected <- 42L;
#            expect_equal(fn1(40L, 2L), expected)
#        })
#    ')
#    expect_equal("list", "list")
#    expect_equal(typeof(ret), "list")
#    expect_equal(ret$expected_value, 42L)
#    expect_equal(ret$tested_function, "fn1")
#    expect_equal(lapply(ret$arguments, eval), list(40L, 2L))
#})
#
#test_that("loadTestFile() works", {
#    testfile <- tempfile()
#    cat('
#        library("testthat")
#
#        context("myFunnyTests")
#
#        test_that("test42", {
#            expected <- 42L;
#            expect_equal(fn1(40L, 2L), expected)
#        })
#
#        test_that("test11", {
#            expected <- 10L;
#            expect_equal(fn1(9L, 1L), expected)
#        })
#    ', file = testfile)
#    ret <- loadTestFile(testfile)
#
#    expect_equal(typeof(ret), "list")
#    expect_equal(ret$context, "myFunnyTests")
#    expect_equal(ret$libs, c("testthat"))
#
#    expect_equal(length(ret$testCases), 2)
#
#    testcase1 <- ret$testCases[[1]]
#    expect_equal(typeof(testcase1), "list")
#    expect_equal(testcase1$expected_value, 42L)
#    expect_equal(testcase1$tested_function, "fn1")
#    expect_equal(lapply(testcase1$arguments, eval), list(40L, 2L))
#
#    testcase2 <- ret$testCases[[2]]
#    expect_equal(typeof(testcase2), "list")
#    expect_equal(testcase2$expected_value, 10)
#    expect_equal(testcase2$tested_function, "fn1")
#    expect_equal(lapply(testcase2$arguments, eval), list(9, 1))
#})
