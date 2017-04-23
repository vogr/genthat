#
#library(testthat)
#library(datasets)
#
#context("testIsSyntacticName")
#
#test_that('testIsSyntacticName() reserved words', {
#    expect_false(testIsSyntacticName("if"));
#    expect_false(testIsSyntacticName("NaN"));
#})
#
#test_that('testIsSyntacticName() valid names', {
#    expect_true(testIsSyntacticName("my_ident"));
#    expect_true(testIsSyntacticName(".my_ident"));
#    expect_true(testIsSyntacticName("."));
#    expect_true(testIsSyntacticName("a"));
#})
#
#test_that('testIsSyntacticName() invalid names', {
#    expect_false(testIsSyntacticName("8abc"));
#    expect_false(testIsSyntacticName(".8abc"));
#    expect_false(testIsSyntacticName("_my_ident"));
#    expect_false(testIsSyntacticName("__GENTHAT_TYPE"));
#})
#
