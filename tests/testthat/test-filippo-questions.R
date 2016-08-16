#library(testr)
#library(testthat)
#
#context("Filippo dotazy")
#
#test_that('This works if pasted into an R console, but not in a test.', {
#    ran <- FALSE
#    fn1 <- function(x) { x + 3; }
#    fn2 <- function() { 1 + 3; ran <<- TRUE }
#
#    trace(fn1, fn2)
#    fn1(2)
#
#    expect_true(ran)
#})
#
#test_that('This works in both of the cases (usually :D, not even kidding)', {
#    ran <- FALSE
#    fn1 <- function(x) { x + 3 }
#    fn2 <- function() { 1 + 3; ran <<- TRUE }
#
#    trace(fn1, { fn2(); }) # expression instead of function
#    fn1(2)
#
#    expect_true(ran)
#})
#
#test_that('.decorated environment', {
#    genv <- globalenv()
#    genv[['filippo']] <- function() { }
#
#    decorated <- get('.decorated', getNamespace("testr"))
#    print(str(decorated))
#
#    expect_false(exists('filippo', decorated, inherits=TRUE))
#})
