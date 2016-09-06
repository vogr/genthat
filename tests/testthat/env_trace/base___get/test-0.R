library(testthat)
library(testr)

context("base:::get")

test_that("0", {
    expected <- deserialize("3")
    expect_equal({
        base:::get(envir = deserialize("structure(list(a=3, c=9), `__GENTHAT_TYPE`=\"<ENVIRONMENT>\")"), 
            x = deserialize("\"a\""))
    }, expected)
})
