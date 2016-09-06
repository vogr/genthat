library(testthat)
library(testr)

context("base:::get")

test_that("base:::get", {
    expect_error({
        base:::get(envir = deserialize("structure(list(a=3, c=9), `__GENTHAT_TYPE`=\"<ENVIRONMENT>\")"), 
            x = deserialize("\"a\""))
    }, "<text>:3:1: unexpected '<'\n2: .Internal(get(x, envir, mode, inherits))\n3: <\n   ^")
})
