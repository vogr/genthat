library(testthat)
library(datasets)

context("Serialize R EXP")

deserialize_exp <- function(x) eval(parse(text=paste0("quote(", x, ")")))


test_that('Can serialize symbols', {
    exp <- quote(a)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "a")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);
})

test_that('Can serialize literals', {
    exp <- quote(1L)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "1L")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);

    exp <- quote(1)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "readBin(as.raw(c(0,0,0,0,0,0,0xf0,0x3f)), n=1, \"double\")")

    exp <- quote(TRUE)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "TRUE")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);

    exp <- quote(NULL)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "NULL")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);

    exp <- quote("hello")
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "\"hello\"")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);
})


test_that('Can serialize simple language', {
    exp <- quote(a + b)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "a+b")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);
})

test_that('Can serialize nested languages', {
    exp <- quote(a + b + c + d)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "a+b+c+d")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);

    exp <- quote(a ~ b + c | d)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "a~b+c|d")
    deserialized <- deserialize_exp(s1);
    expect_identical(exp, deserialized);
})

test_that('Can serialize functions', {
    exp <- quote(function(x) 1)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "function(x) readBin(as.raw(c(0,0,0,0,0,0,0xf0,0x3f)), n=1, \"double\")")

    exp <- quote(function(x = 5L) x)
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "function(x = 5L) x")

    exp <- quote(function(x) {1L})
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "function(x) {1L}")

    exp <- quote(function(x) 
    	{
    		a + b;
    		c <- function (y) x
    		c() ^2L
    	})
    s1 <- serialize_r_expr(exp)
    expect_identical(s1, "function(x) {a+b;\nc<-function(y) x;\nc()^2L}")
})
