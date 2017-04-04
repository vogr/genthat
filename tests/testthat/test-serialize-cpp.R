library(testthat)
library(datasets)

context("Serialize")

deserialize <- function(x) eval(parse(text=x))

test_that('Can serialize vectors of reals.', {
    l1 <- c(12.3, 435.549)
    s1 <- serialize_r(l1);
    expect_true(is.character(s1))
    l2 <- deserialize(s1);
    expect_true(identical(l1, l2));
})

test_that('Can serialize vectors of complex.', {
    l1 <- c(12.3+3.39912i, complex(real=435.549, imaginary=Inf))
    s1 <- serialize_r(l1);
    expect_true(is.character(s1))
    l2 <- deserialize(s1);
    expect_true(identical(l1, l2));
})

test_that('Can serialize lists.', {
    l1 <- list(a = 3, 4, c = 9)
    s1 <- serialize_r(l1);
    expect_true(is.character(s1))
    l2 <- deserialize(s1);
    expect_true(identical(l1, l2));
})

test_that('Can serialize matrices', {
    m1 <- matrix(1:4, nrow=2, ncol=2)
    s1 <- serialize_r(m1);
    expect_true(is.character(s1))
    m2 <- deserialize(s1);
    expect_true(identical(m1, m2));
})

test_that('Can serialize environments.', {
    e1 <- as.environment(list(a = 3, c = 9))
    s1 <- serialize_r(e1);
    expect_true(is.character(s1))
    e2 <- deserialize(s1);
    expect_true(isTRUE(all.equal(e1, e2)));
})

test_that('Big outputs should not be split into multiple strings.', {
    x <- list(a = Seatbelts)
    s <- serialize_r(x)
    expect_type(s, "character")
    expect_length(s, 1)
})

test_that('(deserialize . serialize) on big frame', {
    x1 <- Seatbelts
    s <- serialize_r(x1)
    x2 <- deserialize(s)
    expect_equal(x1, x2)
})

test_that('Cannot serialize looped structures.', {
    e1 <- as.environment(list(a = 3))
    e2 <- as.environment(list(b = 4))
    e1$child <- e2
    e2$child <- e1
    error_thrown <- FALSE
    tryCatch(serialize_r(e1), error = function(e) { error_thrown <<- TRUE });
    expect_true(error_thrown);
})

test_that('Can serialize symbols', {
    symbol1 <- quote(x)
    s <- serialize_r(symbol1);
    expect_true(is.character(s))
    expect_equal(s, "quote(x)")
    symbol2 <- deserialize(s);
    expect_true(identical(symbol1, symbol2));
})

test_that('Can serialize calls', {
    call1 <- quote(f(x,y))
    s <- serialize_r(call1);
    expect_true(is.character(s))
    expect_equal(s, "quote(f(x,y))")
    call2 <- deserialize(s);
    expect_true(identical(call1, call2));
})

test_that('Can serialize infix calls - symbols', {
    call1 <- quote(x + y)
    s <- serialize_r(call1);
    expect_true(is.character(s))
    expect_equal(s, "quote(x+y)")
    call2 <- deserialize(s);
    expect_true(identical(call1, call2));
})

test_that('Can serialize infix calls - ints', {
    call1 <- quote(x + 5L)
    s <- serialize_r(call1);
    expect_true(is.character(s))
    expect_equal(s, "quote(x+5L)")
    call2 <- deserialize(s);
    expect_true(identical(call1, call2));
})

test_that('Can serialize infix calls - doubles', {
    x <- 4
    call1 <- quote(x + 5)
    s <- serialize_r(call1);
    expect_true(is.character(s))
    expect_equal(s, "quote(x+readBin(as.raw(c(0,0,0,0,0,0,0x14,0x40)), n=1, \"double\"))")
    call2 <- deserialize(s);
    # expect_true(identical(call1, call2)); -- this one is not possible to have
    expect_true(identical(eval(call1), eval(call2)));
})
