context("serialize")

serialize <- function(x) {
    eval(parse(text=serialize_value(x)))
}

serialize_code <- function(x) {
    serialize_value(substitute(x))
}

test_that("single element string vector serialization", {
    v <- "A"
    expect_equal(serialize(v), v)
})

test_that("string vector serialization", {
    v <- c("A", "B")
    expect_equal(serialize(v), v)
})

test_that("list serialization", {
    v <- list(a=1, b=2, 3, 4)
    expect_equal(serialize(v), v)
})

test_that("list with quotes serialization", {
    v <- list(a=quote(a), 2)
    expect_equal(serialize(v), v)
})

test_that("pairlist serialization", {
    v <- list(a=quote(a), 2)
    expect_equal(serialize(v), v)
})

test_that("infix function serialization", {
    expect_equal(serialize_code(a + b * c), "a + b * c")
})

test_that("@ function serialization", {
    expect_equal(serialize_code(a@b), "a@b")
})

test_that("$ function serialization", {
    expect_equal(serialize_code(a$b), "a$b")
})

test_that("@<- function serialization", {
    expect_equal(serialize_code(a@b <- "A"), 'a@b <- "A"')
})

test_that("$<- function serialization", {
    expect_equal(serialize_code(a$b <- "A"), 'a$b <- "A"')
})

test_that(": function serialization", {
    expect_equal(serialize_code(a:b), "a:b")
})

test_that(":: function serialization", {
    expect_equal(serialize_code(pkg::fun), "pkg::fun")
})

test_that("::: function serialization", {
    expect_equal(serialize_code(pkg:::fun), "pkg:::fun")
})

test_that("function serialization", {
    expect_equal(serialize_code(function(x, y="A", z) x + y + z), 'function(x, y="A", z) x + y + z')
})

test_that("{ function serialization", {
    expect_equal(serialize_code({ x }), '{\n\tx\n}')
})

test_that("{ with ; function serialization", {
    expect_equal(serialize_code({x; y}), '{\n\tx\n\ty\n}')
})

test_that("[ serialization", {
    expect_equal(serialize_code(x["A"]), 'x["A"]')
})

test_that("[ with multiple arguments serialization", {
    expect_equal(serialize_code(x["A", "B"]), 'x["A", "B"]')
})

test_that("[[ serialization", {
    expect_equal(serialize_code(x[[c("A", "B")]]), 'x[[c("A", "B")]]')
})

test_that("[[ serialization", {
    expect_equal(serialize_code(x[[c("A", "B")]]), 'x[[c("A", "B")]]')
})

test_that("function serialization", {
    v <- serialize_code(my_call(x=a, y="2", z=function(x,y) x + y, zz=list(a="1", b=my, c=quote(my_call))))
    expect_equal(v, 'my_call(x=a, y="2", z=function(x, y) x + y, zz=list(a="1", b=my, c=quote(my_call)))')
})

test_that("function serialization with a quote", {
    v <- serialize_code(my_call("0", "1", a=quote(my_fun), b=my_fun(a="1", b=ref)))
    expect_equal(v, 'my_call("0", "1", a=quote(my_fun), b=my_fun(a="1", b=ref))')
})

test_that("environment serialization", {
    e <- new.env(parent=emptyenv())
    e$a <- 1
    expect_equal(serialize(e), e)
})

test_that("environment with a cycle", {
    e <- new.env(parent=emptyenv())
    e$a <- 1
    e$e <- e
    expect_error(serialize_value(e), "contains cycle")
})

test_that("environment parents", {
    e1 <- new.env(parent=emptyenv())
    e2 <- new.env(parent=e1)

    e1$a <- 1
    e2$a <- 2

    expect_equal(serialize(e2), e2)
})

test_that("environment parents with basenv serialization", {
    e <- new.env(parent=baseenv())
    e$a <- 1

    expect_equal(serialize(e), e)
})

test_that("environment parents with globalenv serialization", {
    e <- new.env(parent=globalenv())
    e$a <- 1

    expect_equal(serialize(e), e)
})

test_that("environment parents with package environment serialization", {
    e <- new.env(parent=as.environment("package:genthat"))
    e$a <- 1

    expect_equal(serialize(e), e)
})

test_that("environment parents with package environment serialization", {
    e <- new.env(parent=getNamespace("genthat"))
    e$a <- 1

    expect_equal(serialize(e), e)
})

test_that("environment with environments serialization", {
    e1 <- new.env(parent=emptyenv())
    e2 <- new.env(parent=e1)
    e3 <- new.env(parent=e2)
    e4 <- new.env(parent=e3)

    e4$e1 <- e1
    e4$e2 <- e2
    e4$e3 <- e3

    expect_equal(serialize(e4), e4)
})

test_that("class serialization", {
    v <- list(1,2)
    class(v) <- "my_class"

    expect_equal(serialize(v), v)
})

test_that("attributes serialization", {
    v <- list(1,2)
    attr(v, "A1") <- 1
    attr(v, "A2") <- 2
    attr(v, "A3") <- list(a=1, b=3)

    expect_equal(serialize(v), v)
})

test_that("( serialization", {
    expect_equal(serialize(2*(1+2)), 6)
})

# TODO: matrix
# TODO: factors
# TODO: named elements

#test_that('Can serialize vectors of reals.', {
#    l1 <- c(12.3, 435.549)
#    s1 <- serialize_r(l1);
#    expect_true(is.character(s1))
#    l2 <- deserialize(s1);
#    expect_true(identical(l1, l2));
#})
#
#test_that('Can serialize vectors of complex.', {
#    l1 <- c(12.3+3.39912i, complex(real=435.549, imaginary=Inf))
#    s1 <- serialize_r(l1);
#    expect_true(is.character(s1))
#    l2 <- deserialize(s1);
#    expect_true(identical(l1, l2));
#})
#
#test_that('Can serialize lists.', {
#    l1 <- list(a = 3, 4, c = 9)
#    s1 <- serialize_r(l1);
#    expect_true(is.character(s1))
#    l2 <- deserialize(s1);
#    expect_true(identical(l1, l2));
#})
#
#test_that('Can serialize matrices', {
#    m1 <- matrix(1:4, nrow=2, ncol=2)
#    s1 <- serialize_r(m1);
#    expect_true(is.character(s1))
#    m2 <- deserialize(s1);
#    expect_true(identical(m1, m2));
#})
#
#test_that('Can serialize environments.', {
#    e1 <- as.environment(list(a = 3, c = 9))
#    s1 <- serialize_r(e1);
#    expect_true(is.character(s1))
#    e2 <- deserialize(s1);
#    expect_true(isTRUE(all.equal(e1, e2)));
#})
#
#test_that('Big outputs should not be split into multiple strings.', {
#    x <- list(a = Seatbelts)
#    s <- serialize_r(x)
#    expect_type(s, "character")
#    expect_length(s, 1)
#})
#
#test_that('(deserialize . serialize) on big frame', {
#    x1 <- Seatbelts
#    s <- serialize_r(x1)
#    x2 <- deserialize(s)
#    expect_equal(x1, x2)
#})
#
#test_that('Cannot serialize looped structures.', {
#    e1 <- as.environment(list(a = 3))
#    e2 <- as.environment(list(b = 4))
#    e1$child <- e2
#    e2$child <- e1
#    error_thrown <- FALSE
#    tryCatch(serialize_r(e1), error = function(e) { error_thrown <<- TRUE });
#    expect_true(error_thrown);
#})
#
