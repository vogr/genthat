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

test_that("LANGSXP serialization works with anonymous functions", {
    v <- serialize_code((function(x)x+1)(1))
    expect_equal(v, "(function(x) x + 1)(1)")
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

test_that("closure serialization", {
    f <- function() 1+1
    expect_equal(serialize(f)(), 2)
})

test_that("closure reference serialization", {
    expect_equal(serialize(ls)(), ls())
})

test_that("closure with cycles in environments", {
    a <- 1
    b <- 2
    c <- 3

    # this one needs to be linked
    g <- function(x) x + b
    expect_equal(serialize(function(x) g(x) + b)(1), 5)

    # this one does not need to be linked
    g <- function(x) x + x
    expect_equal(serialize(function(x) g(x) + b)(1), 4)

    # this one contains c, but does not need to be linked
    g <- function(x) x + c
    expect_equal(serialize(function(x) g(x) + b)(1), 6)

    # this one contains c and needs to be linked
    g <- function(x) x + c + b
    expect_equal(serialize(function(x) g(x) + b)(1), 8)
})

test_that("serialize properly escapes non-syntactic names", {
    expect_equal(serialize_value(quote(`_a`())), "`_a`()")
    expect_equal(serialize_value(quote(`_a`(`_1`=1))), "`_a`(`_1`=1)")
    expect_equal(serialize_value(quote(`_a`)), "`_a`")
})

test_that("list serialization", {
    l <- list(1, 2, 3)
    expect_equal(serialize(l), l)

    l <- list(`1`=1, `2`=2, `3`=3)
    expect_equal(serialize(l), l)
})

test_that("data frame serialization", {
    df <- data.frame(x=c(1, 2, 3), y=c(4, 5, 6))
    expect_equal(serialize(df), df)
})

test_that("can serialize unary functions", {
    expect_equal(serialize(-1), -1)
    expect_equal(serialize(+1), +1)
    expect_equal(serialize_value(quote(~x)), "~x")
    expect_equal(serialize_value(quote(!x)), "!x")
})

test_that("RAWSXP serialization", {
    expect_equal(serialize(as.raw(1)), as.raw(1))
})

test_that("EXPRSXP serialization", {
    expect_equal(serialize(expression(1+1)), expression(1+1))
})

test_that("BUILTINSXP serialization", {
    expect_equal(serialize_value(quote(`+`)), "`+`")
})

test_that("SPECIALSXP serialization", {
    expect_equal(serialize_value(quote(on.exit)), "on.exit")
})

test_that("external function serialization", {
    expect_true(is.closure(serialize(testthat::test_file)))
})

test_that("EXTPTRSXP serializatin works", {
     Rcpp::cppFunction("Rcpp::XPtr<int> ext_ptr() { int *x = new int[1]; return Rcpp::XPtr<int>(x); }")

     p1 <- ext_ptr()
     p2 <- ext_ptr()

     # some basic assumptions
     expect_equal(p1, p1)
     expect_true(identical(p1, p1))
     expect_false(identical(p1, p2))

     s <- serialize_value(p1)

     expect_equivalent(s, ".ext.1")
     expect_equal(attr(s, "externals")$.ext.1, p1)

     s <- serialize_value(list(p1, p1))
     expect_equivalent(s, "list(.ext.1, .ext.1)")
     expect_equal(attr(s, "externals")$.ext.1, p1)

     s <- serialize_value(list(p1, p2))
     expect_equivalent(s, "list(.ext.1, .ext.2)")
     expect_equal(attr(s, "externals")$.ext.1, p1)
     expect_equal(attr(s, "externals")$.ext.2, p2)

     # try with attributes
     x <- list(1, 2, p1)
     attr(x, "a1") <- p2
     attr(x, "a2") <- list(p1, p2)
     s <- serialize_value(x)
     expect_equivalent(s, "structure(list(1, 2, .ext.1), a1=.ext.2, a2=list(.ext.1, .ext.2))")
     expect_equal(attr(s, "externals")$.ext.1, p1)
     expect_equal(attr(s, "externals")$.ext.2, p2)

     # try with attributes on primitive vectors
     x <- c(1, 2, 3)
     attr(x, "a1") <- p1
     attr(x, "a2") <- list(p1, p2)
     s <- serialize_value(x)
     expect_equivalent(s, "structure(c(1, 2, 3), a1=.ext.1, a2=list(.ext.1, .ext.2))")
     expect_equal(attr(s, "externals")$.ext.1, p1)
     expect_equal(attr(s, "externals")$.ext.2, p2)
})

test_that("S4SXP serialization", {
    setClass("Person", representation(name="character", age="numeric"), prototype(name=NA_character_, age=0))
    p1 <- new("Person", name="Joe", age=31)

    s <- serialize_value(p1)
    expect_equivalent(s, ".ext.1")
    expect_equal(attr(s, "externals")$.ext.1, p1)
})

##     p2 <- new("Person", name="Joe")

##     sides <- function(object) 0
##     setGeneric("sides")

##     setGeneric("sides", function(object) {
##         standardGeneric("sides")
##     })

##     setClass("Shape")
##     setClass("Polygon", representation(sides = "integer"), contains = "Shape")
##     setClass("Triangle", contains = "Polygon")
##     setClass("Square", contains = "Polygon")
##     setClass("Circle", contains = "Shape")

##     setMethod("sides", signature(object = "Polygon"), function(object) {
##         object@sides
##     })

##     setMethod("sides", signature("Triangle"), function(object) 3)
##     setMethod("sides", signature("Square"),   function(object) 4)
##     setMethod("sides", signature("Circle"),   function(object) Inf)

##     setGeneric("sides", valueClass = "numeric", function(object) {
##         standardGeneric("sides")
##     })

## })


## test_that("", {
##     f <- function(a, b, c) a * b * c
##     attr(f, "genthat_extracted_closure") <- TRUE

##     str(serialize_value(f))
## })

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
