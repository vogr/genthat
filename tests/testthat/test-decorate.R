context("decorate")

if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools needed for this function to work. Please install it.", call. = FALSE)
}

test_that("create_function creates functions", {
    f <- create_function(pairlist(a=1, b=2), substitute(a+b))
    expect_equal(f(), 3)
})

test_that("create_function assigns the right environment", {
    e <- new.env()
    e$var <- 1

    f <- create_function(pairlist(a=1, b=2), substitute(a+b+var), e)
    expect_equal(f(), 4)
    expect_equal(environment(f), e)
})

test_that("create_function assigns attributes", {
    attrs <- list(x=TRUE, y=TRUE)
    f <- create_function(pairlist(a=1, b=2), substitute(a+b), attributes=attrs)

    expect_equal(f(1, 2), 3)
    expect_equal(attributes(f), attrs)
})

test_that("is_decorated knows when a functions is decorated", {
    d1 <- create_decorator()
    d2 <- create_decorator()

    f <- function() {}
    expect_false(is_decorated(f, decorator=d1))

    decorate_function(f, decorator=d1)
    expect_true(is_decorated(f, decorator=d1))
    expect_false(is_decorated(f, decorator=d2))
})

test_that("decorate functions redecorates already decorated function", {
    d1 <- create_decorator()

    f <- function() {}
    expect_false(is_decorated(f, decorator=d1))

    decorate_function(f, decorator=d1)
    b1 <- body(f)
    decorate_function(f, decorator=d1)
    b2 <- body(f)

    expect_equal(b1, b2)
})

test_that("decorate_environment decorates all functions in the environment", {
    d <- create_decorator()

    env <- new.env(parent=emptyenv())

    env$f <- function(x) x
    environment(env$f) <- env

    env$g <- function(x) x+1
    environment(env$g) <- env

    env$h <- sin

    env$i <- function(x) UseMethod("i")
    environment(env$i) <- env

    env$j <- function(x) x*2

    expect_equal(length(env), 5)

    decorate_environment(env, decorator=d, exclude="j")

    expect_equal(length(env), 5)

    expect_true(is_decorated(name="f", decorator=d, env=env))
    expect_true(is_decorated(name="g", decorator=d, env=env))

    expect_false(is_decorated(name="h", decorator=d, env=env)) # it is a primitive function
    expect_false(is_decorated(name="i", decorator=d, env=env)) # it is S3 generic
    expect_false(is_decorated(name="j", decorator=d, env=env)) # it excluded

    # TODO: check decorations
})

test_that("reassign_function only replaces function body", {
     f <- function(a, b) {a + b}
     attr(f, "a") <- TRUE

     g <- function(a, b, c) {a - b}
     attr(g, "b") <- TRUE

     reassign_function(f, g)

     expect_equal(f(1, 2), -1)
     expect_equal(attr(f, "a"), TRUE)
     expect_equal(attr(f, "b"), NULL)
     expect_equal(length(formals(f)), 2)
})

test_that("create_duplicate duplicates a function", {
     f <- function(a, b) {a + b}

     expect_identical(f, create_duplicate(f))
     expect_error(create_duplicate(NULL))
})

test_that("decorate_function returns decorated function", {
    d <- create_decorator()

    f <- function(x) x

    decorate_function(f, decorator=d)

    expect_equal(length(d$decorations), 1)

    expect_true(is.function(f))
    expect_true(is_decorated(f, decorator=d))
    expect_true(is.list(d$decorations$f))
})

test_that("reset_function", {
    d <- create_decorator()

    f <- function(x) x

    decorate_function(f, decorator=d)
    expect_true(is_decorated(f, decorator=d))
    expect_equal(length(d$decorations), 1)

    reset_function(f, decorator=d)
    expect_false(is_decorated(f, decorator=d))
    expect_equal(length(d$decorations), 0)
})

test_that("decorator does not work with S3 generics", {
    f <- function(x) UseMethod("n")
    g <- function(x) {
        1+1
        UseMethod("n")
    }

    d <- create_decorator()

    expect_error(decorate_function(f, decorator=d), regexp="f: is a S3 generic function")
    expect_error(decorate_function(g, decorator=d), regexp="g: is a S3 generic function")
})

test_that("decorator works with S4 methods", {
    setGeneric("sides", function(object) {
        standardGeneric("sides")
    })

    setClass("Shape")
    setClass("Polygon", representation(sides = "integer"), contains = "Shape")
    setClass("Triangle", contains = "Polygon")

    setMethod("sides", signature(object = "Polygon"), function(object) object@sides)
    setMethod("sides", signature("Triangle"), function(object) 3)

    tracer <- create_sequence_tracer()
    set_tracer(tracer)
    on.exit(reset_traces())

    d <- create_decorator()
    decorate_function(name="sides", decorator=d)

    p <- new("Polygon", sides=4L)
    t <- new("Triangle")

    sides(p)
    sides(t)

    traces <- copy_traces()
    expect_equal(length(traces), 2)
    expect_equal(traces[[1]]$globals$p, p)
    expect_equal(traces[[1]]$retv, 4)
    expect_equal(traces[[2]]$globals$t, t)
    expect_equal(traces[[2]]$retv, 3)
})

1
## ## # TODO: test that we cannot decorate builtins

## ## # TODO: test imported namespaces

## ## # TODO: test decorations
