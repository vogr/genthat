context("decorate")

if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools needed for this function to work. Please install it.", call. = FALSE)
}

test_that("reset_functions resets all decorated function", {
    f <- function(x) x
    f1 <- function(x) x*2

    g <- function(y) y
    g1 <- function(y) y*2

    .decorations[["x"]] <- list(fqn="x", fun=f, ofun=f1)
    .decorations[["y"]] <- list(fqn="y", fun=g, ofun=g1)

    reset_functions()

    expect_length(.decorations, 0)
    expect_equal(f, f1)
    expect_equal(g, g1)
})

test_that("decorate_function decorates a function", {
    on.exit(reset_functions())

    f <- function(x) 42

    capture <- list()
    expect_equal(decorate_function(f, onentry=function(info) { capture <<- info }), "f")

    expect_length(.decorations, 1)
    with(.decorations[[sexp_address(f)]], {
        expect_equal(fqn, "f")
        expect_equal(fun, f)
        expect_equal(body(ofun), 42)
    })

    f(1)

    with(capture, {
        expect_equal(name, "f")
        expect_equal(decorator, "onentry")
    })
})

test_that("decorate_function decorates only functions", {
    expect_error(decorate_function(1+1), "double: unsupported type")
})

test_that("decorate_function fails to decorate primitive functions", {
    expect_error(decorate_function(sin), "sin: is a primitive function")
})

test_that("decorate_function fails to decorate s3 generic functions", {
    expect_error(decorate_function(print), "print: is a S3 generic function")
})

test_that("get_decorations returns a list of decorated functions", {
    on.exit(reset_functions())

    f <- function(x) 42
    g <- function(y) 84

    decorate_function(f, onentry=identity)
    decorate_function(g, onentry=identity)

    ff <- f
    gg <- g

    with(get_decorations(), {
        expect_equal(body(f), body(ff))
        expect_equal(body(g), body(gg))
    })
})

test_that("is_decorated", {
    on.exit(reset_functions())

    f <- function(x) 42
    g <- function(y) 84

    decorate_function(f, onentry=identity)

    expect_true(is_decorated(f))
    expect_false(is_decorated(g))
})

test_that("reset_function", {
    on.exit(reset_functions())

    f <- function(x) 42
    g <- function(y) 84

    decorate_function(f, onentry=identity)

    expect_equal(reset_function(f), "f")
    expect_warning(reset_function(g), "g: is not decorated")
})

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

test_that("decorate_environment decorates all functions in the environment", {
    on.exit(reset_functions())

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

    res <- decorate_environment(env, type="all", onentry=identity, exclude="j")

    expect_length(res, 4)

    with(res, {
        expect_type(f, "closure")
        expect_type(g, "closure")
        expect_equal(h, "h: is a primitive function")
        expect_equal(i, "i: is a S3 generic function")
    })

    expect_length(env, 5)

    expect_true(is_decorated("f", env))
    expect_true(is_decorated("g", env))

    expect_false(is_decorated("h", env)) # it is a primitive function
    expect_false(is_decorated("i", env)) # it is S3 generic
    expect_false(is_decorated("j", env)) # it excluded
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

test_that("decorator_function works with S4 methods", {
    on.exit(reset_functions())

    # the reason why the where environment is needed is that
    # test_check (which is called from R CMD check) calls test_pkg_env
    # which creates a local copy of the whole package environment
    # with attributes so there will be two namespaces called genthat
    # and that confuses resolve_function

    setGeneric("sides", where=environment(), function(object) {
        standardGeneric("sides")
    })

    setClass("Shape", where=environment())
    setClass("Polygon", where=environment(), representation(sides="integer"), contains="Shape")
    setClass("Triangle", where=environment(), contains="Polygon")

    setMethod("sides", where=environment(), signature(object="Polygon"), function(object) object@sides)
    setMethod("sides", where=environment(), signature("Triangle"), function(object) 3)

    capture <- list()
    decorate_function("sides", onexit=function(info) {
        capture <<- info
    })

    p <- new("Polygon", sides=4L)
    t <- new("Triangle")

    sides(p)
    with(capture, {
        expect_equal(name, "sides")
        expect_equal(args$object, quote(p))
        expect_equal(decorator, "onexit")
    })

    capture <- list()
    sides(t)
    with(capture, {
        expect_equal(name, "sides")
        expect_equal(args$object, quote(t))
        expect_equal(decorator, "onexit")
    })
})

