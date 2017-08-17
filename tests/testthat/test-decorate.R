context("decorate")

if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools needed for this function to work. Please install it.", call. = FALSE)
}

test_that("get_function_package_name returns the package name", {
    expect_equal(get_function_package_name(ls), "base")
    expect_equal(get_function_package_name(`%in%`), "base")
    expect_equal(get_function_package_name(tools::Rcmd), "tools")

    my_f <- function() {}

    expect_equal(get_function_package_name(my_f), NULL)
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

test_that("is_decorated knows when a functions is decorated", {
    decorator <- create_decorator()

    f <- function() {}
    expect_false(is_decorated(f))

    decorate_function(decorator, f, "f", record_fun=function(...) {})
    expect_true(is_decorated(f))
})

test_that("decorate_environment decorates all functions in the environment", {
    decorator <- create_decorator()

    env <- new.env(parent=emptyenv())

    env$f <- function(x) x
    environment(env$f) <- env

    env$g <- function(x) x+1
    environment(env$g) <- env

    env$h <- sin

    expect_equal(length(env), 3)

    decorate_environment(env, decorator=decorator)

    expect_equal(length(env), 3)

    expect_true(is_decorated(env$f))
    expect_true(is_decorated(env$g))

    expect_false(is_decorated(env$h)) # it is a primitive function

    # TODO: check decorations
})

test_that("reassign_function replaces function body and add attributes", {
    f <- function(a,b) {a+b}
    g <- function(a,b) {a-b}
    attr(g, "a") <- TRUE

    reassign_function(f, g)

    expect_equal(f(1, 2), -1)
    expect_equal(attr(f, "a"), TRUE)
})

test_that("create_duplicate duplicates a function", {
    f <- function(a, b) {a + b}

    expect_identical(f, create_duplicate(f))
    expect_error(create_duplicate(NULL))
})

test_that("resolve_decorating_fun_args works with all cases", {
    f <- function() 1
    g <- function() 2

    xs <- resolve_decorating_fun_args("f")
    expect_equal(
        xs,
        list(f=list(name="f", fun=f))
    )

    xs <- resolve_decorating_fun_args("f", "g")
    expect_equal(
        xs,
        list(f=list(name="f", fun=f), g=list(name="g", fun=g))
    )

    xs <- resolve_decorating_fun_args("f", g)
    expect_equal(
        xs,
        list(f=list(name="f", fun=f), g=list(name="g", fun=g))
    )

    xs <- resolve_decorating_fun_args(f, g)
    expect_equal(
        xs,
        list(f=list(name="f", fun=f), g=list(name="g", fun=g))
    )

    xs <- resolve_decorating_fun_args(c("f", "g"))
    expect_equal(
        xs,
        list(f=list(name="f", fun=f), g=list(name="g", fun=g))
    )

    xs <- resolve_decorating_fun_args(list(f=f, g=g))
    expect_equal(
        xs,
        list(f=list(name="f", fun=f), g=list(name="g", fun=g))
    )
})

test_that("decorate_functions returns decorated function", {
    decorator <- create_decorator()

    f <- function(x) x

    decoration <- decorate_functions(f, decorator=decorator)

    expect_equal(length(decoration), 1)

    expect_true(is.function(decoration$f))
    expect_true(is.function(f))
    expect_equal(decoration$f, f)
    expect_equal(attr(f, "__genthat_original_fun"), function(x) x)
})

test_that("reset_function", {
    decorator <- create_decorator()

    f <- function(x) x

    decorate_functions(f, decorator=decorator)
    reset <- reset_functions(f, decorator=decorator)

    expect_equal(length(reset), 1)

    expect_true(is.function(reset$f))
    expect_true(is.function(f))
    expect_equal(reset$f, f)
    expect_equal(attr(f, "__genthat_original_fun"), NULL)
})

test_that("decorate_function checks for primitive functions", {
    decorator <- create_decorator()

    expect_error(decorate_function(decorator, `$`, "$"), regexp="\\$: is a primitive function")
})

test_that("decorator can be supplied method", {
    f <- function(x) x

    expect_equal(create_decorator("trycatch")$method, genthat:::decorate_with_trycatch)
    expect_equal(create_decorator("onexit")$method, genthat:::decorate_with_onexit)
    expect_equal(create_decorator(f)$method, f)
})

## # TODO: test that we cannot decorate builtins

## # TODO: test imported namespaces

## # TODO: test decorations
