context("decoration")

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

test_that("decorated function calls recorded with retv on success", {
    capture <- list()

    f <- function(a,b,c) { if (a) b + c else stop("an error") }

    d <- do_decorate_function(
        "f",
        NULL,
        f,
        .recorder=function(...) capture <<- list(...)
    )

    expect_equal(formals(d), formals(d))
    expect_equal(environment(d), environment(f))
    expect_equal(attributes(d), list(`__genthat_original_fun`=f))

    # TEST retv
    d(TRUE, 1L, 2L)

    expect_equal(length(capture), 5L)
    expect_equal(capture$name, "f")
    expect_equal(capture$pkg, NULL)
    expect_equal(capture$args, list(a=TRUE, b=1L, c=2L))
    expect_false(is.null(capture$env))
    expect_equal(capture$retv, 3L)

    # TEST error
    expect_error(d(FALSE, 1L, 2L))

    expect_equal(length(capture), 5L)
    expect_equal(capture$name, "f")
    expect_equal(capture$pkg, NULL)
    expect_equal(capture$args, list(a=FALSE, b=1L, c=2L))
    expect_false(is.null(capture$env))
    expect_equal(capture$error$message, "an error")
    expect_false(is.null(capture$error$call))
})

test_that("decorated functions can be multiline function", {
    capture <- list()

    f <- function(x, y) {
        x1 <- x + 1
        y1 <- y + 1
        x1 + y1
    }

    d <- do_decorate_function(
            "f",
            NULL,
            f,
            .recorder=function(...) capture <<- list(...)
    )

    d(1, 2)

    expect_equal(capture$retv, 5)
})

test_that("decorated function supports ...", {
    capture <- list()

    f <- function(...) sum(...)
    d <- do_decorate_function(
            "f",
            NULL,
            f,
            .recorder=function(...) capture <<- list(...)
    )

    expect_equal(d(a=1L, b=2L, 3L, 4L), 10L)

    expect_equal(length(capture), 5L)
    expect_equal(capture$name, "f")
    expect_equal(capture$args, list(a=1L, b=2L, 3L, 4L))
    expect_equal(capture$retv, 10L)
})

test_that("do_decorate_function decorates a package function", {
    capture <- list()

    d <- do_decorate_function(
            "file_path_sans_ext",
            "tools",
            tools::file_path_sans_ext,
            .recorder=function(...) capture <<- list(...)
    )

    expect_equal(formals(d), formals(tools::file_path_sans_ext))
    expect_equal(environment(d), environment(tools::file_path_sans_ext))

    d("a.b")

    expect_equal(capture$name, "file_path_sans_ext")
    expect_equal(capture$pkg, "tools")
    expect_equal(capture$args, list(x="a.b"))
    expect_equal(capture$retv, "a")
})

test_that("is_decorated knows when a functions is decorated", {
    f <- function() {}
    expect_false(is_decorated(f))

    d <- do_decorate_function("f", NULL, f, .recorder=function(...) {})
    expect_true(is_decorated(d))
})

test_that("decorate_environment decorates all functions in the environment", {
    env <- new.env(parent=emptyenv())

    env$f <- function(x) x
    environment(env$f) <- env

    env$g <- function(x) x+1
    environment(env$g) <- env

    env$h <- sin

    expect_equal(length(env), 3)

    decorate_environment(env)

    expect_equal(length(env), 3)

    expect_true(is_decorated(env$f))
    expect_true(is_decorated(env$g))

    expect_false(is_decorated(env$h)) # it is a primitive function
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
    f <- function(x) x

    decoration <- decorate_functions(f)

    expect_equal(length(decoration), 1)

    expect_true(is.function(decoration$f))
    expect_true(is.function(f))
    expect_equal(decoration$f, f)
    expect_equal(attr(f, "__genthat_original_fun"), function(x) x)
})

test_that("reset_function", {
    f <- function(x) x

    decorate_functions(f)
    reset <- reset_functions(f)

    expect_equal(length(reset), 1)

    expect_true(is.function(reset$f))
    expect_true(is.function(f))
    expect_equal(reset$f, f)
    expect_equal(reset$f, function(x) x)
    expect_equal(attr(f, "__genthat_original_fun"), NULL)
})

test_that("decorate_function checks for primitive functions", {
    expect_error(decorate_function(`$`, "$"), regexp="\\$: is a primitive function")
})

## # TODO: test that we cannot decorate builtins

## # TODO: test imported namespaces
