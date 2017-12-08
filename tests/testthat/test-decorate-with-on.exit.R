context("decorate-with-on.exit")

test_that("decorated function calls records retv on success", {
    capture <- list()

    f <- function(a,b,c) { if (a) b + c else stop("an error") }

    d <- decorate_with_on.exit(
        f,
        "f",
        NULL,
        record_fun=function(...) capture <<- list(...)
    )

    expect_equal(formals(d), formals(d))
    expect_equal(environment(d), environment(f))
    expect_equal(attributes(d), list(`__genthat_original_fun`=f))

    expect_equal(d(TRUE, 1L, 2L), 3L)

    expect_equal(capture$name, "f")
    expect_equal(capture$pkg, NULL)
    expect_equal(capture$args, list(a=TRUE, b=1L, c=2L))
    expect_equal(capture$retv, 3L)

    # TEST error
    capture <- list()
    expect_error(d(FALSE, 1L, 2L))
    expect_length(capture, 0)
})

test_that("decorated functions can be multiline function", {
    capture <- list()

    f <- function(x, y) {
        x1 <- x + 1L
        y1 <- y + 1L
        x1 + y1
    }

    d <- decorate_with_on.exit(
            f,
            "f",
            NULL,
            record_fun=function(...) capture <<- list(...)
    )

    expect_equal(d(1L, 2L), 5L)

    expect_equal(capture$args, list(x=1L, y=2L))
    expect_equal(capture$retv, 5L)
})

test_that("decorated functions can use return", {
    capture <- list()

    f <- function(x, y) {
        return(x+y)
    }

    d <- decorate_with_on.exit(
            f,
            "f",
            NULL,
            record_fun=function(...) capture <<- list(...)
    )

    expect_equal(d(1L, 2L), 3L)

    expect_equal(capture$args, list(x=1L, y=2L))
    expect_equal(capture$retv, 3L)
})

test_that("decorated function supports ...", {
    capture <- list()

    f <- function(...) sum(...)
    d <- decorate_with_on.exit(
            f,
            "f",
            NULL,
            record_fun=function(...) capture <<- list(...)
    )

    expect_equal(d(a=1L, b=2L, 3L, 4L), 10L)
    expect_equal(capture$args, list(a=1L, b=2L, 3L, 4L))
    expect_equal(capture$retv, 10L)
})

test_that("decorate_with_on.exit decorates a package function", {
    capture <- list()

    d <- decorate_with_on.exit(
            tools::file_path_sans_ext,
            "file_path_sans_ext",
            "tools",
            record_fun=function(...) capture <<- list(...)
    )

    expect_equal(formals(d), formals(tools::file_path_sans_ext))
    expect_equal(environment(d), environment(tools::file_path_sans_ext))

    d("a.b")

    expect_equal(capture$name, "file_path_sans_ext")
    expect_equal(capture$pkg, "tools")
    expect_equal(capture$args, list(x="a.b"))
    expect_equal(capture$retv, "a")
})
