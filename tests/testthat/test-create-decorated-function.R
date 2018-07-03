context("create-decorated-function")

test_that("create_decorated_function creates function with onentry decoration", {
    f <- function(x) 42

    capture <- list()
    fd <- create_decorated_function(fun=f, name='f', package=NULL, onentry=function(info) {
        capture <<- info
    })

    expect_equal(fd(1), 42)

    expect_length(capture, 5)
    with(capture, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(decorator, "onentry")
    })
})

test_that("create_decorated_function creates function with onexit decoration", {
    f <- function(x) 42

    capture <- list()
    fd <- create_decorated_function(fun=f, name='f', package=NULL, onexit=function(info) {
        capture <<- info
    })

    expect_equal(fd(1), 42)

    expect_length(capture, 7)
    with(capture, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(retv, 42)
        expect_equal(args, list(x=1))
        expect_equal(decorator, "onexit")
    })
})

test_that("create_decorated_function creates function with onerror decoration", {
    f <- function(x) if (x) 42 else stop("error in f")

    capture <- list()
    fd <- create_decorated_function(fun=f, name='f', package=NULL, onerror=function(info) {
        capture <<- info
    })

    expect_equal(fd(TRUE), 42)
    expect_length(capture, 0)

    capture <- list()
    expect_error(fd(FALSE), "error in f")

    expect_length(capture, 7)
    with(capture, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(args, list(x=FALSE))
        expect_equal(message, "error in f")
        expect_equal(decorator, "onerror")
    })
})

test_that("create_decorated_function creates function with onentry and onerror decoration", {
    f <- function(x) if (x) 42 else stop("error in f")

    capture <- list()
    fd <- create_decorated_function(fun=f, name='f', package=NULL,
        onentry=function(info) {
            capture$onentry <<- info
        },
        onerror=function(info) {
            capture$onerror <<- info
        }
    )

    expect_equal(fd(TRUE), 42)
    expect_length(capture, 1)
    with(capture$onentry, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(decorator, "onentry")
    })

    capture <- list()
    expect_error(fd(FALSE), "error in f")
    expect_length(capture, 2)

    expect_length(capture$onentry, 5)
    with(capture$onentry, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(decorator, "onentry")
    })

    expect_length(capture$onerror, 7)
    with(capture$onerror, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(args, list(x=FALSE))
        expect_equal(message, "error in f")
        expect_equal(decorator, "onerror")
    })
})

test_that("create_decorated_function creates function with onentry, onexit and onerror decoration", {
    f <- function(x) if (x) 42 else stop("error in f")

    capture <- list()
    fd <- create_decorated_function(fun=f, name='f', package=NULL,
        onentry=function(info) {
            capture$onentry <<- info
        },
        onexit=function(info) {
            capture$onexit <<- info
        },
        onerror=function(info) {
            capture$onerror <<- info
        }
    )

    expect_equal(fd(TRUE), 42)
    expect_length(capture, 2)
    with(capture$onentry, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(decorator, "onentry")
    })
    expect_length(capture$onexit, 7)
    with(capture$onexit, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(args, list(x=TRUE))
        expect_equal(retv, 42)
        expect_equal(decorator, "onexit")
    })

    capture <- list()
    expect_error(fd(FALSE), "error in f")
    expect_length(capture, 2)

    expect_length(capture$onentry, 5)
    with(capture$onentry, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(decorator, "onentry")
    })
    expect_length(capture$onerror, 7)
    with(capture$onerror, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(args, list(x=FALSE))
        expect_equal(message, "error in f")
        expect_equal(decorator, "onerror")
    })
})

test_that("create_decorated_function creates function with onentry and onexit decoration", {
    f <- function(x) 42

    capture <- list()
    fd <- create_decorated_function(fun=f, name='f', package=NULL,
        onentry=function(info) {
            capture$onentry <<- info
        },
        onexit=function(info) {
            capture$onexit <<- info
        }
    )

    expect_equal(fd(1), 42)
    expect_length(capture, 2)
    with(capture$onentry, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(decorator, "onentry")
    })
    expect_length(capture$onexit, 7)
    with(capture$onexit, {
        expect_equal(name, "f")
        expect_equal(package, NULL)
        expect_is(env, "environment")
        expect_is(seed, "integer")
        expect_equal(args, list(x=1))
        expect_equal(retv, 42)
        expect_equal(decorator, "onexit")
    })
})

test_that("decorated functions can be multiline function", {
    capture <- list()

    f <- function(x, y) {
        x1 <- x + 1L
        y1 <- y + 1L
        x1 + y1
    }

    d <- create_decorated_function(
        fun=f,
        name="f",
        package=NULL,
        onexit=function(info) capture <<- info
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

    d <- create_decorated_function(
        fun=f,
        name="f",
        package=NULL,
        onexit=function(info) capture <<- info
    )

    expect_equal(d(1L, 2L), 3L)

    expect_equal(capture$args, list(x=1L, y=2L))
    expect_equal(capture$retv, 3L)
})

test_that("decorated function supports ...", {
    capture <- list()

    f <- function(...) sum(...)
    d <- create_decorated_function(
        fun=f,
        name="f",
        package=NULL,
        onexit=function(info) capture <<- info
    )

    expect_equal(d(a=1L, b=2L, 3L, 4L), 10L)
    expect_equal(capture$args, list(a=1L, b=2L, 3L, 4L))
    expect_equal(capture$retv, 10L)
})

test_that("decorate_with_on.exit decorates a package function", {
    capture <- list()

    d <- create_decorated_function(
        fun=tools::file_path_sans_ext,
        name="file_path_sans_ext",
        package="tools",
        onexit=function(info) capture <<- info
    )

    expect_equal(formals(d), formals(tools::file_path_sans_ext))
    expect_equal(environment(d), environment(tools::file_path_sans_ext))

    d("a.b")

    expect_equal(capture$name, "file_path_sans_ext")
    expect_equal(capture$package, "tools")
    expect_equal(capture$args, list(x="a.b"))
    expect_equal(capture$retv, "a")
})

test_that("test __genthat_tmp", {
    `f<-` <- function(x, y, value) {
        x[y] <- value
        x
    }

    `g<-` <- create_decorated_function(
        fun=`f<-`,
        name="f<-",
        package=NULL,
        onexit=function(info) {
            expect_equal(get("__genthat_tmp", envir=sys.frame(-1)), list(a=1))
        }
    )

    x <- list(a=1)
    g(x, 1) <- 2
    expect_equal(x$a, 2)
})
