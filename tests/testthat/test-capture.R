context("capture")

test_that("on_function_entry correctly evaluates and stores arguments", {
    on.exit(reset_call_traces())

    a <- 1
    b <- 2

    g <- function(y) a + y
    f <- function(x, y) x + y

    on_function_entry(1, "f", fun=f, args=list(x=quote(b + 2), y=quote(g(1))))

    t <- get_call_trace(1)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(b + 2), y=quote(g(1))))
    expect_equal(length(t$globals), 2)
    expect_equal(t$globals$b, 2)
    expect_equal(t$globals$g$args, formals(g))
    expect_equal(t$globals$g$body, body(g))
    expect_equal(t$globals$g$globals, list(a=1))
})

test_that("on_function_entry correctly evaluates and stores arguments with ...", {
    on.exit(reset_call_traces())

    a <- 1
    b <- 2
    c <- 3
    d <- 4

    g <- function(y) a + y + h(b)
    h <- function(x) x + c + d
    f <- function(x, ...) x

    # f(b+1, 2, 3, g(c))
    on_function_entry(1, "f", fun=f, args=list(quote(b + 1), 2, 3, quote(g(c))))

    t <- get_call_trace(1)
    expect_equal(t$fun, "f")
    expect_equal(t$args, list(quote(b + 1), 2, 3, quote(g(c))))
    expect_equal(length(t$globals), 3)
    expect_equal(t$globals$b, 2)
    expect_equal(t$globals$c, 3)
    expect_equal(t$globals$g$globals$a, 1)
    expect_equal(t$globals$g$globals$h$globals, list(d=4))
})

test_that("on_function_entry defaults", {
    on.exit(reset_call_traces())

    f <- function(x) {
        on_function_entry(1, "f", as.list(match.call())[-1])
    }

    f()

    t <- get_call_trace(1)
})

test_that("on_function_entry defaults with the same names", {
    on.exit(reset_call_traces())

    x <- 1
    f <- function(x, y) {
        on_function_entry(1, "f", as.list(match.call())[-1])
    }

    f(x, 2)

    t <- get_call_trace(1)
    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(x), y=2))
    expect_equal(length(t$globals), 1)
    expect_equal(t$globals$x, 1)
})

test_that("on_function_exit records exit value", {
    on.exit(reset_call_traces())

    set_call_trace(1, create_trace("f"))

    on_function_exit(1, 2)

    expect_equal(get_call_trace(1), create_trace("f", retv=2))
})

test_that("create_trace returns entry trace if no return value is specified", {
    expect_true(methods::is(create_trace(fun="f"), "genthat_trace_entry"))
    expect_true(methods::is(create_trace(fun="f", retv=1), "genthat_trace"))
})

test_that("find_symbol_env finds symbols", {

    e1 <- new.env()
    e2 <- new.env(parent=e1)
    e3 <- new.env(parent=e2)

    e3$a <- 3
    e2$a <- 2
    e1$a <- 1

    e2$b <- 2

    e1$c <- 1

    expect_equal(find_symbol_env("a", e3), e3)
    expect_equal(find_symbol_env("b", e3), e2)
    expect_equal(find_symbol_env("c", e3), e1)
    expect_equal(find_symbol_env("d", e3), NULL)
})

test_that("find_symbol_env finds the earlier symbol", {
    var <- 1 # this shall conflict with stats::var

    x <- function() {
        var
    }

    expect_equal(x(), 1)
    expect_identical(find_symbol_env("var", environment(x)), environment(x))
})

test_that("get_symbol_values gets values", {
    env <- new.env(parent=globalenv())
    env$a <- 1
    env$b <- 2

    expect_equal(get_symbol_values(c("a", "b"), env), list(a=1, b=2))
    expect_equal(get_symbol_values(c(), env), list())
    expect_equal(get_symbol_values("unzip", globalenv()), list(unzip=quote(utils:::unzip)))

    # it is a named list for which expect_equal won't work
    expect_equivalent(get_symbol_values("+", globalenv()), list())
    expect_equal(get_symbol_values("+", globalenv(), include_base_symbols=TRUE), list(`+`=quote(base:::`+`)))

    expect_warning(get_symbol_values("does-not-exists", new.env(parent=emptyenv())))
})

test_that("get_symbol_names gets unique variables names form an expression", {
    expect_equal(get_symbol_names(list(quote(a + b + b + c))), c("+", "a", "b", "c"))
    expect_equal(get_symbol_names(list(quote(a + b + b + c), quote(b + c + d))), c("+", "a", "b", "c", "d"))
})

test_that("extract_closure works for a simple function", {
    a <- 1
    f <- function(x) a + x

    sc <- extract_closure(f)
    expect_equal(sc$args, formals(f))
    expect_equal(sc$body, body(f))
    expect_equal(sc$globals, list(a=1))
    expect_equal(attr(sc, "class"), "genthat_closure")
})

test_that("extract_closure works for nested closures", {
    a <- 1
    b <- 2

    f <- function(x) a + x + b
    g <- function() f(b)

    sc <- extract_closure(g)
    expect_equal(sc$args, formals(g))
    expect_equal(sc$body, body(g))
    expect_equal(sc$globals$b, 2)
    expect_equal(sc$globals$f$args, formals(f))
    expect_equal(sc$globals$f$body, body(f))
    expect_equal(sc$globals$f$globals, list(a=1))
})

test_that("extract_closure works with references", {
    a <- mtcars
    f <- function() mtcars$cyl*a$wt

    sc <- extract_closure(f)
    expect_equal(sc$args, formals(f))
    expect_equal(sc$body, body(f))
    expect_equal(sc$globals$mtcars, quote(datasets:::mtcars))

    # this one is not a reference
    expect_equal(sc$globals$a, mtcars)
    expect_equal(attr(sc, "class"), "genthat_closure")
})

test_that("extract_closure works with default values", {
    a <- function() mtcars
    f <- function(x=a, y=1) x() * y

    sc <- extract_closure(f)
    expect_equal(sc$args, formals(f))
    expect_equal(sc$body, body(f))
    expect_equal(sc$globals$a$args, formals(a))
    expect_equal(sc$globals$a$body, body(a))
    expect_equal(sc$globals$a$globals$mtcars, quote(datasets:::mtcars))
})

test_that("extract_closure works with recursion", {
    f <- function() f()

    sc <- extract_closure(f)
    expect_equal(sc$args, formals(f))
    expect_equal(sc$body, body(f))
    expect_equal(sc$globals, list())
})

test_that("extract_closure works with cycles", {
    a <- function() b()
    b <- function() c()
    c <- function() a()

    sc <- extract_closure(c)
    expect_equal(sc$args, formals(c))
    expect_equal(sc$body, body(c))
    expect_equal(sc$globals$a$args, formals(a))
    expect_equal(sc$globals$a$body, body(a))
    expect_equal(sc$globals$a$globals$b$args, formals(b))
    expect_equal(sc$globals$a$globals$b$body, body(b))
    expect_equal(sc$globals$a$globals$b$globals, list())
})

test_that("on_function_entry resolves caller and callee environments", {
    env <- new.env(parent=baseenv())
    env$a <- function(x) x
    env$b <- function(x) x+2
    env$d <- function(x) x+3
    env$e <- function(x) x+4
    env$f <- function(x=a(1), y=2*a(2), z=d(3)+e(4), ...) { b(1) }
    environment(env$f) <- env

    a <- 10
    d <- 30
    e <- 40
    b <- function(x=d(), y=e) {
        d <- 300
        x+20+d+e
    }

    ## browser()
    # env$f(y=2*x, a+x+y)
    on_function_entry(1, "f", list(x=quote(b(d)), quote(a)), fun=env$f)

    t <- get_call_trace(1)
    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(b(d)), quote(a)))
    expect_equal(length(t$globals), 3)
    expect_equal(t$globals$a, 10)
    expect_equal(t$globals$d, 30)
    expect_equal(t$globals$b$args, formals(b))
    expect_equal(t$globals$b$body, body(b))
    expect_equal(t$globals$b$globals, list(e=40))
})

# I need free variables
# all non-functions in user-defined envs I can serialize straight away
# all functions in user-defined envs I can serialize
# all functions in packages I need to import into a namespace
# for all functions in user-defined envs I have to do the same

test_that("get_call_trace works", {
    on.exit(reset_call_traces())

    t <- create_trace("f")
    set_call_trace(1, t)

    expect_equal(get_call_trace(1), t)
    expect_error(get_call_trace(2))
})
