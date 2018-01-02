context("capture")

get_trace <- function(tracer, idx) {
    copy_traces(tracer)[[idx]]
}

test_that("record_trace correctly evaluates and stores arguments", {
    tracer <- create_sequence_tracer()

    a <- 1
    b <- 2

    g <- function(y) a + y
    f <- function(x, y) x + y

    record_trace("f", args=list(x=quote(b + 2), y=quote(g(1))), tracer=tracer)

    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(b + 2), y=quote(g(1))))
    expect_equal(length(t$globals), 2)
    expect_equal(t$globals$b, 2)
    expect_equal(formals(t$globals$g), formals(g))
    expect_equal(body(t$globals$g), body(g))
    expect_equal(environment(t$globals$g)$a, 1)
    expect_equal(as.list(environment(t$globals$g)), list(a=1))
})

test_that("record_trace correctly evaluates and stores arguments with ...", {
    tracer <- create_sequence_tracer()

    a <- 1
    b <- 2
    c <- 3
    d <- 4

    g <- function(y) a + y + h(b)
    h <- function(x) x + c + d
    f <- function(x, ...) x

    # f(b+1, 2, 3, g(c))
    record_trace("f", args=list(quote(b + 1), 2, 3, quote(g(c))), tracer=tracer)

    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(quote(b + 1), 2, 3, quote(g(c))))
    expect_equal(length(t$globals), 3)
    expect_equal(t$globals$b, 2)
    expect_equal(t$globals$c, 3)
    expect_equal(environment(t$globals$g)$a, 1)
    expect_equal(environment(environment(t$globals$g)$h)$d, 4)
})

test_that("record_trace called from a function body with argument matching", {
    tracer <- create_sequence_tracer()

    # kind of a simulation of the injected code
    f <- function(x) {
        record_trace("f", pkg=NULL, as.list(match.call())[-1], tracer=tracer)
    }

    f()

    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list())
    expect_equal(t$globals, list())
})

test_that("record_trace correctly resolves the names in lexical scopes", {
    tracer <- create_sequence_tracer()

    # x is here
    x <- 1
    # and x is here
    f <- function(x, y) {
        record_trace("f", pkg=NULL, as.list(match.call())[-1], tracer=tracer)
    }

    f(x, 2)

    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(x), y=2))
    expect_equal(length(t$globals), 1)
    expect_equal(t$globals$x, 1)
})

test_that("find_symbol_env finds symbols", {
    e1 <- new.env(parent=emptyenv())
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

test_that("find_symbol_env finds correct function origins", {
    e1 <- new.env(parent=emptyenv())
    e2 <- new.env(parent=e1)
    e_g <- new.env(parent=emptyenv())

    g <- function() {}
    environment(g) <- e_g

    e1$g <- g
    # the symbol is not defined in e_g
    expect_equal(find_symbol_env("g", e2), e1)

    e_g$g <- g
    expect_equal(find_symbol_env("g", e2), e_g)
})

test_that("find_symbol_env resolves %>% (#74)", {
    library(dplyr)
    expect_true("package:dplyr" %in% search())
    expect_equal(find_symbol_env("%>%", globalenv()), asNamespace("magrittr"))
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
    expect_equal(get_symbol_values("unzip", globalenv()), list(unzip=quote(utils::unzip)))

    # it is a named list for which expect_equal won't work
    expect_equivalent(get_symbol_values("+", globalenv()), list())
    expect_equal(get_symbol_values("+", globalenv(), include_base_symbols=TRUE), list(`+`=as.name("+")))
    expect_length(get_symbol_values("does-not-exists"), 0)
})

test_that("get_symbol_names gets unique variables names form an expression", {
    expect_equal(get_symbol_names(list(quote(a + b + b + c))), c("+", "a", "b", "c"))
    expect_equal(get_symbol_names(list(quote(a + b + b + c), quote(b + c + d))), c("+", "a", "b", "c", "d"))
})

test_that("extract_closure works for a simple function", {
    a <- 1
    f <- function(x) a + x

    sc <- extract_closure(f)
    expect_equal(attr(sc, "genthat_extracted_closure"), TRUE)
    expect_equal(formals(sc), formals(f))
    expect_equal(body(sc), body(f))
    expect_equal(as.list(environment(sc)), list(a=1))
    expect_identical(parent.env(environment(sc)), baseenv())
})

test_that("extract_closure works for nested closures", {
    a <- 1
    b <- 2

    f <- function(x) a + x + b
    g <- function() f(b)

    sc <- extract_closure(g)
    expect_equal(formals(sc), formals(g))
    expect_equal(body(sc), body(g))
    expect_equal(environment(sc)$b, 2)
    expect_identical(parent.env(environment(sc)), baseenv())
    expect_equal(formals(environment(sc)$f), formals(f))
    expect_equal(body(environment(sc)$f), body(f))
    expect_equal(as.list(environment(environment(sc)$f)), list(a=1))
    expect_identical(parent.env(environment(environment(sc)$f)), environment(sc))
})

test_that("extract_closure only links environments when necessary", {
    a <- 1
    b <- 2
    c <- 3

    # this one needs to be linked
    g <- function(x) x + b

    ## debug(extract_closure)
    sc <- extract_closure(function(x) g(x) + b)
    env_g <- environment(environment(sc)$g)
    expect_equal(length(env_g), 0)
    expect_identical(parent.env(env_g), environment(sc))

    # this one does not need to be linked
    g <- function(x) x + x

    sc <- extract_closure(function(x) g(x) + b)
    env_g <- environment(environment(sc)$g)
    expect_equal(length(env_g), 0)
    expect_identical(parent.env(env_g), baseenv())


    # this one contains c, but does not need to be linked
    g <- function(x) x + c
    sc <- extract_closure(function(x) g(x) + b)
    env_g <- environment(environment(sc)$g)
    expect_equal(as.list(env_g), list(c=3))
    expect_identical(parent.env(env_g), baseenv())


    # this one contains c and needs to be linked
    g <- function(x) x + c + b
    sc <- extract_closure(function(x) g(x) + b)
    env_g <- environment(environment(sc)$g)
    expect_equal(as.list(env_g), list(c=3))
    expect_identical(parent.env(env_g), environment(sc))
})

# test for https://github.com/PRL-PRG/genthat/issues/16
test_that("extract_closure works with references", {
    a <- mtcars
    f <- function() mtcars$cyl*a$wt

    sc <- extract_closure(f)
    expect_equal(formals(sc), formals(f))
    expect_equal(body(sc), body(f))
    expect_equal(length(environment(sc)), 2)

    # this one is a reference
    expect_equal(environment(sc)$mtcars, quote(datasets::mtcars))

    # this one is not a reference
    expect_equal(environment(sc)$a, mtcars)
})

test_that("get_variable_value_or_reference uses correctly :: and :::", {
    # from package environment
    expect_equal(get_variable_value_or_reference("zip", as.environment("package:utils")), quote(utils::zip))
    # from namespace, but exported
    expect_equal(get_variable_value_or_reference("zip", asNamespace("utils")), quote(utils::zip))
    # from namespace, but exported
    expect_equal(get_variable_value_or_reference("specialOps", asNamespace("utils")), quote(utils:::specialOps))

    # value
    g <- function() {}
    expect_equal(get_variable_value_or_reference("g", environment(g)), g)
})

# test for https://github.com/PRL-PRG/genthat/issues/16
test_that("extract_closure works with default values", {
    a <- function() mtcars
    f <- function(x=a, y=1) x() * y

    sc <- extract_closure(f)
    expect_equal(formals(sc), formals(f))
    expect_equal(body(sc), body(f))
    expect_equal(length(environment(sc)), 1)
    expect_equal(formals(environment(sc)$a), formals(a))
    expect_equal(body(environment(sc)$a), body(a))
    expect_equal(environment(environment(sc)$a)$mtcars, quote(datasets::mtcars))
})

test_that("extract_closure works with recursion", {
    f <- function() f()

    sc <- extract_closure(f)
    expect_equal(formals(sc), formals(f))
    expect_equal(body(sc), body(f))
    expect_equal(as.list(environment(sc)), list())
})

test_that("extract_closure works with cycles", {
    a <- function() b()
    b <- function() c()
    c <- function() a()

    sc <- extract_closure(c)
    expect_equal(formals(sc), formals(c))
    expect_equal(body(sc), body(c))
    expect_equal(length(environment(sc)), 1)

    expect_equal(formals(environment(sc)$a), formals(a))
    expect_equal(body(environment(sc)$a), body(a))
    expect_equal(length(environment(environment(sc)$a)), 1)

    expect_equal(formals(environment(environment(sc)$a)$b), formals(b))
    expect_equal(body(environment(environment(sc)$a)$b), body(b))
    expect_equal(length(environment(environment(environment(sc)$a)$b)), 0)
})

test_that("record_trace resolves caller and callee environments", {
    tracer <- create_sequence_tracer()

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
    record_trace("f", pkg=NULL, list(x=quote(b(d)), quote(a)), tracer=tracer)

    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(b(d)), quote(a)))
    expect_equal(length(t$globals), 3)
    expect_equal(t$globals$a, 10)
    expect_equal(t$globals$d, 30)
    expect_equal(formals(t$globals$b), formals(b))
    expect_equal(body(t$globals$b), body(b))
    expect_equal(as.list(environment(t$globals$b)), list(e=40))
})

test_that("capture works with lapply", {
    tracer <- create_sequence_tracer()

    f <- function(x) {
        y <- x+1
        record_trace("f", pkg=NULL, args=as.list(match.call())[-1], retv=y, env=parent.frame(), tracer=tracer)
        y
    }

    lapply(1:10, f)

    for (i in 1:10) {
        t <- get_trace(tracer, i)
        expect_equal(t$globals$i, i)
        expect_equal(t$globals$X, 1:10)
        expect_equal(t$retv, i+1)
    }
})

test_that("get_symbol_value can handle ...", {
     tracer <- create_sequence_tracer()

     f <- function(...) g(...)
     g <- function(...) h(...)
     h <- function(...) {
         record_trace("h", args=as.list(match.call())[-1], tracer=tracer)
     }

     f(10L, 20L)
     traces <- copy_traces(tracer)

     expect_equal(traces[[1]]$args, list(10L, 20L))
     expect_length(traces[[1]]$globals, 0L)
})

test_that("get_symbol_value can handle non-evaluated ..N", {
     tracer <- create_sequence_tracer()

     f <- function(...) g(...)
     g <- function(...) h(..1)
     h <- function(...) {
         record_trace("h", args=as.list(match.call())[-1], tracer=tracer)
     }

     f(10L, 20L)
     traces <- copy_traces(tracer)

     # here it should be empty since none of the value has been actually used
     # and therefore the ..1 promise has never been evaluated
     expect_equal(traces[[1]]$args, list(as.name("..1")))
     expect_length(traces[[1]]$globals, 0L)
})

test_that("get_symbol_value can handle evaluated ..N", {
     tracer <- create_sequence_tracer()

     f <- function(...) g(...)
     g <- function(...) h(..1)
     h <- function(...) {
         ..1 + 1L
         record_trace("h", args=as.list(match.call())[-1], tracer=tracer)
     }

     f(10L*10L, 20L)
     traces <- copy_traces(tracer)

     expect_equal(traces[[1]]$args, list(100L))
     expect_length(traces[[1]]$globals, 0L)
})

test_that("captures nested language objects' global variable", {
    tracer <- create_sequence_tracer()

    a <- 0
    b <- 1
    arg1 <- list(n=quote(a + b + 1), m=1)

    f <- function(x, y) list(x, y)

    record_trace("f", args=list(x=quote(arg1), y=quote(b + 1)), tracer=tracer)
    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(arg1), y=quote(b + 1)))
    expect_equal(length(t$globals), 3)
    expect_equal(t$globals$a, 0)
    expect_equal(t$globals$b, 1)
    expect_equal(t$globals$arg1, arg1)
})

test_that("captures nested language objects' global variables", {
    tracer <- create_sequence_tracer()

    a <- 1
    b <- 2
    c <- 3
    arg1 <- list(n=quote(a + b), m=quote(b + a))
    arg2 <- list(n=quote(list(a + c)), m=quote(1))

    f <- function(x, y, z) list(x, y, z)

    record_trace("f", args=list(x=quote(arg1), y=quote(arg2), z=quote(a)), tracer=tracer)

    t <- get_trace(tracer, 1L)

    expect_equal(t$fun, "f")
    expect_equal(t$args, list(x=quote(arg1), y=quote(arg2), z=quote(a)))
    expect_equal(length(t$globals), 5)
    expect_equal(t$globals$a, 1)
    expect_equal(t$globals$b, 2)
    expect_equal(t$globals$c, 3)
    expect_equal(t$globals$arg1, arg1)
    expect_equal(t$globals$arg2, arg2)
})
