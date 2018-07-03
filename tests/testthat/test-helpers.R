context("test helpers")

test_that("split_function_name", {
    expect_equal(split_function_name("a"), list(package=NULL, name="a"))
    expect_equal(split_function_name("pkg::a"), list(package="pkg", name="a"))
    expect_equal(split_function_name("pkg:::a"), list(package="pkg", name="a"))
    expect_error(split_function_name(""))
    expect_error(split_function_name(NULL))
})

test_that("filter works", {
    expect_equal(filter(list(1L, 2, 3, 4L), is.integer), list(1L, 4L))
    expect_equal(filter(list(), is.integer), list())
})

test_that("filter_not works", {
    expect_equal(filter_not(list(1L, 2, 3, 4L), is.integer), list(2, 3))
    expect_equal(filter_not(list(), is.integer), list())
})

test_that("contains_key", {
    expect_true(contains_key(list(a=1, b=2), "a"))
    expect_false(contains_key(list(a=1, b=2), "c"))
    expect_error(contains_key(function() {}, "a"))
})

test_that("filter_idx filters indexes", {
    expect_equal(filter_idx(list(NULL, 2, NULL, 3), is.null), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("filter filters elements where the predicate holds", {
    expect_equal(filter(list(NULL, 2, NULL, 3), is.null), list(NULL, NULL))
})

test_that("filter filters elements where the predicate does not hold", {
    expect_equal(filter_not(list(NULL, 2, NULL, 3), is.null), list(2, 3))
})

test_that("link_environments work",{
    b <- as.function(c(alist(x=0), quote(b())),
        envir=list2env(
            list(
                a=as.function(c(alist(x=0), quote(d())), envir=list2env(
                    list(
                        d=as.function(c(alist(x=0), 1), envir=new.env())
                    ),
                    parent=emptyenv()
                ),
                parent=emptyenv())
            ),
            parent=emptyenv()
        ))

    link_environments()

    b_env <- environment(b)
    a_env <- environment(b_env$a)
    d_env <- environment(a_env$d)

    expect_identical(parent.env(b_env), sys.frame(sys.nframe()))
    expect_identical(parent.env(a_env), b_env)
    expect_identical(parent.env(d_env), a_env)
})

test_that("is_imports_namespace works", {
    expect_false(is_imports_namespace(new.env()))
    expect_false(is_imports_namespace(baseenv()))
    expect_false(is_imports_namespace(globalenv()))

    expect_false(is_imports_namespace(asNamespace("genthat")))
    expect_true(is_imports_namespace(parent.env(asNamespace("genthat"))))
    expect_false(is_imports_namespace(as.environment("package:genthat")))
})

test_that("is_package_environment works", {
    expect_false(is_package_environment(new.env()))
    expect_false(is_package_environment(baseenv()))
    expect_false(is_package_environment(globalenv()))

    expect_false(is_package_environment(asNamespace("genthat")))
    expect_false(is_package_environment(parent.env(asNamespace("genthat"))))
    expect_true(is_package_environment(as.environment("package:genthat")))
})

test_that("is_package_namespace works", {
    expect_false(is_package_namespace(new.env()))
    expect_false(is_package_namespace(baseenv()))
    expect_false(is_package_namespace(globalenv()))

    expect_true(is_package_namespace(asNamespace("genthat")))
    expect_false(is_package_namespace(parent.env(asNamespace("genthat"))))
    expect_false(is_package_namespace(as.environment("package:genthat")))
})

test_that("next_file_in_row works with/without extensions", {
    expect_error(next_file_in_row(""))
    expect_equal(next_file_in_row("does-not-exist"), "./does-not-exist-1")
    expect_equal(next_file_in_row("does-not-exist.R"), "./does-not-exist-1.R")
})

test_that("next_file_in_row works with existing files", {
    f <- tempfile()
    on.exit(file.remove(f))
    writeLines("1", f)

    f1 <- next_file_in_row(f)
    expect_equal(f1, paste0(f, "-1"))
})

test_that("next_file_in_row works", {
    f <- tempfile()

    f1 <- next_file_in_row(f)
    on.exit(file.remove(f1))
    expect_equal(f1, paste0(f, "-1"))
    writeLines("1", f1)

    f2 <- next_file_in_row(f)
    on.exit(file.remove(f2), add=TRUE)
    expect_equal(f2, paste0(f, "-2"))
    writeLines("1", f2)

    f100 <- paste0(f, "-100")
    writeLines("1", f100)
    on.exit(file.remove(f100), add=TRUE)

    f3 <- next_file_in_row(f)
    expect_equal(f3, paste0(f, "-101"))
})

test_that("resolve_package_name returns the package name", {
    expect_equal(resolve_package_name(ls, "ls"), "base")
    expect_equal(resolve_package_name(`%in%`, "%in%"), "base")
    expect_equal(resolve_package_name(tools::Rcmd, "Rcmd"), "tools")
    expect_equal(resolve_package_name(dplyr::`%>%`, "%>%"), "magrittr")

    my_f <- function() {}

    expect_equal(resolve_package_name(my_f, "my_f"), NULL)
})

# this is the way we are actually using the resolve_function
# so we need to test resolve_function2
resolve_function2 <- function(fun, env=parent.frame()) {
    resolve_function(fun, substitute(fun), env)
}

test_that("resolve_function using string", {
    expect_equal(resolve_function2("ls"), list(fqn="base:::ls", name="ls", package="base", fun=ls))
    expect_equal(resolve_function2("base::ls"), list(fqn="base:::ls", name="ls", package="base", fun=ls))
    expect_equal(resolve_function2("base:::ls"), list(fqn="base:::ls", name="ls", package="base", fun=ls))
})

test_that("resolve_function using function refs", {
    expect_equal(resolve_function2(ls), list(fqn="base:::ls", name="ls", package="base", fun=ls))
    expect_equal(resolve_function2(base::ls), list(fqn="base:::ls", name="ls", package="base", fun=ls))
    expect_equal(resolve_function2(base::ls), list(fqn="base:::ls", name="ls", package="base", fun=ls))
})

test_that("resolve_function using quotes", {
    expect_equal(resolve_function2(quote(ls)), list(fqn="base:::ls", name="ls", package="base", fun=ls))
    expect_equal(resolve_function2(quote(base::ls)), list(fqn="base:::ls", name="ls", package="base", fun=ls))
    expect_equal(resolve_function2(quote(base:::ls)), list(fqn="base:::ls", name="ls", package="base", fun=ls))
})

test_that("resolve_function non-existing functions", {
    expect_error(resolve_function2("utils::this_one_does_not_exist_here_123"))
    expect_error(resolve_function2("this_one_does_not_exist_here_123"))
    expect_error(resolve_function2(quote(this_one_does_not_exists)))
})

test_that("resolve_function local function", {
    my_fun <- function() {}
    expect_equal(resolve_function2("my_fun"), list(fqn="my_fun", name="my_fun", package=NULL, fun=my_fun))
    expect_equal(resolve_function2(quote(my_fun)), list(fqn="my_fun", name="my_fun", package=NULL, fun=my_fun))
})

test_that("resolve_function on special symbols", {
    expect_equal(
        resolve_function2(`substr<-`),
        list(fqn="base:::`substr<-`", name="substr<-", package="base", fun=`substr<-`)
    )
    expect_equal(
        resolve_function2(base::`substr<-`),
        list(fqn="base:::`substr<-`", name="substr<-", package="base", fun=`substr<-`)
    )
    expect_equal(resolve_function2(`::`), list(fqn="base:::`::`", name="::", package="base", fun=`::`))
    expect_equal(resolve_function2(`:::`), list(fqn="base:::`:::`", name=":::", package="base", fun=`:::`))

    expect_equal(resolve_function2("::"), list(fqn="base:::`::`", name="::", package="base", fun=`::`))
    expect_equal(resolve_function2(":::"), list(fqn="base:::`:::`", name=":::", package="base", fun=`:::`))
})

test_that("resolve_function in specified env", {
    env <- new.env(parent=emptyenv())
    env$f <- function() {}
    expect_equal(resolve_function2("f", env=env), list(fqn="f", name="f", package=NULL, fun=env$f))
})

test_that("is_s3_generic works", {
    f <- function(x) 42

    expect_true(is_s3_generic(print))
    expect_false(is_s3_generic(f))
})

# TODO: update for link_environments()
## test_that("linked_environment links environments", {
##     e <- linked_environment(
##         a=1,
##         b=2,
##         c={
##             `_c` <- function() a+b+d
##             environment(`_c`) <- linked_environment(
##                 d=3
##             )
##             `_c`
##         })

##     expect_equal(e$c(), 6)
## })
