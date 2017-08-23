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
