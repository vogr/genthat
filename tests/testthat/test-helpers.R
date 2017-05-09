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

test_that("list_merge works", {
    expect_equal(list_merge(list(a=1, b=2), list(3, 4)), list(a=1, b=2, 3, 4))
    expect_equal(list_merge(list(1, 2), list(3, 4)), list(1, 2, 3, 4))
    expect_equal(list_merge(list(1, 2), list(a=3, b=4)), list(1, 2, a=3, b=4))
    expect_equal(list_merge(list(a=1), list(b=2)), list(a=1, b=2))
    expect_equal(list_merge(list(a=1), list(a=2)), list(a=2))
    expect_equal(list_merge(list(0, a=1), list(a=2, b=3)), list(0, a=2, b=3))
    expect_equal(list_merge(list(0, a=1), list(a=2, b=3, 4)), list(0, a=2, b=3, 4))
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
