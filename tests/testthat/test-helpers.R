context("test helpers")

test_that("get_function_name", {
    expect_equal(get_function_name("a"), list(package=NULL, name="a"))
    expect_equal(get_function_name("pkg::a"), list(package="pkg", name="a"))
    expect_equal(get_function_name("pkg:::a"), list(package="pkg", name="a"))
    expect_error(get_function_name(""))
    expect_error(get_function_name(NULL))
})

test_that("contains", {
    expect_true(contains(list(a=1, b=2), "a"))
    expect_false(contains(list(a=1, b=2), "c"))
    expect_error(contains("", "a"))
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
