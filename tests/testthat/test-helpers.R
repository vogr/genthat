context("test helpers")

test_that("get_function_name", {
    expect_equal(get_function_name("a"), list(package=NULL, name="a"))
    expect_equal(get_function_name("pkg::a"), list(package="pkg", name="a"))
    expect_equal(get_function_name("pkg:::a"), list(package="pkg", name="a"))
    expect_error(get_function_name(""))
    expect_error(get_function_name(NULL))
})
