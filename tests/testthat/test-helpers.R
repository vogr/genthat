context("test helpers")

test_that("get_function_name", {
    expect_equal(get_function_name("a"), "a")
    expect_equal(get_function_name("pkg::a"), c("pkg", "a"))
    expect_equal(get_function_name("pkg:::a"), c("pkg", "a"))
    expect_error(get_function_name(""))
    expect_error(get_function_name(NULL))
})
