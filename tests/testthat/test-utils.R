context("utils")

test_that("environment_name_as_code works", {
    expect_equal(environment_name_as_code(globalenv()), ".GlobalEnv")
    expect_equal(environment_name_as_code(baseenv()), ".BaseNamespaceEnv")
    expect_equal(environment_name_as_code(environment(unzip)), "getNamespace(\"utils\")")

    e <- new.env()
    attr(e, "name") <- "x"

    expect_equal(environmentName(e), "x")
    expect_equal(environment_name_as_code(e), "")

    expect_equal(environment_name_as_code(new.env()), "")
})
