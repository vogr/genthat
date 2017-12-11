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


test_that("reassign_function keeps attributes", {
    f1 <- function(x) x+1
    g1 <- function(y) y+1

    attr(f1, "a1") <- 1
    attr(f1, "a2") <- 2

    attr(g1, "b3") <- 3
    attr(g1, "b4") <- 4

    reassign_function(f1, g1)

    expect_equal(attr(f1, "a1"), 1)
    expect_equal(attr(f1, "a2"), 2)
    expect_equal(attr(f1, "b3"), 3)
    expect_equal(attr(f1, "b4"), 4)
})

test_that("reassign_function keeps attributes", {
    f1 <- function(x) x+1
    g1 <- function(y) y+1

    attr(f1, "a1") <- 1
    attr(f1, "a2") <- 2

    attr(g1, "b3") <- 3
    attr(g1, "b4") <- 4

    reassign_function(f1, g1, keep_only_new_attributes=TRUE)

    expect_null(attr(f1, "a1"))
    expect_null(attr(f1, "a2"))
    expect_equal(attr(f1, "b3"), 3)
    expect_equal(attr(f1, "b4"), 4)
})

