context("run R code")

test_that("runs R script", {
    expect_equal(run_r_fun(function() "Hi"), 0)
    expect_equal(run_r_fun(function() {
        a <- 1
        b <- 2
        a + b
    }), 0)

    expect_equal(run_r_fun(function() stop("Bye")), 1)
    expect_equal(run_r_fun(function() {
        stop("Bye")
    }), 1)
})

