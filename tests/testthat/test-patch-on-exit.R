context("patch-on-exit")

test_that("patched_on_exit works", {
    l1 <- list()
    l2 <- list()

    f <- function() {
        on.exit({l1 <<- append(l1, 1L)})
        on.exit({l1 <<- append(l1, 2L)})

        TRUE
    }

    f()
    expect_equal(as.numeric(l1), c(2L))

    g <- function() {
        patched_on_exit({l2 <<- append(l2, 3L)})
        patched_on_exit({l2 <<- append(l2, 4L)})

        TRUE
    }

    g()
    expect_equal(as.numeric(l2), c(3L, 4L))
})

test_that("patching works", {
    l <- list()

    f <- function(i, j) {
        on.exit({l <<- append(l, i)})
        on.exit({l <<- append(l, j)})

        TRUE
    }

    f(1L, 2L)
    expect_equal(as.numeric(l), c(2L))

    patch_on_exit()

    l <- list()
    f(3L, 4L)
    expect_equal(as.numeric(l), c(3L, 4L))

    reset_on_exit()

    l <- list()
    f(5L, 6L)
    expect_equal(as.numeric(l), c(6L))

})
