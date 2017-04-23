context("replacements")

test_that("reassigns replaces function body and add attributes", {
    f <- function(a,b) {a+b}
    g <- function(a,b) {a-b}
    attr(g, "a") <- TRUE

    reassign_function(f, g)
    
    expect_equal(f(1, 2), -1)
    expect_equal(attr(f, "a"), TRUE)
})

test_that("remove_replacement", {
    f <- function() {}
    
    r <- create_replacement("a", environment(f), f, f, f)
    add_replacement(r)

    r2 <- remove_replacement("a")

    expect_equal(r, r2)
    expect_equal(length(get_replacements()), 0)
})


test_that("reset_function", {
    devtools::load_all("samplepkg")
    on.exit(detach(package:samplepkg))

    decorate_functions(samplepkg::public_fn)

    expect_true(is_decorated(samplepkg::public_fn))

    reset_functions(samplepkg::public_fn)

    expect_false(is_decorated(samplepkg::public_fn))
    expect_equal(length(get_replacements()), 0)
})
