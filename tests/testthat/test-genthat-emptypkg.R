context("genthat on empty package")

test_that("trace_package works on an empty package", {
    with_test_pkgs({
        output_dir <- tempfile()

        ret <- trace_package("emptypkg", output_dir=output_dir, quiet=TRUE)

        expect_equal(nrow(ret), 3)
        expect_equal(ret$package, rep("emptypkg", 3))
        expect_equal(ret$tag, rep(NA, 3))
        expect_equal(ret$trace, rep(NA, 3))
        expect_equal(ret$type, rep(NA, 3))
        expect_equal(ret$error, rep(NA, 3))
    })
})
