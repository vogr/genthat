context("gen_from_package")

test_that("gen_from_package works on sample pkg", {
    output_dir <- tempfile()
    working_dir <- tempfile()

    withr::with_temp_libpaths({
        devtools::install_local("samplepkg", quick=TRUE, quiet=TRUE, lib=.libPaths()[1])
        ret <- gen_from_package("samplepkg", types=c("examples", "tests"), output_dir=output_dir, working_dir=working_dir, quiet=TRUE)
        expect_equal(ret$n_tests, c(2, 4, NA, 6))
    })
})

test_that("gen_from_package works on empty pkg", {
    output_dir <- tempfile()
    working_dir <- tempfile()

    withr::with_temp_libpaths({
        devtools::install_local("emptypkg", quick=TRUE, quiet=TRUE, lib=.libPaths()[1])
        ret <- gen_from_package("emptypkg", types=c("examples", "tests"), output_dir=output_dir, working_dir=working_dir, quiet=TRUE)
        expect_equal(ret$n_tests, as.integer(c(NA, NA)))
    })
})

