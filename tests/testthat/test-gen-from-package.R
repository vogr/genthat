context("gen_from_package")

# TODO: move to test-genthat.R

test_that("gen_from_package works on sample pkg", {
    skip_on_cran()
    skip_on_travis()

    output_dir <- tempfile()
    working_dir <- tempfile()

    on.exit(unlink(c(output_dir, working_dir), recursive=TRUE))

    with_test_pkgs({
        ret <- gen_from_package(
            find.package("samplepkg"),
            types=c("examples", "tests"),
            action="generate",
            output_dir=output_dir,
            working_dir=working_dir
        )

        # TODO: better matching after the API is stable
        expect_true(sum(complete.cases(ret)) > 0)
    })
})

test_that("gen_from_package works on empty pkg", {
    skip_on_cran()
    skip_on_travis()

    output_dir <- tempfile()
    working_dir <- tempfile()

    on.exit(unlink(c(output_dir, working_dir), recursive=TRUE))

    with_test_pkgs({
        expect_warning(
            ret <- gen_from_package(
                find.package("emptypkg"),
                types="all",
                output_dir=output_dir,
                working_dir=working_dir,
                quiet=TRUE
            ),
            "No runnable code was found, make sure that the `from` packages were installed with"
        )

        expect_nrow(ret, 0)
        # TODO assert column names and attributes
    })
})

