context("genthat on sample package")

test_that("trace_package works on a sample package", {
    with_test_pkgs({
        tags <- c(
            "My-add.Rd",
            "My-call.Rd",
            "My-public.Rd",
            "testthat",
            "my-ext-vignette-notrace",
            "my-ext-vignette-trace",
            "my-vignette"
        )

        output_dir <- tempfile()

        ret <- trace_package("samplepkg", output_dir=output_dir, tracer="sequence", quiet=TRUE)

        expect_equal(ret$package, rep("samplepkg", 16))
        expect_equal(ret$type, rep("C", 16))
        expect_equal(ret$error, rep(NA, 16))

        # examples
        examples <- dplyr::filter(ret, startsWith(tag, "examples"))
        tests <- dplyr::filter(ret, startsWith(tag, "tests"))
        vignettes <- dplyr::filter(ret, startsWith(tag, "vignettes"))

        # my_public is not there
        expect_equal(examples$tag, c(rep("examples/My-add.Rd.R", 2), rep("examples/My-call.Rd.R", 4)))
        expect_equal(tests$tag, c(rep("tests/testthat.R", 8)))
        expect_equal(vignettes$tag, c("vignettes/my-ext-vignette-trace.R", "vignettes/my-vignette.R"))

    })
})

