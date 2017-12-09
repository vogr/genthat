context("gen_from_package")

test_that("gen_from_package works on sample pkg", {
    testthat::skip("until the API is stable")

    output_dir <- tempfile()
    working_dir <- tempfile()
    info_file <- tempfile()

    on.exit(unlink(c(output_dir, working_dir, info_file), recursive=TRUE))

    with_test_pkgs({
        ret <- gen_from_package(
            "samplepkg",
            types=c("examples", "tests"),
            output_dir=output_dir,
            working_dir=working_dir,
            quiet=TRUE,
            info_file=info_file
        )

        expect_equal(ret$n_tests, c(2, 4, NA, 8))

        info <- readr::read_csv(info_file)
        expect_equal(nrow(info), 14)

        run <- lapply(info[, "test_file"]$test_file, source, verbose=FALSE)
        run <- sapply(run, `[[`, "value")
        expect_equal(run, rep(TRUE, 14))
    })
})

test_that("gen_from_package works on empty pkg", {
    testthat::skip("until the API is stable")

    output_dir <- tempfile()
    working_dir <- tempfile()
    info_file <- tempfile()

    on.exit(unlink(c(output_dir, working_dir, info_file), recursive=TRUE))

    with_test_pkgs({
        ret <- gen_from_package(
            "emptypkg",
            types=c("examples", "tests"),
            output_dir=output_dir,
            working_dir=working_dir,
            quiet=TRUE,
            info_file=info_file
        )
        expect_equal(ret$n_tests, as.integer(c(NA, NA)))

        expect_false(file.exists(info_file))
    })
})

