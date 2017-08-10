context("integration tests")

skip_if_not_integration_test <- function() {
    skip_if_not(getOption("genthat.run_itests") == TRUE)
}

test_that("gen_from_package works on stringr", {
    skip_if_not_integration_test()

    withr::with_temp_libpaths({
        install.packages(
            "stringr",
            type="source",
            repos="https://cloud.r-project.org",
            quiet=TRUE,
            INSTALL_opts=c('--example', '--install-tests', '--with-keep.source', '--no-multiarch')
        )

        output_dir <- tempfile()
        on.exit(unlink(output_dir, recursive=TRUE))
        ret <- gen_from_package("stringr", output_dir=output_dir, quiet=TRUE)

        n_traces <- sum(ret$n_traces)

        # a bit of guess :-)
        expect_true(n_traces > 500)

        traces <- lapply(ret$filename, readRDS)
        traces <- unlist(traces, recursive=FALSE)
        expect_equal(length(traces), n_traces)
    })
})
