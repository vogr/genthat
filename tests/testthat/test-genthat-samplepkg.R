context("genthat on sample package")

test_that("trace_package works on a sample package", {
    withr::with_temp_libpaths({
        devtools::install_local("samplepkg", quiet=TRUE, build_vignettes=TRUE)

        tags <- c("My-add.Rd", "My-call.Rd", "My-public.Rd", "testthat", "my-ext-vignette-notrace", "my-ext-vignette-trace", "my-vignette")
        output_dir <- tempfile()
        files <- file.path(output_dir, paste0(tags, "-1.RDS"))
        files[3] <- NA # this one does not have any traces
        files[5] <- NA # this one does not have any traces

        ret <- trace_package("samplepkg", output_dir=output_dir, quiet=TRUE)

        expect_equal(ret$tag, tags)
        expect_equal(ret$filename, files)
        expect_equal(ret$n_traces, c(2, 4, 0, 6, 0, 1, 1))
        expect_equal(ret$status, rep(0, 7))
        expect_equal(ret$running_time > 0, rep(TRUE, 7))
        expect_equal(ret$package, rep("samplepkg", 7))
        expect_equal(ret$type, c("examples", "examples", "examples", "tests", "vignettes", "vignettes", "vignettes"))
    })
})

