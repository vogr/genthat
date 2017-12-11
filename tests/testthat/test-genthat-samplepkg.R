context("genthat on sample package")

test_that("trace_package works on a sample package", {
    with_test_pkgs({
        output_dir <- tempfile()
        f1 <- tempfile()
        f2 <- tempfile()
        f3 <- tempfile()
        f4 <- tempfile()

        on.exit({
            file.remove(c(f1, f2, f3))
            unlink(output_dir, recursive=TRUE)
        })

        # a trace
        cat("samplepkg::my_add(1,1)", file=f1)
        # no trace
        cat("1+1", file=f2)
        # error
        cat("errorrrr!", file=f3)
        # f4 is deliberately missing

        ret <- trace_package("samplepkg", c(f1, f2, f3, f4), output_dir=output_dir, action="export", quiet=TRUE)

        expect_equal(length(ret), 4)

        expect_equal(ret[[f1]]$file, f1)
        expect_equal(ret[[f1]]$trace, file.path(output_dir, "samplepkg", "my_add", "trace-0.RDS"))
        expect_equal(ret[[f1]]$type, "C")
        expect_equal(ret[[f1]]$error, NA)

        expect_equal(ret[[f2]], 0)

        expect_equal(ret[[f3]], 1)

        expect_true(is.character(ret[[f4]]))
    })
})

test_that("gen_from_package works on a sample package", {
    with_test_pkgs({
        output_dir <- tempfile()

        on.exit({
            unlink(output_dir, recursive=TRUE)
        })

        ret <- gen_from_package(
            "samplepkg",
            types="all",
            tracer="sequence",
            output_dir=output_dir,
            action="export",
            quiet=TRUE
        )

        expect_equal(nrow(ret), 18)
    })
})

