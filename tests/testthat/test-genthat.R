context("genthat integration tests")

test_that("gen_from_package works on samplepkg tests", {
    tmp_dir <- tempfile()
    on.exit({
        #unlink(tmp_dir, recursive=TRUE)
        detach(package:samplepkg)
    })

    res <- gen_from_package("samplepkg", output_dir=tmp_dir, type="tests", quiet=TRUE)

    devtools::load_all("samplepkg")

    x <- testthat::test_dir(tmp_dir)
    x <- as.data.frame(x)

    expect_equal(nrow(x), 4)
    expect_equal(sum(x[["failed"]]), 0)
    expect_equal(sum(x[["error"]]), 0)
})
