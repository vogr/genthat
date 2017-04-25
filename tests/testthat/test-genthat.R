context("genthat integration tests")

skip_if_not_integration_test <- function() {
    skip_if_not(getOption("genthat.run_integration_tests") == TRUE)
}

test_that("gen_from_package works on samplepkg tests", {
    skip_if_not_integration_test()
    
    tmp_dir <- tempfile()

    if (is_debug_enabled()) {
        message("The working dir: ", tmp_dir)
    }

    on.exit({
        if (!is_debug_enabled()) {
            unlink(tmp_dir, recursive=TRUE)
        }
        detach(package:samplepkg)
    })

    res <- gen_from_package("samplepkg", output_dir=tmp_dir, type="tests", quiet=!is_debug_enabled())
    if (is_debug_enabled()) {
        message("Output from gen_from_package():")
        print(res)
    }

    expect_equal(length(res$traces), 6)
    expect_equal(length(res$errors), 1)
    expect_equal(length(res$failures), 1)
    expect_equal(length(res$tests), 4)

    devtools::load_all("samplepkg")

    x <- testthat::test_dir(tmp_dir, reporter="silent")
    x <- as.data.frame(x)

    expect_equal(nrow(x), 4)
    expect_equal(sum(x[["failed"]]), 0)
    expect_equal(sum(x[["error"]]), 0)
})

test_that("gen_from_package works on stringr", {
    skip_if_not_integration_test()

    pkg_tmp_dir <- tempfile()
    tmp_dir <- tempfile()

    if (is_debug_enabled()) {
        message("The package working dir: ", pkg_tmp_dir)
        message("The working dir: ", tmp_dir)
    }

    on.exit({
        if (!is_debug_enabled()) {
            unlink(tmp_dir, recursive=TRUE)
            unlink(pkg_tmp_dir, recursive=TRUE)
        }
    })

    pkg <- download_package("stringr", pkg_tmp_dir, repos=c("CRAN"="https://cran.rstudio.com/"), quiet=!is_debug_enabled())
    res <- gen_from_package(pkg$path, output_dir=tmp_dir, type="tests", quiet=!is_debug_enabled())
    if (is_debug_enabled()) {
        message("Output from gen_from_package():")
        print(res)
    }

    # somthing is captured - good for now
    expect_true(length(res$traces) > 0)
})
