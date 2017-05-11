context("genthat integration tests")

skip_if_not_integration_test <- function() {
    skip_if_not(getOption("genthat.run_integration_tests") == TRUE)
}

test_that("closures", {
    on.exit(detach(package:samplepkg))
    on.exit({reset_call_traces(); reset_replacements()}, add=TRUE)


    devtools::load_all("samplepkg")

    decorate_functions(samplepkg::my_call)
    decorate_functions(samplepkg::my_add)

    samplepkg::my_call(samplepkg::my_add, 1, 2)

    t <- get_call_trace(0)
    expect_equal(t$fun, "samplepkg::my_call")
    expect_equal(t$args, list(fn=quote(samplepkg::my_add), 1, 2))
    expect_equal(t$globals, list())

    expect_equal(t$retv, 3)

    t <- get_call_trace(1)
    expect_equal(t$fun, "samplepkg::my_add")
    expect_equal(t$args, list(a=1, b=2))
    expect_equal(t$globals, list())
    expect_equal(t$retv, 3)
})

test_that("gen_from_package works on samplepkg tests", {
    skip_if_not_integration_test()

    tmp_dir <- tempfile()

    if (is_debug_enabled()) {
        message("The working dir: ", tmp_dir)
        cat("The working dir: ", tmp_dir, "\n")
    }

    on.exit({
        if (!is_debug_enabled()) {
            unlink(tmp_dir, recursive=TRUE)
        }
        detach(package:samplepkg)
    })

    res <- gen_from_package(
        "samplepkg",
        output_dir=tmp_dir,
        type="tests",
        quiet=!is_debug_enabled())

    if (is_debug_enabled()) {
        message("Output from gen_from_package():")
        print(res)
    }

    expect_equal(length(res$traces), 7)
    expect_equal(length(res$errors), 1)
    expect_equal(length(res$failures), 0)
    expect_equal(length(res$tests), 6)

    devtools::load_all("samplepkg")

    x <- testthat::test_dir(tmp_dir, reporter="silent")
    x <- as.data.frame(x)

    expect_equal(nrow(x), 6)
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

    pkg <- download_package(
        "stringr",
        pkg_tmp_dir,
        repos=c("CRAN"="https://cran.rstudio.com/"),
        quiet=!is_debug_enabled())

    res <- gen_from_package(pkg$path, output_dir=tmp_dir, type="tests", quiet=!is_debug_enabled())
    if (is_debug_enabled()) {
        message("Output from gen_from_package():")
        print(res)
    }

    # somthing is captured - good for now
    expect_true(length(res$traces) > 0)
})
