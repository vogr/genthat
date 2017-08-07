## context("genthat integration tests")

if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools needed for this function to work. Please install it.", call. = FALSE)
}

skip_if_not_integration_test <- function() {
    skip_if_not(getOption("genthat.run_integration_tests") == TRUE)
}

test_that("gen_from_package works on samplepkg tests", {
    skip_if_not_integration_test()

    ret <- trace_package("samplepkg", genthat::run_package("samplepkg"))
    expect_equal(length(ret$traces), 9)
    expect_equal(length(ret$replacements), 6)

    tests <- generate_tests(ret$traces, include_trace_dump=TRUE)

    expect_equal(nrow(tests), 9)
    expect_true(all(is.na(tests$error)))
    expect_equal(sum(is.na(tests$code)), 1)

    tests <- tests[!is.na(tests$code),]
    devtools::load_all("samplepkg")
    runs <- genthat::run_generated_tests(tests)

    expect_equal(nrow(runs), 8)
    expect_equal(sum(runs$result == 1), 8)
    expect_equal(sum(is.na(runs$error)), 8)
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

    path <- download_package(
        "stringr",
        pkg_tmp_dir,
        extract=TRUE,
        repos=c("CRAN"="https://cran.rstudio.com/"),
        quiet=!is_debug_enabled()
    )

    ret <- trace_package(path, genthat::run_package("stringr"))
    expect_true(length(ret$traces) > 0)
    expect_true(length(ret$replacements) > 0)

    tests <- generate_tests(ret$traces, include_trace_dump=TRUE)

    expect_true(nrow(tests) > 0)
    expect_true(length(is.na(tests$code)) > 0)

    tests <- tests[!is.na(tests$code),]
    devtools::load_all(path)
    runs <- genthat::run_generated_tests(tests)

    expect_true(nrow(runs) > 0)

    cat("\nResults of stringr:\n")
    print(
        data.frame(
            traces=length(ret$traces),
            replacements=length(ret$replacements),
            tests=length(is.na(tests$code)),
            passed=length(runs[!is.na(runs$result), "result"] == 1)
        )
    )
})

test_that("tracing control work", {
    capture <- list()

    f <- function(x,y) x + y
    decorate_functions(f, .recorder=function(...) capture <<- list(...))

    disable_tracing()
    f(1L, 2L)

    expect_equal(is_tracing_enabled(), FALSE)
    expect_equal(length(capture), 0L)

    enable_tracing()
    f(1L, 2L)

    expect_equal(is_tracing_enabled(), TRUE)
    expect_equal(length(capture), 5L)
    expect_equal(capture$retv, 3L)
})
