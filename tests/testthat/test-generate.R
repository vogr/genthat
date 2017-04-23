context("test generation")

test_that("format_args", {
    expect_equal(format_args(create_trace(fun="f", args=NULL)), "")
    expect_equal(format_args(create_trace(fun="f", args=list(1))), "1")
    expect_equal(format_args(create_trace(fun="f", args=list(1, 2))), "1, 2")
    expect_equal(format_args(create_trace(fun="f", args=list(a=1))), "a=1")
    expect_equal(format_args(create_trace(fun="f", args=list(a=1, b=2))), "a=1, b=2")
    expect_equal(format_args(create_trace(fun="f", args=list(a=1, 2, 3))), "a=1, 2, 3")
    expect_equal(format_args(create_trace(fun="f", args=list(a=1, 2, 3, c=4))), "a=1, 2, 3, c=4")
})

test_that("generate_test", {
    trace <- create_trace(fun="f", args=list(1), retv=1)
    
    expect_equal(generate_test(trace), "test_that(\"f\", {\n\texpected <- 1\n\texpect_equal(f(1), expected)\n})")
})

test_that("gen_tests with no traces", {
    # TODO
})

test_that("gen_tests", {
    tmp_dir <- tempfile()
    on.exit(unlink(tmp_dir, recursive=TRUE))

    traces <- list(create_trace(fun="f", args=list(1), retv=2), create_trace(fun="g", args=list(3), retv=4))

    files <- gen_tests(traces, tmp_dir)

    expect_equal(files, file.path(tmp_dir, c("test-1.R", "test-2.R")))
    expect_equal(file.exists(files), c(TRUE, TRUE))
    expect_equal(
        paste(readLines(files[1]), collapse="\n"),
        "test_that(\"f\", {\n\texpected <- 2\n\texpect_equal(f(1), expected)\n})")
    expect_equal(
        paste(readLines(files[2]), collapse="\n"),
        "test_that(\"g\", {\n\texpected <- 4\n\texpect_equal(g(3), expected)\n})")
})

test_that("gen_test with no traces non-error traces", {
    tmp_dir <- tempfile()
    on.exit(unlink(tmp_dir, recursive=TRUE))

    traces <- list(create_trace_error(fun="f", args=list(1), msg="error"), create_trace_error(fun="g", args=list(3), msg="error"))

    files <- gen_tests(traces, tmp_dir)

    expect_equal(length(files), 0)
    expect_equal(length(list.files(tmp_dir)), 0)
})
