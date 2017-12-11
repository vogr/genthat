context("genthat")

test_that("tracing control work", {
    capture <- list()

    f <- function(x,y) x + y
    decorate_functions(f, record_fun=function(...) capture <<- list(...))

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

test_that("export_traces work", {
    stats_file <- tempfile()
    output_dir <- tempfile()

    on.exit({
        file.remove(stats_file)
        unlink(output_dir, recursive=TRUE)
    })

    expect_false(file.exists(stats_file))
    expect_false(dir.exists(output_dir))

    trace_1 <- create_trace("fun1")
    trace_2 <- create_trace("fun2", retv=1)
    trace_3 <- create_trace("fun3", error=simpleError("Bad call"))
    trace_4 <- create_trace("fun4", failure=simpleError("Something is wrong"))

    export_traces(list(trace_1), file="f", output_dir=output_dir, stats_file=stats_file)

    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 1)
    expect_equal(stats$file, "f")
    expect_true(endsWith(stats$trace, file.path("_NULL_", "fun1", "trace-0.RDS")))
    expect_equal(stats$type, "I")
    expect_true(is.na(stats$error))
    rds <- readRDS(stats$trace)
    expect_equal(rds, trace_1)

    # this should append
    export_traces(list(trace_2, trace_3, trace_4), file="f", output_dir=output_dir, stats_file=stats_file)

    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 4)
    expect_equal(stats$type, c("I", "C", "E", "F"))
    expect_equal(stats$error, c(NA, NA, NA, "Something is wrong"))
    rdss <- lapply(stats$trace, readRDS)
    expect_length(rdss, 4)
    expect_equal(rdss[[1]], trace_1)
    expect_equal(rdss[[2]], trace_2)
    expect_equal(rdss[[3]], trace_3)
    expect_equal(rdss[[4]], trace_4)

    ## the stats file should not be changed
    export_traces(list(), file="f", output_dir=output_dir, stats_file=stats_file)

    expect_true(file.exists(stats_file))
    expect_true(file.exists(output_dir))
    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 4)

    # this should not generate any traces, but it should update the stats_file
    file.remove(stats_file)

    export_traces(list(trace_2, trace_3), file="f", output_dir=NULL, stats_file=stats_file)
    expect_true(file.exists(stats_file))
    stats <- read_stats_file(stats_file)
    expect_equal(stats$trace, c(NA, NA))
})
