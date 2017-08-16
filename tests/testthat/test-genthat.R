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

    expect_false(file.exists(stats_file))
    expect_false(dir.exists(output_dir))

    trace_1 <- create_trace("fun1")
    trace_2 <- create_trace("fun2")
    trace_3 <- create_trace("fun3")

    export_traces(list(trace_1), output_dir=output_dir, stats_file=stats_file)

    expect_true(file.exists(stats_file))
    expect_true(file.exists(output_dir))

    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 1)
    expect_equal(stats$filename, file.path(output_dir, "1.RDS"))
    expect_equal(stats$n_traces, 1)
    expect_equal(stats$n_complete, 0)
    expect_equal(stats$n_error, 0)
    expect_equal(stats$n_entry, 1)
    expect_equal(stats$tag, NA)

    rds <- readRDS(file.path(output_dir, "1.RDS"))
    expect_length(rds, 1)
    expect_equal(rds[[1]], trace_1)

    ## this should append
    export_traces(list(trace_2, trace_3), output_dir=output_dir, stats_file=stats_file)

    expect_true(file.exists(stats_file))
    expect_true(file.exists(output_dir))

    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 2)
    expect_equal(stats$filename, file.path(output_dir, c("1.RDS", "2.RDS")))
    expect_equal(stats$n_traces, c(1, 2))
    expect_equal(stats$tag, c(NA, NA))

    rds <- readRDS(file.path(output_dir, "2.RDS"))
    expect_length(rds, 2)
    expect_true(list_contains(rds, trace_2))
    expect_true(list_contains(rds, trace_3))

    ## this should not append
    export_traces(list(), output_dir=output_dir, stats_file=stats_file)

    expect_true(file.exists(stats_file))
    expect_true(file.exists(output_dir))

    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 2)
    expect_equal(stats$filename, file.path(output_dir, c("1.RDS", "2.RDS")))
    expect_equal(stats$n_traces, c(1, 2))
    expect_equal(stats$tag, c(NA, NA))
})

test_that("export traces merges file names", {
    stats_file <- tempfile()
    output_dir <- tempfile()

    trace_1 <- create_trace("fun1")
    trace_2 <- create_trace("fun2")

    export_traces(list(trace_1, trace_2), output_dir=output_dir, stats_file=stats_file, batch_size=1)

    stats <- read_stats_file(stats_file)
    expect_equal(nrow(stats), 1)
    expect_equal(stats$filename, paste(file.path(output_dir, c("1.RDS", "2.RDS")), collapse="\n"))

    traces <- unlist(lapply(file.path(output_dir, c("1.RDS", "2.RDS")), readRDS), recursive=FALSE)
    expect_length(traces, 2)
    expect_true(list_contains(traces, trace_1))
    expect_true(list_contains(traces, trace_2))
})
