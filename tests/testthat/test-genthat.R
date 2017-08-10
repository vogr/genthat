context("genthat tests")

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
})

test_that("gen_from_package work", {
    withr::with_temp_libpaths({
        devtools::install_local("samplepkg", quiet=TRUE, build_vignettes=TRUE)

        output_dir <- tempfile()
        ret <- gen_from_package("samplepkg", output_dir=output_dir, quiet=TRUE)

        expect_equal(ret$run$examples, c("My-add.Rd.R"=0, "My-call.Rd.R"=0))
        expect_equal(ret$run$tests, c("testthat.R"=0))
        expect_equal(ret$run$vignettes, c("my-vignette.R"=0))

        tags <- c("My-add.Rd", "My-call.Rd", "testthat", "my-vignette")
        expect_equal(ret$traces$tag, tags)
        expect_equal(ret$traces$filename, file.path(output_dir, paste0(tags, "-1.RDS")))
        # number of traces in the individual files - cf. above
        expect_equal(ret$traces$n_traces, c(2, 4, 6, 1))
    })
})

