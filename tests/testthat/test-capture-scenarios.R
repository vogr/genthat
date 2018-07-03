context("capture scenarios")

df2 <- data.frame(
  a = rep(c(NA, 1, 2, 3), each = 4),
  b = rep(c(0L, NA, 1L, 2L), 4),
  c = c(NA, NA, NA, NA, letters[10:21]),
  d = rep(c(T, NA, F, T), each = 4),
  id = 1:16,
  stringsAsFactors = FALSE
)

na_last <- function(x) {
    n <- length(x)
    all(is.na(x[(n - 3):n]))
}

test_that("dplyr arrange.data.frame (from dplyr/tests/testthat/test-arrange.r)", {
    on.exit({
        reset_functions()
        reset_traces()
    })

    # if this does not fail it means that in globalenv there is a variable `a`
    # in this case this test will fail
    expect_error(get("a", envir=globalenv()))

    tracer <- create_set_tracer()
    set_tracer(tracer)

    d <- create_decorated_function(
        fun=dplyr:::arrange.data.frame,
        name="arrange.data.frame",
        package="dplyr",
        onexit=record_trace
    )

    expect_true(na_last(d(df2, a)$a))

    traces <- copy_traces(tracer)
    trace <- filter(traces, function(x) x$fun == "arrange.data.frame")

    # we are trying to assess that we can capture `df2` but not `a` which is
    # correct in this case since `a` comes from `df2`
    expect_length(trace, 1)
    expect_equal(trace[[1]]$globals$df2, df2)
    expect_null(trace[[1]]$globals$a)
})

test_that("replacement function", {
    on.exit({
        reset_functions()
        reset_traces()
    })

    with_test_pkgs({
        x <- 1:5
        samplepkg::gg(x, 4) <- 0
        expect_equal(x, c(0, 0, 0, 4, 5))

        tracer <- create_set_tracer()
        set_tracer(tracer)

        y <- 1:5
        decorate_function(samplepkg::`gg<-`, onexit=record_trace)
        samplepkg::gg(y, 4) <- 0
        expect_equal(y, c(0, 0, 0, 4, 5))

        t <- copy_traces(tracer)[[1]]

        tmp <- tempfile()
        on.exit(unlink(tmp, recursive=TRUE), add=TRUE)

        test <- generate_test_file(t, tmp)
        res <- run_generated_test(test)
        expect_true(res > 0)
    })
})

test_that("full tracing scenario with a seed", {
    on.exit({
        reset_functions()
        reset_traces()
    })

    set.seed(42)

    with_test_pkgs({
        tmp <- tempfile()
        on.exit(unlink(tmp, recursive=TRUE))

        tracer <- create_set_tracer()
        set_tracer(tracer)
        on.exit(reset_traces())

        decorate_function(samplepkg::my_add, onexit=record_trace)

        samplepkg::my_add(runif(10), 1)

        traces <- copy_traces()
        expect_equal(length(traces), 1)
        expect_equal(length(traces[[1]]$retv), 10)

        test <- generate_test_file(traces[[1]], tmp)
        res <- run_generated_test(test)
        expect_true(res > 0)
    })
})

