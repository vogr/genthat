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
    # if this does not fail it means that in globalenv there is a variable `a`
    # in this case this test will fail
    expect_error(get("a", envir=globalenv()))

    tracer <- create_set_tracer()
    set_tracer(tracer)

    d <- decorate_with_on.exit(dplyr:::arrange.data.frame, "arrange.data.frame", "dplyr", record_fun=quote(genthat:::record_trace))

    expect_true(na_last(d(df2, a)$a))

    traces <- copy_traces(tracer)
    trace <- filter(traces, function(x) x$fun == "arrange.data.frame")

    # we are trying to assess that we can capture `df2` but not `a` which is
    # correct in this case since `a` comes from `df2`
    expect_length(trace, 1)
    expect_equal(trace[[1]]$globals$df2, df2)
    expect_null(trace[[1]]$globals$a)
})

test_that("full tracing scenario with a seed", {
    set.seed(42)

    with_test_pkgs({
        tmp <- tempfile()
        on.exit(unlink(tmp, recursive=TRUE))

        tracer <- create_set_tracer()
        set_tracer(tracer)

        # TODO: fix decorating functions from packages (cf. #114)
        my_add <- samplepkg::my_add
        environment(my_add) <- environment(samplepkg::my_add)
        decorate_functions(my_add)

        my_add(runif(10), 1)

        traces <- copy_traces()
        expect_equal(length(traces), 1)
        expect_equal(length(traces[[1]]$retv), 10)

        test <- generate_test_file(traces[[1]], tmp)
        res <- run_generated_test(test, quiet=F)
        expect_true(res > 0)
    })
})

