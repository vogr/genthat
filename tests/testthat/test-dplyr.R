context("dplyr")

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

test_that("dplyr arrange - onexit", {
    tracer <- create_set_tracer()
    set_tracer(tracer)

    d <- decorate_with_onexit(dplyr:::arrange.data.frame, "arrange", "dplyr", record_fun=quote(genthat:::record_trace))

    expect_true(na_last(d(df2, a)$a))

    traces <- copy_traces(tracer)
    expect_equal(traces[[1]]$globals$df2, df2)
})

test_that("dplyr arrange - onentry", {
    tracer <- create_set_tracer()
    set_tracer(tracer)

    d <- decorate_with_onentry(dplyr:::arrange.data.frame, "arrange", "dplyr", record_fun=quote(genthat:::record_trace))

    expect_true(na_last(d(df2, a)$a))

    traces <- copy_traces(tracer)
    expect_equal(traces[[1]]$globals$df2, df2)
})

test_that("dplyr arrange - both", {
    tracer <- create_set_tracer()
    set_tracer(tracer)

    d <- decorate_with_onboth(dplyr:::arrange.data.frame, "arrange", "dplyr", record_fun=quote(genthat:::record_trace))

    expect_true(na_last(d(df2, a)$a))

    traces <- copy_traces(tracer)
    expect_equal(traces[[1]]$globals$df2, df2)
})
