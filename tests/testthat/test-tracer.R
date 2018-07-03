context("tracer")

test_that("create tracer creates trace based on the option", {
    on.exit(options(genthat.tracer_type="set"))

    t <- create_tracer()
    expect_is(t, "set_tracer")

    options(genthat.tracer_type="sequence")

    t <- create_tracer()
    expect_is(t, "sequence_tracer")
})

test_that("set_tracer sets a tracer", {
    on.exit(.genthat$tracer <- NULL)

    .genthat$tracer <- NULL

    tracer <- create_set_tracer()
    set_tracer(tracer)

    expect_equal(.genthat$tracer, tracer)
})

test_that("get_tracer get the current tracer", {
    on.exit(.genthat$tracer <- NULL)

    tracer <- create_set_tracer()
    .genthat$tracer <- tracer

    expect_equal(get_tracer(), tracer)
})

test_that("get_tracer creates a tracer if non exists", {
    on.exit(.genthat$tracer <- NULL)

    .genthat$tracer <- NULL

    expect_is(get_tracer(), "set_tracer")
})

test_that("copy_traces returns stored traces, reset_traces removes all traces", {
    on.exit(.genthat$tracer <- NULL)

    tracer <- create_set_tracer()
    set_tracer(tracer)

    t1 <- create_trace(fun="f")
    store_trace(tracer, t1)

    expect_equal(copy_traces(), list(t1))

    reset_traces()

    expect_equal(copy_traces(), list())
})
