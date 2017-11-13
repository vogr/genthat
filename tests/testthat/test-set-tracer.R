context("set tracer")

test_that("create_set_trace create empty tracer", {
    tracer <- create_set_tracer()

    traces <- copy_traces(tracer)
    expect_equal(length(traces), 0)
})

test_that("stores a trace", {
    tracer <- create_set_tracer()
    trace <- create_trace("a", args=list(1,2), retv=3)

    store_trace(tracer, trace)

    traces <- copy_traces(tracer)
    expect_equal(length(traces), 1)
    expect_equal(traces[[1]], trace)
})

test_that("same trace is stored only one", {
    tracer <- create_set_tracer()
    trace <- create_trace("a", args=list(1,2), retv=3)

    store_trace(tracer, trace)
    store_trace(tracer, trace)

    traces <- copy_traces(tracer)
    expect_equal(length(traces), 1)
    expect_equal(traces[[1]], trace)
})

test_that("different traces are stored", {
    tracer <- create_set_tracer()
    trace1 <- create_trace("a", args=list(1,2), retv=3)
    trace2 <- create_trace("a", args=list(1,2), retv=4)

    store_trace(tracer, trace1)
    store_trace(tracer, trace2)

    expect_equal(length(copy_traces(tracer)), 2)
})

test_that("reset trace clears traces", {
    tracer <- create_set_tracer()

    store_trace(tracer, create_trace("a"))
    store_trace(tracer, create_trace("b"))

    expect_equal(length(copy_traces(tracer)), 2)

    reset_traces(tracer)

    expect_equal(length(copy_traces(tracer)), 0)
})

test_that("set tracer works with session files", {
    trace1 <- create_trace("a", args=list(1,2), retv=3)
    trace2 <- create_trace("b", args=list(1,2), retv=4)
    trace3 <- create_trace("c", args=list(1,2), retv=4)

    session_file <- tempfile()

    tracer <- create_set_tracer(session_file=session_file)

    store_trace(tracer, trace1)
    store_trace(tracer, trace2)

    expect_equal(length(copy_traces(tracer)), 2)

    reset_traces(tracer)

    tracer <- create_set_tracer(session_file=session_file)

    store_trace(tracer, trace1)
    store_trace(tracer, trace2)
    store_trace(tracer, trace3)

    expect_equal(copy_traces(tracer), list(trace3))
})
