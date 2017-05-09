context("test generation")

test_that("format_args", {
    on.exit(options(genthat.use_deparse=FALSE))

    options(genthat.use_deparse=TRUE)

    expect_equal(format_args(NULL), "")
    expect_equal(format_args(list(1)), "1")
    expect_equal(format_args(list(1, 2)), "1, 2")
    expect_equal(format_args(list(a=1)), "a=1")
    expect_equal(format_args(list(a=1, b=2)), "a=1, b=2")
    expect_equal(format_args(list(a=1, 2, 3)), "a=1, 2, 3")
    expect_equal(format_args(list(a=1, 2, 3, c=4)), "a=1, 2, 3, c=4")
})

test_that("generate_test", {
    trace <- create_trace(fun="f", args=list("1"), retv="1")
    ## expect_equal(generate_test_code(trace), "test_that(\"f\", {\n\texpect_equal(f(\"1\"), \"1\")\n})")
})

test_that("gen_tests with no traces", {
    traces <- list(create_trace_error("f", list(), "error"))
    tmp_dir <- tempfile()
    files <- generate_tests(traces, tmp_dir)

    expect_equal(length(files), 0)
    expect_false(dir.exists(tmp_dir))
})

test_that("generate_tests", {
    tmp_dir <- tempfile()
    on.exit(unlink(tmp_dir, recursive=TRUE))

    traces <- list(create_trace(fun="f", args=list("1"), retv="2"), create_trace(fun="g", args=list("3"), retv="4"))

    files <- generate_tests(traces, tmp_dir)

    expect_equal(files, file.path(tmp_dir, c("test-1.R", "test-2.R")))
    expect_equal(file.exists(files), c(TRUE, TRUE))
    ## expect_equal(
    ##     paste(readLines(files[1]), collapse="\n"),
    ##     "test_that(\"f\", {\n\texpect_equal(f(\"1\"), \"2\")\n})")
    ## expect_equal(
    ##     paste(readLines(files[2]), collapse="\n"),
    ##     "test_that(\"g\", {\n\texpect_equal(g(\"3\"), \"4\")\n})")
})

test_that("format_value.genthat_closure works", {
    cls <- structure(list(args=list(a="1", b="2"), body=quote(paste0(a,b,c)), globals=list(c="3")), class="genthat_closure")
    expect_equal(eval(parse(text=paste0(format_value(cls), "()"))), "123")
})

test_that("format_value.genthat_closure works 2", {
    cls_d <- structure(list(args=list(), body=quote(paste0("4", c)), globals=list(c="5")), class="genthat_closure")
    cls <- structure(list(args=list(a="1", b="2"), body=quote(paste0(a,b,c,d())), globals=list(c="3",d=cls_d)), class="genthat_closure")

    expect_equal(eval(parse(text=paste0(format_value(cls), "()"))), "12345")
})

test_that("format_closure can generate proper closure", {
    on.exit({ reset_replacements(); reset_call_traces(); options(genthat.use_deparse=FALSE) })

    options(genthat.use_deparse=TRUE)


    f <- function(x, y) g(x + y)

    trace <- structure(
        list(
            fun = "f",
            args = structure(
                list(
                    x = quote(b + 2),
                    y = quote(a)
                )
            ), globals = structure(
                list(
                    a = 1,
                    b = 2,
                    g = structure(
                        list(
                            args = alist(y = ),
                            body = quote(a + y + c),
                            globals = list(c = 3)
                        ), class = "genthat_closure"
                    )
                )
            ),
            retv = 9
        ), class = "genthat_trace")

    ## cat("\n")
    ## cat(generate_test_code(trace))


    eval(parse(text=generate_test_code(trace)))
})


test_that("", {
    on.exit({ reset_replacements(); reset_call_traces(); options(genthat.use_deparse=FALSE) })
    options(genthat.use_deparse=TRUE)

    a <- 1
    b <- 2
    c <- 3

    g <- function(y) a + y + c
    f <- function(x, y) {
        g(x + y)
    }

    ## browser()
    decorate_functions(f)

    f(b+2, a)

    tests <- lapply(get_call_traces(), generate_test_code)
    ## lapply(tests, cat)
})
