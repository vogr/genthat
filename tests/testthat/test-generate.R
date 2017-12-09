context("test generation")

test_that("format_calling_args", {
    on.exit(options(genthat.use_deparse=FALSE))

    options(genthat.use_deparse=TRUE)

    format_args <- function(x) paste(format_calling_args(x), collapse=", ")

    expect_equal(format_args(NULL), "")
    expect_equal(format_args(list(1)), "1")
    expect_equal(format_args(list(1, 2)), "1, 2")
    expect_equal(format_args(list(a=1)), "a=1")
    expect_equal(format_args(list(a=1, b=2)), "a=1, b=2")
    expect_equal(format_args(list(a=1, 2, 3)), "a=1, 2, 3")
    expect_equal(format_args(list(a=1, 2, 3, c=4)), "a=1, 2, 3, c=4")
})

test_that("generate_test", {
    trace <- create_trace(fun="f", pkg=NULL, args=list("1"), retv="1")
    ## expect_equal(generate_test_code(trace), "test_that(\"f\", {\n\texpect_equal(f(\"1\"), \"1\")\n})")
})

test_that("generate_call supports infix functions for base package", {
    trace <- create_trace("%in%", pkg="base", args=list(x=1, table=c(1, 2)), retv=TRUE)
    expect_equal(generate_call(trace), "1 %in% c(1, 2)")
})

test_that("generate_call supports infix functions for others", {
    trace <- create_trace("%in%", pkg="mypkg", args=list(x=1, table=c(1, 2)), retv=TRUE)
    expect_equal(generate_call(trace), "mypkg:::`%in%`(x=1, table=c(1, 2))")
})

test_that("generate_tests", {
    traces <- list(
        create_trace(fun="f1", pkg="p", args=list(x=1, y=2), retv=3),
        create_trace(fun="f2", pkg="p", args=list(x=1, y=2)),
        create_trace(fun="f3", pkg="p", args=list(x=1, y=2), error=simpleError("An error")),
        create_trace(fun="f4", pkg="p", args=list(x=1, y=2), failure=simpleError("A failure"))
    )

    tests <- generate_tests(traces)

    expect_equal(nrow(tests), 4)
    expect_equal(tests$fun, c("f1", "f2", "f3", "f4"))
    expect_equal(tests$pkg, rep("p", 4))
    expect_equal(tests$gen_error, c(NA, "Generate error: No return value", "Code error: An error", "Trace error: A failure"))
    expect_false(is.na(tests$elapsed[1]))
    expect_true(all(is.na(tests$elapsed[2:4])))
})

test_that("save_tests save each group", {
    output_dir <- tempfile()
    on.exit(unlink(output_dir, recursive=T))

    tests <- dplyr::tribble(
        ~trace, ~fun, ~pkg, ~code, ~error, ~elapsed,
        NA, "f1", "p1", "code_p1_f1_1", NA, 0,
        NA, "f1", "p1", "code_p1_f1_2", NA, 0,
        NA, "f2", "p1", "code_p1_f2_1", NA, 0,
        NA, "g1", "p2", "code_p2_g1_1", NA, 0,
        NA, "g2", "p2", NA, NA, 0
    )

    saves <- save_tests(tests, output_dir)
    files <- saves$test_file

    expect_equal(files, c(
        file.path(
            output_dir,
            c(
                "p1/f1/test-0.R",
                "p1/f1/test-1.R",
                "p1/f2/test-0.R",
                "p2/g1/test-0.R"
            )
        ),
        NA)
    )

    files <- saves %>% dplyr::filter(!is.na(test_file)) %>% .$test_file
    expect_equal(sapply(files, readLines, USE.NAMES=FALSE), c(
        "code_p1_f1_1",
        "code_p1_f1_2",
        "code_p1_f2_1",
        "code_p2_g1_1"
        ))
})

test_that("generate escapes global non-syntactic names", {
    expect_equal(
        generate_globals(list(`%>%`="A", `__b`="B", c="C")),
        paste(c('`%>%` <- "A"', '`__b` <- "B"', 'c <- "C"'), collapse="\n")
    )
})

## test_that("gen_tests with no traces", {
##     traces <- list(create_trace_error("f", list(), "error"))
##     tmp_dir <- tempfile()
##     files <- generate_tests(traces, tmp_dir)

##     expect_equal(length(files), 0)
##     expect_false(dir.exists(tmp_dir))
## })

## test_that("generate_tests", {
##     tmp_dir <- tempfile()
##     on.exit(unlink(tmp_dir, recursive=TRUE))

##     traces <- list(create_trace(fun="f", args=list("1"), retv="2"), create_trace(fun="g", args=list("3"), retv="4"))

##     files <- generate_tests(traces, tmp_dir)

##     expect_equal(files, file.path(tmp_dir, c("test-1.R", "test-2.R")))
##     expect_equal(file.exists(files), c(TRUE, TRUE))
##     ## expect_equal(
##     ##     paste(readLines(files[1]), collapse="\n"),
##     ##     "test_that(\"f\", {\n\texpect_equal(f(\"1\"), \"2\")\n})")
##     ## expect_equal(
##     ##     paste(readLines(files[2]), collapse="\n"),
##     ##     "test_that(\"g\", {\n\texpect_equal(g(\"3\"), \"4\")\n})")
## })

