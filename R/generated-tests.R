
#' Accepts one string representing a call to \code{test_that()} and returns an appropriate data structure.
#'
#' @export
#' @param text String representation of test.
#'
#' @examples
#' parseGeneratedTest("\n\ttest_that('test-42', { expected <- 42; expect_equal(the_truth_function("anything"), expected) })\n\n")
parseGeneratedTest <- function(text) {
    ast <- parse(text = text)
    stopifnot(is.expression(ast))
    call2test_that <- ast[[1]]
    stopifnot(typeof(call2test_that) == "language")
    stopifnot(length(call2test_that) == 3)
    stopifnot(call2test_that[[1]] == "test_that")
    test_label <- call2test_that[[2]]
    stopifnot(typeof(test_label) == "character")
    test_expression <- call2test_that[[3]]
    stopifnot(typeof(test_expression) == "language")
    stopifnot(length(test_expression) == 3)
    stopifnot(test_expression[[1]] == "{")
    expected_assignment <- test_expression[[2]]
    call2expect_equal <- test_expression[[3]]

    stopifnot(typeof(expected_assignment) == "language")
    stopifnot(length(expected_assignment) == 3)
    stopifnot(expected_assignment[[1]] == "<-")
    stopifnot(expected_assignment[[2]] == "expected")
    expected_val <- expected_assignment[[3]]

    stopifnot(typeof(call2expect_equal) == "language")
    stopifnot(length(call2expect_equal) == 3)
    stopifnot(call2expect_equal[[1]] == "expect_equal")
    stopifnot(call2expect_equal[[3]] == "expected")
    call2tested_fn <- call2expect_equal[[2]]

    stopifnot(typeof(call2tested_fn) == "language")
    stopifnot(length(call2tested_fn) >= 1)
    tested_fn <- as.character(call2tested_fn[[1]])
    args <- as.list(call2tested_fn[-1])

    structure(
        list(
            expected.value = expected_val,
            tested.function = tested_fn,
            arguments = args
        ),
        class = "genthat_test_case"
    )
}

loadTestFile <- function(path) {
    ast <- parse(path)
    stopifnot(is.expression(ast))

    context <- NULL
    libs <- c()
    testCases <- list()

    for (i in seq(length(ast))) {
        current_call <- ast[[i]]
        stopifnot(typeof(current_call) == "language")
        if (current_call[[1]] == "context") {
            stopifnot(length(current_call) == 2)
            context <- current_call[[2]]
        } else if (current_call[[1]] == "library") {
            stopifnot(length(current_call) == 2)
            libs <- c(libs, current_call[[2]])
        } else if (current_call[[1]] == "test_that") {
            stopifnot(length(current_call) >= 1)
            testCases[[length(testCases)+1]] <- parseGeneratedTest(deparse(current_call))
        } else {
            stop("Unexpected statement in testfile!")
        }
    }

    list(
        libs = libs,
        context = context,
        testCases = testCases
    )
}

