library(testthat)

for (pkg in c("devtools", "withr")) {
    if (!requireNamespace(pkg, quietly=TRUE)) {
        stop(pkg, " is needed for this function to work. Please install it.", call. = FALSE)
    }
}

test_check("genthat")
