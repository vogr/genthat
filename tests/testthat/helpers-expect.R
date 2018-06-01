expect_nrow <- function(df, expected) {
    testthat::expect_equal(nrow(df), expected)
}
