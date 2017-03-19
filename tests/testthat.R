
library(testthat)
library(devtools)
load_all(".", export_all = TRUE, quiet = TRUE)

res <- test_dir("./tests/testthat")

exit_code <- if (isTRUE(testthat:::all_passed(res))) 0 else 1

quit(save = "no", status = exit_code, runLast = FALSE)
