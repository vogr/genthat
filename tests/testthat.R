
library(testthat)
library(devtools)
document(".") # have to generate exports
load_all(".", reset = TRUE, export_all = TRUE, quiet = TRUE)

res <- test_dir("./tests/testthat")

exit_code <- if (isTRUE(testthat:::all_passed(res))) 0 else 1

quit(save = "no", status = exit_code, runLast = FALSE)
