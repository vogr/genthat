library(dplyr)
library(testthat)
library(genthat)

package <- "BH"

tracing <- gen_from_package(
    package, types="examples", decorator="on.exit", tracer="set", output_dir="tmp", action="generate"
)

n_traces <- sum(tracing$n_complete)
message("We have ", n_traces, " traces to work with")
expect_true(tracing %>% filter(n_traces - n_complete - n_failures - n_error - n_entry != 0) %>% nrow() == 0)

# load traces
traces <- import_traces(filter(tracing, n_complete > 0)$filename)
message("Loaded ", length(traces), " traces from RDS")
expect_equal(n_traces, length(traces))

# generate tests
tests <- generate_tests(traces)
expect_equal(nrow(tests), n_traces)
n_tests <- tests %>% filter(is.na(gen_error)) %>% nrow()
message("Generated ", n_tests, " tests")

# show the error
tests %>% count(gen_error) %>% print()

# save tests
saved_tests <- save_tests(tests, output_dir="tmp")
n_saved_tests <- saved_tests %>% filter(!is.na(test_file)) %>% nrow()
expect_equal(n_saved_tests, n_tests)
message("Saved ", n_saved_tests, " tests")

# show the error
saved_tests %>% count(save_error) %>% print()

# run tests
ran_tests <- run_generated_tests(saved_tests$test_file)
n_ran_tests <- ran_tests %>% filter(!is.na(nb)) %>% nrow()
expect_equal(n_ran_tests, n_tests)

# show the errors
ran_tests %>% count(run_error) %>% print()
ran_tests %>% filter(failed == 1) %>% print()
ran_tests %>% filter(error == TRUE) %>% print()

n_success_tests <- ran_tests %>% filter(!is.na(nb), nb == 1, failed == 0) %>% nrow()
message("We have successfully ran ", n_success_tests, " = ", n_success_tests / n_tests * 100, "%")
