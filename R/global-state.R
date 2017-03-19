
cache <- new.env()
cache$capture_file_size <- 50 * 1000 * 1000
cache$capture_arguments <- TRUE
cache$capture_dir = NULL
cache$capture_num <- 0
cache$output_dir <- NA
cache$generated_tests <- 0L
cache$retv_mismatch_count <- 0L
cache$unparsable_count <- 0L
cache$arguments <- list()
cache$decorated_functions <- list()
cache$call_id_counter <- as.environment(list(value = 0))

