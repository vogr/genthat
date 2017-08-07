#!/usr/bin/env Rscript

library(foreach)
library(doParallel)
library(ggplot2)
library(digest)
library(dplyr)
library(devtools)
library(pbapply)
library(stringr)
library(knitr)
library(readr)

do_stats <- function(dir) {
    package <- basename(dir)
    rdss <- list.files(dir, pattern = "\\.RDS$", recursive = TRUE, full.names = TRUE)

    if (length(rdss) == 0) {
        message("No traces for ", package)
        return(data_frame(package, n_traces=0, n_complete_traces=0, n_unique_traces=0))
    }

    message("Loading ", package, " traces ...")
    traces <- pblapply(rdss, readRDS)
    all_traces <- unlist(traces, recursive = FALSE)
    n_traces <- length(all_traces)

    complete_traces <- Filter(function(x) class(x) == "genthat_trace", all_traces)
    n_complete_traces <- length(complete_traces)

    if (length(complete_traces) == 0) {
        message("No complete traces for ", package)
        return(data_frame(package, n_traces, n_complete_traces=0, n_unique_traces=0))
    }

    message("Loading ", package, " digests ...")
    dfs <- pblapply(complete_traces, function(x) data_frame(fun=str_c(x$pkg, ":::", x$fun), digest=digest(x, algo ="sha1")))

    df <- bind_rows(dfs)

    ddf <- df %>% count(digest)
    n_unique_traces <- ddf %>% nrow()

    #ddf %>% count(n) %>% print(n=20)

    data_frame(package, n_traces, n_complete_traces, n_unique_traces)
}

trace_dirs <- list.dirs("~/genthat-package-tracing/runs/cran-top-100/tracing", recursive = FALSE, full.names = TRUE)
trace_dirs <- list.dirs("~/genthat-package-tracing/runs/cran-top-100/tracing/dplyr", recursive = FALSE, full.names = TRUE)
trace_dirs <- list.dirs("~/genthat-package-tracing/runs/cran-top-100-2/tracing", recursive = FALSE, full.names = TRUE)
#trace_dirs <- sample(trace_dirs, size = 2)

registerDoParallel(cores=8)
print(getDoParWorkers())

stats <- foreach(dir=trace_dirs, .combine=bind_rows, .verbose=TRUE) %dopar% {
    time <- system.time(s <- do_stats(dir))
    elapsed <- time["elapsed"]
    s <- cbind(s, elapsed=elapsed)

    s
}

stats <- stats %>% mutate(
    complete=ifelse(n_traces > 0, n_complete_traces/n_traces, 0),
    unique=ifelse(n_traces > 0, n_unique_traces/n_traces, 0)
)

stats %>% write_csv("traces-stats.csv")
stats %>% knitr::kable() %>% print()
stats %>% summary() %>% print()
