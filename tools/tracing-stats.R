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

    trace_classes <- sapply(all_traces, function(x) {
      switch(class(x),
        genthat_trace=1,
        genthat_trace_error=2,
        genthat_trace_entry=3)
    })

    n_complete_traces <- length(trace_classes[trace_classes == 1])
    n_error_traces <- length(trace_classes[trace_classes == 2])
    n_entry_traces <- length(trace_classes[trace_classes == 3])

    message("Computing ", package, " digests ...")
    dfs <- pblapply(all_traces, function(x) data_frame(fun=str_c(x$pkg, ":::", x$fun), digest=digest(x, algo="sha1"), clazz=class(x)))

    df <- bind_rows(dfs)

    n_unique_traces <- df %>% distinct(digest) %>% nrow()

    data_frame(package, n_traces, n_complete_traces, n_error_traces, n_entry_traces, n_unique_traces)
}

#trace_dirs <- list.dirs("~/genthat-package-tracing/runs/cran-top-100/tracing", recursive = FALSE, full.names = TRUE)
#trace_dirs <- list.dirs("~/genthat-package-tracing/runs/cran-top-100-2/tracing/stringr", recursive = FALSE, full.names = TRUE)
#trace_dirs <- list.dirs("~/genthat-package-tracing/runs/cran-top-100-2/tracing", recursive = FALSE, full.names = TRUE)

args <- commandArgs(trailingOnly=TRUE)
dir <- if (length(args) == 1) args[1] else getwd()
trace_dirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
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
