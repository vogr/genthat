library(dplyr)
library(stringr)
library(testthat)
library(genthat)
library(covr)

options(repos="https://mirrors.nic.cz/R")
options(genthat.debug=T)
options(genthat.source_paths="~/R/CRAN")

quiet <- TRUE

pkg_to_trace <- "data.table"
pkg_to_trace <- "adegenet"

rev_deps <- tools::package_dependencies(pkg_to_trace, which=c("Depends", "Imports", "LinkingTo"), reverse=TRUE, recursive=FALSE)
rev_deps <- rev_deps[[1]]

pkgs_installed <- installed.packages()[, 1]
pkgs_to_install <- setdiff(rev_deps, pkgs_installed)
install.packages(pkgs_to_install, INSTALL_opts = c("--example", "--install-tests", "--with-keep.source", "--no-multiarch"))
pkgs_installed <- installed.packages()[, 1]
rev_deps <- intersect(rev_deps, pkgs_installed)
rev_deps <- sample(rev_deps, length(rev_deps))

total_coverage <- data_frame(package=character(), filename=character(), functions=character(), line=integer(), value=numeric())
total_stats <- data_frame(
    package=character(),
    time=numeric(),
    all=numeric(),
    generated=numeric(),
    ran=numeric(),
    kept=numeric(),
    coverage=numeric()
)

for (dep in rev_deps) {
    genthat:::log_debug("Running ", dep)

    time <- genthat:::stopwatch(
        r <- gen_from_package(pkg_to_trace, dep, types="all", action="generate", prune_tests=TRUE, quiet=quiet)
    )

    saveRDS(r, paste0(pkg_to_trace, "-", dep, ".RDS"))

    time <- as.numeric(time, units="secs")
    stats <- attr(r, "stats")
    if (!is.null(stats)) {
        total_stats <- dplyr::bind_rows(
            total_stats,
            data_frame(
                package=dep,
                time=time,
                all=stats["all"],
                generated=stats["generated"],
                ran=stats["ran"],
                kept=stats["kept"],
                coverage=stats["coverage"]
            )
        )

        saveRDS(r, paste0(pkg_to_trace, "-stats.RDS"))
    }

    pkg_coverage <- attr(r, "raw_coverage")
    if (!is.null(pkg_coverage)) {
        pkg_coverage$package <- dep
        pkg_coverage <- as_data_frame(pkg_coverage)
        total_coverage <- dplyr::bind_rows(total_coverage, pkg_coverage)

        saveRDS(r, paste0(pkg_to_trace, "-coverage.RDS"))
    }

    coverage <- if (nrow(total_coverage) == 0) {
        0
    } else {
        df <- group_by(total_coverage, filename, functions, line) %>% summarise(value=sum(value))
        df <- summarise(ungroup(df), coverage=sum(value>0)/length(value)*100)
        df$coverage
    }

    print(stats)
    genthat:::log_debug("Finished ", dep, " in ", time, " secs, coverage so far ", coverage)
}
