options(error = function() {
    traceback(3)
    quit(status = 1, save = "no")
})

options(genthat.debug=as.logical(Sys.getenv("GENTHAT_DEBUG", "FALSE")))
options(genthat.max_trace_size=as.integer(Sys.getenv("GENTHAT_MAX_TRACE_SIZE")))

genthat::set_decorator(genthat::create_decorator(Sys.getenv("GENTHAT_DECORATOR")))

if (Sys.getenv("GENTHAT_TRACER") == "set") {
    genthat::set_tracer(
        genthat::create_tracer(
            "set",
            session_file=Sys.getenv("GENTHAT_SESSION_FILE")
        )
    )
} else {
    genthat::set_tracer(genthat::create_tracer(Sys.getenv("GENTHAT_TRACER")))
}

for (pkg in  strsplit(Sys.getenv("GENTHAT_PKGS"), ",", fixed=TRUE)[[1]]) {
    genthat::decorate_environment(pkg)
}

library(methods)

reg.finalizer(
    e=loadNamespace("genthat"),
    onexit=TRUE,
    f=function(x) {
        ret <- genthat::process_traces(
            traces=genthat::copy_traces(genthat::get_tracer()),
            output_dir=Sys.getenv("GENTHAT_OUTPUT_DIR"),
            action=Sys.getenv("GENTHAT_ACTION")
        )

        stats_file <- Sys.getenv("GENTHAT_STATS_FILE")

        # this is important since some tracing might spawn extra instances
        # of R in which case we want to keep all the traces together
        stats_file_exists <- file.exists(stats_file)

        write.table(
            ret,
            file=stats_file,
            row.names=FALSE,
            col.names=!stats_file_exists,
            append=stats_file_exists,
            qmethod="double",
            sep=","
        )
    }
)
