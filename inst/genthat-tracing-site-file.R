options(error = function() {
    traceback(3)
    quit(status = 1, save = "no")
})

options(genthat.debug=as.logical(Sys.getenv("GENTHAT_DEBUG", "FALSE")))
options(genthat.log_file=Sys.getenv("GENTHAT_LOG_FILE"))

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
        if (Sys.getenv("GENTHAT_ACTION") == "export") {
            genthat::export_traces(
                traces=genthat::copy_traces(genthat::get_tracer()),
                file=Sys.getenv("GENTHAT_CURRENT_FILE"),
                output_dir=Sys.getenv("GENTHAT_OUTPUT_DIR"),
                stats_file=Sys.getenv("GENTHAT_STATS_FILE")
            )
        } else {
            stop("Unknown action: ", Sys.getenv("GENTHAT_ACTION"))
        }
    }
)
