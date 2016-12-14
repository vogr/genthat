
kPkgPrefix  <- "pkg: "
kSymbPrefix <- "symb: "
kValSPrefix <- "vsym: "
kFuncPrefix <- "func: "
kArgsPrefix <- "argv: "
kRetvPrefix <- "retv: "

blacklist <- c(".GlobalEnv", ".Internal", ".Primitive", "substitute",
               ".Machine", "on.exit",
               "withCallingHandlers", "quote",
               "c", "NextMethod", "UseMethod", "standardGeneric", "identity","missing",
               "sys.call", "withVisible", "findRestarts", "local", "withRestarts", "formals",
               ".C", ".Call", ".External", ".External.graphics", ".External2", ".Fortran", ".Call.graphics",
               "length", "as.environment",
               "length<-", "call", "switch", "nargs", "as.numeric", "library.dynam.unload",
               "suppressMessages",
               # errors with trace
               "match.call", ".doTrace", "tracingState", "traceback", "trace", "get0",
               "forceAndCall", # added in R.3.2.1
               "library"
)

sys <- c("system.time", "system.file", "sys.status",
         "sys.source", "sys.save.image", "sys.parents",
         "sys.parent", "sys.on.exit", "sys.nframe",
         "sys.load.image", "sys.function", "sys.frames",
         "sys.frame", "sys.calls", "sys.call", "R_system_version", ".First.sys")
env <- c("environment", "environment<-", "parent.frame", "parent.env", "parent.env<-")
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(", ":", "%sep%", "[", "[[", "$", "@", "=", "[<-",
               "[[<-", "$<-", "@<-", "+", "-", "*", "/",
               "^", "%%", "%*%", "%/%", "<", "<=", "==",
               "!=", ">=", ">", "|", "||", "&", "!", "~",
               "<-", "$", "<<-", "&&", "||" ,"{", "(")

primitive_generics_fails <- c(.S3PrimitiveGenerics, "round", "min", "max", "expression", "attr")
