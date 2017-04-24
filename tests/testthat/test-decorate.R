context("decoration")

test_that("create_function creates functions", {
    f <- create_function(pairlist(a=1, b=2), substitute(a+b))
    expect_equal(f(), 3)
})

test_that("create_function assigns the right environment", {
    e <- new.env()
    e$var <- 1
    
    f <- create_function(pairlist(a=1, b=2), substitute(a+b+var), e)
    expect_equal(f(), 4)
    expect_equal(environment(f), e)
})

test_that("create_function assigns attributes", {
    attrs <- list(x=TRUE, y=TRUE)
    f <- create_function(pairlist(a=1, b=2), substitute(a+b), attributes=attrs)

    expect_equal(f(1, 2), 3)
    expect_equal(attributes(f), attrs)
})

test_that("decorate_function decorates a function", {
    on.exit(reset_call_traces())
    
    entry <- new.env()
    exit <- new.env()

    f <- function(a,b) { a + b }
    
    d <-
        decorate_function(
            "f",
            f,
            .entry=save_calling_args(entry),
            .exit=save_calling_args(exit))

    expect_equal(formals(d), formals(d))
    expect_equal(environment(d), environment(f))
    expect_equal(attributes(d), list(genthat=T))
    
    d(1, 2)

    expect_equal(length(entry), 1)
    expect_equal(entry$c1$call_id, 0)
    expect_equal(entry$c1$name, "f")
    expect_equal(entry$c1$args, list(a=1, b=2))

    expect_equal(length(exit), 1)
    expect_equal(exit$c1$call_id, 0)
    expect_equal(exit$c1$retv, 3)
})


test_that("decorate_function decorates a package function", {    
    entry <- new.env()
    exit <- new.env()
        
    d <-
        decorate_function(
            "file_path_sans_ext",
            tools::file_path_sans_ext,
            .entry=save_calling_args(entry),
            .exit=save_calling_args(exit))

    expect_equal(formals(d), formals(tools::file_path_sans_ext))
    expect_equal(environment(d), environment(tools::file_path_sans_ext))
    
    d("a.b")

    expect_equal(length(entry), 1)
    expect_equal(entry$c1$call_id, 0)
    expect_equal(entry$c1$name, "file_path_sans_ext")
    expect_equal(entry$c1$args, list(x="a.b"))

    expect_equal(length(exit), 1)
    expect_equal(exit$c1$call_id, 0)
    expect_equal(exit$c1$retv, "a")
})

test_that("is_decorated knows when a functions is decorated", {
    on.exit(reset_replacements())

    f <- function() {}
    expect_false(is_decorated(f))

    d <- decorate_function("f", f)
    expect_true(is_decorated(d))
})

test_that("decorate_environment decorates all functions in the environment", {
    on.exit({
        # we do not want to test reset so we force reset the decoration
        detach(package:samplepkg)
        suppressWarnings(reset_replacements())
    })

    env <- devtools::load_all("samplepkg")$env
    original_size <- length(ls(env, all.names=TRUE))
    decorate_environment(env)
    size <- length(ls(env, all.names=TRUE))

    expect_equal(original_size, size)

    names <- ls(env, all.names=TRUE)
    funs <- lapply(names, get, envir=env)
    names(funs) <- names
    
    funs <- filter(funs, is.function)

    expect_true(all(sapply(funs, is_decorated)), TRUE)

    expect_equal(length(get_replacements()), length(funs))
    expect_equivalent(
        sort(sapply(get_replacements(), `[[`, "name")),
        sort(paste0("samplepkg:::",names(funs))))
})

#library(testthat)
#library(devtools)
#
#source("./utils.R")
#
## TODO: move into utils
#
## saves the values of parent function's arguments into an environment
## we want to separate it from the `save_calling_args_fun` to enable easy debugging
#save_calling_args <- function(env, names) {
#    args <- as.list(sys.call(-1))[-1]
#    e <- new.env(parent=parent.frame(2))
#    args <- lapply(args, function(x) eval(x, envir=e))
#    names(args) <- names
#
#    args$return_value <- returnValue()
#
#    l <- paste0("c", length(env) + 1)
#    assign(l, args, envir=env)
#}
#
## returns a function that will save parent function's arguments into an environment
#save_calling_args_fun <- function(env, ...) {
#    names <- c(...)
#    
#    function(...) {
#        save_calling_args(env, names)
#        TRUE
#    }
#}
#
#context("Function decoration")
#
#test_that('is_decorated FALSE cases.', {
#    fn <- function() {}
#
#    expect_false(is_decorated(function() {}))
#    expect_false(is_decorated(fn))
#
#    expect_true(is_decorated(decorate_function_val(fn, "f_label")))
#    expect_true(is_decorated(decorate_function_val(function() {}, "f_label")))
#})
#
#test_that('decorate_function_val__() functionality', {
#    fn <- function(a, b) { a + b + 1 }
#
#    enter <- new.env()
#    exit <- new.env()
#
#    decorated <-
#        genthat:::decorate_function_val__(
#                      fn,
#                      "label1",
#                      enter_function=save_calling_args_fun(enter, "fname", "args", "call_id"),
#                      exit_function=save_calling_args_fun(exit, "call_id"))
#
#    decorated(4, 3)
#
#    expect_equal(enter$c1$fname, "label1")
#    expect_equal(enter$c1$args, list(a=4, b=3))
#    expect_equal(enter$c1$call_id, 0)
#    
#    expect_equal(exit$c1$call_id, 0)
#    expect_equal(exit$c1$return_value, 8)
#})
#
#test_that('decorate_function_val__() evaluates its arguments in the correct environment', {
#    v <- 10
#    fn <- function(a, b) { a + b + v}
#
#    expect_equal(fn(1, 2), 13)
#    
#    enter <- new.env()
#    exit <- new.env()
#
#    decorated <-
#        genthat::decorate_function_val__(
#                     fn,
#                     "label1",
#                     enter_function=save_calling_args_fun(enter, "fname", "args", "call_id"),
#                     exit_function=save_calling_args_fun(exit, "call_id"))
#
#    decorated(1, 2)
#
#    expect_equal(enter$c1$args, list(a=1, b=2))
#    expect_equal(enter$c1$fname, "label1")
#    
#    expect_equal(exit$c1$return_value, 13)
#})
#
#test_that('decorate_function_val__() works with ...', {
#    fn <- function(a, ...) { sum(c(a, ...)) }
#    m <- matrix(1:6, ncol=3)
#
#    expect_equal(apply(m, 1, fn, a=10), c(19, 22))
#    
#    enter <- new.env()
#    exit <- new.env()
#
#    decorated <-
#        genthat::decorate_function_val__(
#                     fn,
#                     "label1",
#                     enter_function=save_calling_args_fun(enter, "fname", "args", "call_id"),
#                     exit_function=save_calling_args_fun(exit, "call_id"))
#
#    expect_equal(apply(m, 1, decorated, a=10), c(19, 22))
#
#    expect_equal(enter$c1$args, list(a=10, c(1, 3, 5)))
#    expect_equal(enter$c1$fname, "label1")
#    expect_equal(exit$c1$return_value, 19)
#
#    expect_equal(enter$c2$args, list(a=10, c(2, 4, 6)))
#    expect_equal(enter$c2$fname, "label1")
#    expect_equal(exit$c2$return_value, 22)
#})
#
#test_that("decoration for S3 functions", {
#    fn <- function(x) {
#        UseMethod("fn", x)
#    }
#
#    fn.foo <- function(x) {
#        x + 1
#    }
#
#    fn.bar <- function(x) {
#        x + 2
#    }
#
#    foo <- structure(1, class = "foo")
#    bar <- structure(2, class = "bar")
#
#    expect_equal(fn(foo), structure(2, class="foo"))
#    expect_equal(fn(bar), structure(4, class="bar"))
#    
#    enter <- new.env()
#    exit <- new.env()
#
#    decorated <-
#        genthat::decorate_function_val__(
#                     fn,
#                     "label1",
#                     enter_function=save_calling_args_fun(enter, "fname", "args", "call_id"),
#                     exit_function=save_calling_args_fun(exit, "call_id"))
#
#    expect_equal(decorated(foo), structure(2, class="foo"))
#    expect_equal(decorated(bar), structure(4, class="bar"))
#
#    expect_equal(enter$c1$args, list(x=structure(1, class="foo")))
#    expect_equal(enter$c1$fname, "label1")
#    expect_equal(exit$c1$return_value, structure(2, class="foo"))
#
#    expect_equal(enter$c2$args, list(x=structure(2, class="bar")))
#    expect_equal(enter$c2$fname, "label1")
#    expect_equal(exit$c2$return_value, structure(4, class="bar"))
#})
#
### TODO: test decorating just one of the S3 function
### TODO: test decorating all S3 functions
#
### TODO: can we decorate builtins?
### TODO: shall we prevent decorating specials?
#enterFunction_cpp <- function(name, args, call_id) { .Call("genthat_enterFunction_cpp", PACKAGE = "genthat", name, args, call_id) }
#exitFunction_cpp <- function(call_id, retv) { .Call("genthat_exitFunction_cpp", PACKAGE = "genthat", call_id, retv) }
#
#test_that("enterFunction_cpp positive", {
#    cid <- get_next_call_id()
#    res <- enterFunction_cpp("fn1", list(a = 42, b = "hey!"), cid)
#    expect_equal(res, 0)
#})
#
#test_that("enterFunction_cpp unserializable", {
#    cid <- get_next_call_id()
#    res <- enterFunction_cpp("fn1", list(x = function() {}), cid) # functions cannot be serialized
#    expect_type(res, "list")
#    expect_equal(res$type, "error")
#    expect_match(res$error_description, "^<unserializable:")
#})
#
#test_that("exitFunction_cpp positive", {
#    cid <- get_next_call_id()
#    enterFunction_cpp("fn1", list(a = 42L), cid)
#    res <- exitFunction_cpp(cid, 1337L)
#    expect_type(res, "list")
#    expect_equal(res$type, "trace")
#    expect_equal(res$func, "fn1")
#    expect_equal(res$args, "list(a=42L)")
#    expect_equal(res$retv, "1337L")
#})
#
#test_that("exitFunction_cpp unserializable", {
#    cid <- get_next_call_id()
#    enterFunction_cpp("fn1", list(a = 42), cid)
#    res <- exitFunction_cpp(cid, function() {}) # functions cannot be serialized
#    expect_type(res, "list")
#    expect_equal(res$type, "error")
#    expect_match(res$error_description, "^<unserializable:")
#})
#
#test_that("exitFunction_cpp non-initialized calls", {
#    cid <- get_next_call_id()
#    res <- exitFunction_cpp(cid, 42L) # functions cannot be serialized
#    expect_type(res, "list")
#    expect_equal(res$type, "error")
#    expect_match(res$error_description, "^<Terminated non-initialized call!>$")
#})
#
#test_that("decorate_function_val()", {
#    fn1 <- function() {}
#    fn2 <- decorate_function_val(fn1, "fn1_label")
#    expect_false(is_decorated(fn1))
#    expect_true(is_decorated(fn2))
#})
#
#test_that("decorate_function_env()", {
#    fn1 <- function() {}
#    decorate_function_env("fn1", env = environment())
#    expect_true(is_decorated(fn1))
#})
#
#test_that("decorate_exported()", {
#    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)
#
#    decorate_exported("examplePackage", c("my_add", "public_fn"))
#
#    expect_true(is_decorated(examplePackage::my_add))
#    expect_true(is_decorated(examplePackage::public_fn))
#    expect_false(is_decorated(examplePackage:::private_fn))
#})
#
#test_that("decorate_exported()", {
#    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)
#
#    decorate_exported("examplePackage", all = TRUE)
#
#    expect_true(is_decorated(examplePackage::my_add))
#    expect_true(is_decorated(examplePackage::public_fn))
#    expect_false(is_decorated(examplePackage:::private_fn))
#})
#
#test_that("decorate_hidden_functions()", {
#    load_all("./example-package", TRUE,  export_all = FALSE, quiet = TRUE)
#
#    decorate_hidden_functions("examplePackage")
#
#    expect_false(is_decorated(examplePackage::my_add))
#    expect_false(is_decorated(examplePackage::public_fn))
#    expect_true(is_decorated(examplePackage:::private_fn))
#})
#
## test_that('We capture the same calls for testthat:::comparison as base::trace.', {
## 
##     runCompareExamples <- function() { capture.output(suppressWarnings(example(compare))) }
## 
##     countTraces <- function() {
##         x <- 0
##         map_iterator(traces, function(trace) { x <<- x + 1 })
##         return(x)
##     }
## 
##     # base trace
##     trace_spy <- get_spy_expression()
##     trace("comparison", trace_spy$expression, where = asNamespace("testthat"))
##     runCompareExamples()
##     untrace("comparison", where = asNamespace("testthat"))
##     base_trace_count <- trace_spy$getCount()
##     expect_true(base_trace_count > 1)
## 
##     # genthat decorate
##     # TODO this decoration break the runCompareExamples call
##     decorate_exported("testthat", "comparison") 
##     runCompareExamples()
##     undecorate_all()
##     genthat_count <- countTraces()
##     expect_true(genthat_count > 1)
## 
##     expect_equal(genthat_count, base_trace_count)
## })
#
