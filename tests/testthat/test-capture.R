context("test capture")

test_that("decorate with package function creates a decorated function of the original", {
    on.exit(reset_call_traces())
    
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

test_that("decorate with local function creates a decorated function of the original", {
    on.exit(reset_call_traces())
    
    entry <- new.env()
    exit <- new.env()

    f <- function(a,b) { a+b }
    
    d <-
        decorate_function(
            "f",
            f,
            .entry=save_calling_args(entry),
            .exit=save_calling_args(exit))

    expect_equal(formals(d), formals(d))
    expect_equal(environment(d), environment(f))
    
    d(1,2)

    expect_equal(length(entry), 1)
    expect_equal(entry$c1$call_id, 0)
    expect_equal(entry$c1$name, "f")
    expect_equal(entry$c1$args, list(a=1, b=2))

    expect_equal(length(exit), 1)
    expect_equal(exit$c1$call_id, 0)
    expect_equal(exit$c1$retv, 3)
})

# TODO: move to replacements
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

