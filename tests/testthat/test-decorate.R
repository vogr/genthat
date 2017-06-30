context("decoration")

if (!requireNamespace("devtools", quietly=TRUE)) {
    stop("devtools needed for this function to work. Please install it.", call. = FALSE)
}

test_that("get_function_package_name returns the package name", {
    expect_equal(get_function_package_name(ls), "base")
    expect_equal(get_function_package_name(`%in%`), "base")
    expect_equal(get_function_package_name(tools::Rcmd), "tools")

    my_f <- function() {}

    expect_equal(get_function_package_name(my_f), NULL)
})

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

test_that("do_decorate_function decorates a function", {
    reset_call_traces()
    entry <- new.env()
    exit <- new.env()

    f <- function(a,b) { a + b }

    d <-
        do_decorate_function(
            "f",
            NULL,
            f,
            .entry=save_calling_args(entry, return_value = 0L),
            .exit=save_calling_args(exit))

    expect_equal(formals(d), formals(d))
    expect_equal(environment(d), environment(f))
    expect_equal(attributes(d), list(genthat=T))

    d(1, 2)

    expect_equal(length(entry), 1)
    expect_equal(entry$c1$name, "f")
    expect_equal(entry$c1$pkg, NULL)
    expect_equal(entry$c1$args, list(a=1, b=2))

    expect_equal(length(exit), 1)
    expect_equal(exit$c1$index, 0L)
    expect_equal(exit$c1$retv, 3)
})

test_that("on_function_entry works with multiline functions", {
    entry <- new.env()
    exit <- new.env()

    f <- function(x, y) {
        x1 <- x + 1
        y1 <- y + 1
        x1 + y1
    }

    d <-
        do_decorate_function(
            "f",
            NULL,
            f,
            .entry=save_calling_args(entry, return_value = 0L),
            .exit=save_calling_args(exit))

    expect_equal(formals(d), formals(d))
    expect_equal(environment(d), environment(f))
    expect_equal(attributes(d), list(genthat=T))

    d(1, 2)

    expect_equal(exit$c1$retv, 5)
})

test_that("do_decorate_function works with ...", {
    entry <- new.env()
    exit <- new.env()

    f <- function(...) sum(...)
    d <-
        do_decorate_function(
            "f",
            NULL,
            f,
            .entry=save_calling_args(entry, return_value = 0L),
            .exit=save_calling_args(exit))

    r <- d(a=1, b=2, 3, 4)

    expect_equal(r, f(1:4))

    expect_equal(length(entry), 1)
    expect_equal(entry$c1$name, "f")
    expect_equal(entry$c1$args, list(a=1, b=2, 3, 4))

    expect_equal(length(exit), 1)
    expect_equal(exit$c1$index, 0)
    expect_equal(exit$c1$retv, f(1:4))
})

test_that("do_decorate_function decorates a package function", {
    entry <- new.env()
    exit <- new.env()

    d <-
        do_decorate_function(
            "file_path_sans_ext",
            "tools",
            tools::file_path_sans_ext,
            .entry=save_calling_args(entry, return_value = 0L),
            .exit=save_calling_args(exit))

    expect_equal(formals(d), formals(tools::file_path_sans_ext))
    expect_equal(environment(d), environment(tools::file_path_sans_ext))

    d("a.b")

    expect_equal(length(entry), 1)
    expect_equal(entry$c1$name, "file_path_sans_ext")
    expect_equal(entry$c1$pkg, "tools")
    expect_equal(entry$c1$args, list(x="a.b"))

    expect_equal(length(exit), 1)
    expect_equal(exit$c1$index, 0)
    expect_equal(exit$c1$retv, "a")
})

test_that("is_decorated knows when a functions is decorated", {
    reset_genthat()

    f <- function() {}
    expect_false(is_decorated(f))

    d <- do_decorate_function("f", NULL, f)
    expect_true(is_decorated(d))
})

test_that("decorate_environment decorates all functions in the environment", {
    reset_genthat()

    on.exit(detach(package:samplepkg))

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
        sort(names(funs)))
})

test_that("reassign_function replaces function body and add attributes", {
    f <- function(a,b) {a+b}
    g <- function(a,b) {a-b}
    attr(g, "a") <- TRUE

    reassign_function(f, g)

    expect_equal(f(1, 2), -1)
    expect_equal(attr(f, "a"), TRUE)
})

test_that("create_duplicate duplicates a function", {
    f <- function(a, b) {a + b}

    expect_identical(f, create_duplicate(f))
    expect_error(create_duplicate(NULL))
})

test_that("remove_replacement", {
    reset_genthat()

    f <- function() {}

    r <- create_replacement("a", environment(f), f, f, f)
    add_replacement(r)

    r2 <- remove_replacement("a")

    expect_equal(r, r2)
    expect_equal(length(get_replacements()), 0)
})


test_that("reset_function", {
    on.exit(detach(package:samplepkg))

    devtools::load_all("samplepkg")

    decorate_functions(samplepkg::my_public)

    expect_true(is_decorated(samplepkg::my_public))

    reset_functions(samplepkg::my_public)

    expect_false(is_decorated(samplepkg::my_public))
    expect_equal(length(get_replacements()), 0)
})

test_that("decorate_function returns decorated function", {
    f <- function(x) x
    decorated <- decorate_function(f)
    expect_true(is.function(decorated))
    expect_equal(decorated, f)
})

test_that("decorate_and_replace_one checks for primitive functions", {
    expect_error(decorate_and_replace_one("$", `$`), regexp="\\$: is a primitive function")
})

# TODO: test that we cannot decorate builtins

# TODO: test imported namespaces
