context("extract-package-code")

# this one is not a great unit test, but it is much easier to write the code in
# one go, instead of setting up the environment all the time
test_that("extract_package_code", {
    skip_on_cran()
    skip_on_travis()

    tmp <- tempfile()
    on.exit(unlink(tmp, recursive=TRUE))

    examples <- file.path(tmp, "examples", c("My-add.Rd.R", "My-call.Rd.R", "My-public.Rd.R"))
    tests <- file.path(tmp, "tests", "testthat.R")
    vignettes <- file.path(
        tmp,
        "vignettes",
        c("my-ext-vignette-notrace.R", "my-ext-vignette-trace.R", "my-vignette.R")
    )

    # the idea is to test the both ways of calling the function
    lib_path <- .TEST_PKG_LIB
    package_path <- file.path(.TEST_PKG_SRC, "samplepkg")

    check_files <- function(actual, expected) {
        expect_equal(actual, expected)
        expect_true(all(file.exists(unlist(expected))))
    }

    # examples
    ret <- extract_package_code("samplepkg", lib_paths=lib_path, types="examples", output_dir=tmp)
    check_files(ret, list(examples=examples))
    ret <- extract_package_code(path=package_path, types="examples", output_dir=tmp)
    check_files(ret, list(examples=examples))

    # tests
    ret <- extract_package_code("samplepkg", lib_paths=lib_path, types="tests", output_dir=tmp)
    check_files(ret, list(tests=tests))
    ret <- extract_package_code(path=package_path, types="tests", output_dir=tmp)
    check_files(ret, list(tests=tests))

    # vignettes
    ret <- extract_package_code("samplepkg", lib_paths=lib_path, types="vignettes", output_dir=tmp)
    check_files(ret, list(vignettes=vignettes))
    ret <- extract_package_code(path=package_path, types="vignettes", output_dir=tmp)
    check_files(ret, list(vignettes=vignettes))

    # test filter
    ret <- extract_package_code("samplepkg", lib_paths=lib_path, types="all", filter="add", output_dir=tmp)
    check_files(ret, list(examples=examples[1], tests=character(), vignettes=character()))
    ret <- extract_package_code(path=package_path, filter="add", output_dir=tmp)
    check_files(ret, list(examples=examples[1], tests=character(), vignettes=character()))

    # test empty package
    ret <- extract_package_code("emptypkg", lib_paths=.TEST_PKG_LIB, types="all", output_dir=tmp)
    expect_equivalent(unlist(ret), character())
    ret <- extract_package_code(path=file.path(.TEST_PKG_SRC, "emptypkg"), types="all", output_dir=tmp)
    expect_equivalent(unlist(ret), character())
})

