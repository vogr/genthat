context("genthat on sample package")

library(dplyr)
library(stringr)

# The sample package contains the following code:
#
# - examples [6]
#   - My-public.Rd [0]
#     - contains one example which does not touch any of the package code
#   - My-add.Rd [2]
#     - two calls to my_add
#   - My-call.Rd [4]
#     - two calls to my_call
#     - two calls to my_add
# - tests [8]
#   - testthat/testMain.R [8]
#      - one call to my_public
#      - one call to my_private
#      - one call to my_add
#      - four calls to my_call
#      - one call to my_warning
#      - one call to my_error - not recorded
# - vignettes [2]
#   - my-ext-vignette-notrace.Rmd [0]
#     - no calls to simplepkg
#   - my-ext-vigntte-trace.Rmd [1]
#     - one call to my_add
#   - my-vignette.Rmd [1]
#     - one call to my_add

test_that("gen_from_package works on a sample package", {
    skip_on_cran()
    skip_on_travis()

    # so it can be reused in the two datasets - one from installed package and
    # one from source package
    check_ret <- function(ret) {
        # check by sources
        expect_nrow(dplyr::filter(ret, str_detect(file, "My-add.Rd.R$")), 2)
        expect_nrow(dplyr::filter(ret, str_detect(file, "My-call.Rd.R$")), 4)
        expect_nrow(dplyr::filter(ret, str_detect(file, "testthat.R$")), 8)
        expect_nrow(dplyr::filter(ret, str_detect(file, "my-ext-vignette-trace.R$")), 1)
        expect_nrow(dplyr::filter(ret, str_detect(file, "my-vignette.R$")), 1)

        # check by functions
        expect_nrow(dplyr::filter(ret, str_detect(output, "/my_add/")), 7)
        expect_nrow(dplyr::filter(ret, str_detect(output, "/my_call/")), 6)
        expect_nrow(dplyr::filter(ret, str_detect(output, "/my_public/")), 1)
        expect_nrow(dplyr::filter(ret, str_detect(output, "/my_warning/")), 1)
        expect_nrow(dplyr::filter(ret, str_detect(output, "/my_private/")), 1)

        # check that =there is no error
        expect_equal(sum(is.na(ret$error)), 16)
    }

    options(genthat.tracer_type="sequence")
    output_dir <- tempfile()

    on.exit({
        unlink(output_dir, recursive=TRUE)
        options(genthat.tracer_type="set")
    })

    ret <- with_test_pkgs({
        gen_from_package(
            path="../test-pkgs-src/samplepkg",
            types="all",
            output_dir=output_dir,
            action="export",
            quiet=!is_debug_enabled()
        )
    })

    check_ret(ret)

    # we need to force unloading samplepkg since otherwise the find.package will
    # find it and the following will nto force package installation
    unloadNamespace("samplepkg")

    ret <- gen_from_package(
        path="../test-pkgs-src/samplepkg",
        types="all",
        output_dir=output_dir,
        action="export",
        quiet=!is_debug_enabled()
    )

    check_ret(ret)
})
