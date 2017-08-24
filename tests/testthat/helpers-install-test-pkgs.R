.TEST_PKG_LIB <- "../test-pkgs-lib"
.TEST_PKG_SRC <- "../test-pkgs-src"

with_test_pkgs <- function(code) {
    withr::with_libpaths(.TEST_PKG_LIB, action="prefix", code)
}
