genthat - test case generation for R
=====

[![Build Status](https://travis-ci.org/fghibellini/genthat.svg)](https://travis-ci.org/fghibellini/genthat) 
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/genthat)](http://cran.r-project.org/package=genthat)

*genthat* is a framework for unit tests generation from source code and for test execution, and filtering of test cases based on C code coverage using `gcov` and R code coverage using `covr`.

# Installation

Even thought the development of the package started sometime ago it is still
rather experimental and no available from CRAN release yet.
However, that is one of the near future plans to have a stable version and
release it through CRAN.

It can be installed easily using the `devtools` package:

```r
library(devtools)
install_github('PRL-PRG/genthat')
```

Or download the sources and build manually. If you're running R on Windows, you need to install Rtools.

Usage
-----

```r
library("genthat")
library("somePackage")

fn1 <- function(a) { a + 1 }
fn2 <- function(b) { b + 2 }

# STEP 1 - Decorate one or more functions so that calls to it are recorded.

# a) create a traced version of a function (the original is not affected)
fn1 <- decorate_function_val(fn1, "fn1_label")
# b) decorate a function bound in an environment
decorate_function("fn2", env = environment())
# c) decorate a function exported from a package
decorate_exported("somePackage", c("exported_fn1", "exported_fn2"))
# d) decorate all the functions a package exports
decorate_exported("somePackage", all = TRUE)
# e) decorate all non-exported function in package
decorate_hidden_functions("somePackage")
# f) to decorate a single non-exported function just call
decorate_function("hidden_fn1", env = getNamespace("somePackage"))

# STEP 2 - Call some code that calls the decorated functions (this will generate the traces).

fn1(42)
fn2(69)
# utility function to run code accompanying the package
run_package("somePackage", include_tests = TRUE, include_vignettes = TRUE, include_man_pages = TRUE)

# STEP 3 - Undecorate all the functions.

undecorate_all()

# STEP 4 - The traces were stored internally and are made available through an iterable interface.

# a) Iterate over the traces.
traces <- genthat::traces
while (traces$has_next()) {
    trace <- traces$get_next()
    print(trace)
}
# b) Generate regression tests from the traces. *
gen_tests(output_dir = "./genthat_tests")

# *) In this case it wouldn't generate anything as all the traces were consumed by the while loop.

```
Alternatively we provide wrapper functions that cover the most common usecases.

```r
library("genthat")
library("somePackage")

# This call will:
# 1) decorate all the functions defined in the package (exported & hidden)
# 2) run the tests in the package
# 3) generate new tests from the traces
gen_from_package("somePackage", include_tests = TRUE,  output_dir = "./genthat_tests")
```

