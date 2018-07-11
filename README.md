genthat - test case generation for R
=====

[![Build Status](https://travis-ci.org/PRL-PRG/genthat.svg)](https://travis-ci.org/PRL-PRG/genthat)
[![codecov](https://codecov.io/github/PRL-PRG/genthat/branch/feature/fixes/graphs/badge.svg)](https://codecov.io/github/PRL-PRG/genthat)
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

Or download the sources and build manually.

Usage
-----

Look for the [genthat overview](https://htmlpreview.github.io/?https://raw.githubusercontent.com/fikovnik/ISSTA18-artifact/master/overview.html)

More resources:
- [ISSTA'18 paper](http://janvitek.org/pubs/issta18.pdf)
- [ISSTA'18 artifact](https://github.com/fikovnik/ISSTA18-artifact)
