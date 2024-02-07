
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" width="200" align="right"/>

# Fasano-Franceschini Test

<!-- badges: start -->

[![](https://img.shields.io/badge/R%20Journal-10.32614/RJ--2023--067-yellow.svg)](https://doi.org/10.32614/RJ-2023-067)
[![](https://www.r-pkg.org/badges/version/fasano.franceschini.test?color=orange)](https://cran.r-project.org/package=fasano.franceschini.test)
[![](https://img.shields.io/badge/devel%20version-2.2.2-blue.svg)](https://github.com/braunlab-nu/fasano.franceschini.test)
[![](http://cranlogs.r-pkg.org/badges/grand-total/fasano.franceschini.test?color=green)](https://cran.r-project.org/package=fasano.franceschini.test)
[![R build
status](https://github.com/braunlab-nu/fasano.franceschini.test/workflows/R-CMD-check/badge.svg)](https://github.com/braunlab-nu/fasano.franceschini.test/actions)
<!-- badges: end -->

The `fasano.franceschini.test` package is an R implementation of the
multivariate Kolmogorov-Smirnov two-sample test as defined by Fasano and
Franceschini (1987).

    Fasano, G. & Franceschini, A. (1987). A multidimensional version of the
    Kolmogorov-Smirnov test. Monthly Notices of the Royal Astronomical Society,
    225:155-170. doi: 10.1093/mnras/225.1.155.

A manuscript accompanying this package was published in The R Journal.

    Puritz, C., Ness-Cohn, E., Braun, R. (2023). fasano.franceschini.test: An Implementation
    of a Multivariate KS Test in R. The R Journal, 15(3):159-171. doi: 10.32614/RJ-2023-067.

## Installation

You can install the released version of the `fasano.franceschini.test`
package from CRAN with:

``` r
install.packages("fasano.franceschini.test")
```

The development version of the `fasano.franceschini.test` package can be
installed from GitHub with:

``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("braunlab-nu/fasano.franceschini.test")
```

## Examples

#### Underlying distributions are the same

``` r
library(fasano.franceschini.test)

# set seed for reproducibility
set.seed(0)

# create 2D samples from the same underlying distribution
S1 <- data.frame(x = rnorm(n = 50, mean = 0, sd = 1),
                 y = rnorm(n = 50, mean = 0, sd = 3))
S2 <- data.frame(x = rnorm(n = 100, mean = 0, sd = 1),
                 y = rnorm(n = 100, mean = 0, sd = 3))

fasano.franceschini.test(S1, S2, seed = 0)
#> 
#>  Fasano-Franceschini Test
#> 
#> data:  S1 and S2
#> D = 1700, p-value = 0.6333
```

#### Underlying distributions are different

``` r
# set seed for reproducibility
set.seed(1)

# create 3D samples from different underlying distributions
S1 <- cbind(rgamma(n = 43, shape = 2),
            rpois(n = 43, lambda = 5),
            rpois(n = 43, lambda = 3.5))
S2 <- cbind(rgamma(n = 72, shape = 2),
            rpois(n = 72, lambda = 5),
            rpois(n = 72, lambda = 5))

fasano.franceschini.test(S1, S2, seed = 1)
#> 
#>  Fasano-Franceschini Test
#> 
#> data:  S1 and S2
#> D = 2022, p-value = 0.01977
```
