
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" width="200" align="right"/>

# Fasano-Franceschini Test

<!-- badges: start -->

[![](https://img.shields.io/badge/arXiv-abs/2106.10539-yellow.svg)](https://arxiv.org/abs/2106.10539)
[![R build
status](https://github.com/nesscoder/fasano.franceschini.test/workflows/R-CMD-check/badge.svg)](https://github.com/nesscoder/fasano.franceschini.test/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![](https://img.shields.io/badge/devel%20version-1.0.1-blue.svg)](https://github.com/nesscoder/fasano.franceschini.test)
[![](https://img.shields.io/github/languages/code-size/nesscoder/fasano.franceschini.test.svg)](https://github.com/nesscoder/fasano.franceschini.test)
[![](http://cranlogs.r-pkg.org/badges/grand-total/fasano.franceschini.test?color=blue)](https://cran.r-project.org/package=fasano.franceschini.test)

<!-- badges: end -->

The `fasano.franceschini.test` package is an R implementation of the 2-D
Kolmogorov-Smirnov (**KS**) two-sample test as defined by Fasano and
Franceschini (1987). This is a variant of the 2-D two-sample KS test as
originally defined by Peacock (1983).

## Installation

You can install the released version of the `fasano.franceschini.test`
package from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fasano.franceschini.test")
```

And the development version of the `fasano.franceschini.test` package
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nesscoder/fasano.franceschini.test")
```

## Example

#### Underlying Distributions Are Different

``` r
library(fasano.franceschini.test)

#set seed for reproducible example
set.seed(123)

#create 2-D samples with the different underlying distributions
sample1Data <- data.frame(
  x = rnorm(n = 50, mean = 0, sd = 3),
  y = rnorm(n = 50, mean = 0, sd = 1)
)
sample2Data <- data.frame(
  x = rnorm(n = 50, mean = 0, sd = 1),
  y = rnorm(n = 50, mean = 0, sd = 3)
)

fasano.franceschini.test(sample1Data,sample2Data)
#> 
#>  Fasano-Francheschini Test
#> 
#> data:  sample1Data and sample2Data
#> D-stat = 0.33, p-value = 0.02221
#> sample estimates:
#> dff,1 dff,2 
#> 0.325 0.335
```

#### Underlying Distributions Are The Same

``` r
#set seed for reproducible example
set.seed(123)

#create 2-D samples with the same underlying distributions
sample1Data <- data.frame(
  x = rnorm(n = 50, mean = 0, sd = 1),
  y = rnorm(n = 50, mean = 0, sd = 1)
)
sample2Data <- data.frame(
  x = rnorm(n = 50, mean = 0, sd = 1),
  y = rnorm(n = 50, mean = 0, sd = 1)
)

fasano.franceschini.test(sample1Data,sample2Data)
#> 
#>  Fasano-Francheschini Test
#> 
#> data:  sample1Data and sample2Data
#> D-stat = 0.19, p-value = 0.4448
#> sample estimates:
#> dff,1 dff,2 
#> 0.205 0.175
```
