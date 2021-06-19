
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" width="200" align="right"/>

# Fasano-Franceschini Test

<!-- badges: start -->
<!-- [![](https://img.shields.io/badge/doi-10.1101/2020.11.19.389981-yellow.svg)](https://doi.org/10.1101/2020.11.19.389981) -->

[![R build
status](https://github.com/nesscoder/fasano.franceschini.test/workflows/R-CMD-check/badge.svg)](https://github.com/nesscoder/fasano.franceschini.test/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/nesscoder/fasano.franceschini.test)
[![](https://img.shields.io/github/languages/code-size/nesscoder/fasano.franceschini.test.svg)](https://github.com/nesscoder/fasano.franceschini.test)

<!-- badges: end -->

The `fasano.franceschini.test` package is an R implementation of the 2-D
Kolmogorov-Smirnov (**KS**) two-sample test as defined by Fasano and
Franceschini (1987). This is a variant of the 2-D two-sample KS test as
originally defined by Peacock (1983).

## Installation

<!-- You can install the released version of the `fasano.franceschini.test` package from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fasano.franceschini.test")
```

And the -->

You can install the development version of the
`fasano.franceschini.test` package from [GitHub](https://github.com/)
with:

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
#>      2-D Two-sample Kolmogorov-Smirnov Test
#> 
#> Fasano Franceschini Test (1987)
#> Data:  sample1Data and sample2Data 
#> D-stat =  0.33 , p-value =  0.02221184 
#> Run Time (s) =  0.01932788
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
#>      2-D Two-sample Kolmogorov-Smirnov Test
#> 
#> Fasano Franceschini Test (1987)
#> Data:  sample1Data and sample2Data 
#> D-stat =  0.19 , p-value =  0.4448033 
#> Run Time (s) =  0.008532047
```
