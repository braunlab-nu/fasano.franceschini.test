
<!-- README.md is generated from README.Rmd. Please edit that file -->

fasano.franceschini.test
========================

<!-- badges: start -->
<!-- badges: end -->

The fasano.franceschini.test package is an R implementation of the 2-D
Kolmogorov-Smirnov (KS) two-sample test as defined by Fasano and
Franceschini (1987). This is a variant of the 2-D two-sample KS test as
originally defined by Peacock (1983).

Installation
------------

You can install the released version of fasano.franceschini.test from
[CRAN](https://CRAN.R-project.org) with:

    install.packages("fasano.franceschini.test")

And the development version from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("nesscoder/fasano.franceschini.test")

Example
-------

This is a basic example which shows you how to solve a common problem:

    library(fasano.franceschini.test)
    ## basic example code

    sample1Data <- data.frame(x = rnorm(n = 100,mean = 0, sd = 3),
                              y = rnorm(n = 100,mean = 0, sd = 1))
    sample2Data <- data.frame(x = rnorm(n = 100,mean = 0, sd = 1),
                              y = rnorm(n = 100,mean = 0, sd = 3))

    fasano.franceschini.test(sample1Data,sample2Data)
    #> Time difference of 0.05070114 secs
    #> $ksStat
    #> [1] 1.885351
    #> 
    #> $pval
    #> [1] 0.00163527
