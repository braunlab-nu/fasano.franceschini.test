
<!-- README.md is generated from README.Rmd. Please edit that file -->

Fasano Franceschini Test
========================

<!-- badges: start -->
<!-- badges: end -->

The fasano.franceschini.test package is an R implementation of the 2-D
Kolmogorov-Smirnov (**KS**) two-sample test as defined by Fasano and
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

#### Underlying Distributions Are Different

    library(fasano.franceschini.test)

    #set seed for reproducible example
    set.seed(123)

    #create 2-D samples with the different underlying distributions
    sample1Data <- data.frame(
      x = rnorm(n = 100, mean = 0, sd = 3),
      y = rnorm(n = 100, mean = 0, sd = 1)
    )
    sample2Data <- data.frame(
      x = rnorm(n = 100, mean = 0, sd = 1),
      y = rnorm(n = 100, mean = 0, sd = 3)
    )

    fasano.franceschini.test(sample1Data,sample2Data)
    #> 
    #>      2-D Two-sample Kolmogorov-Smirnov Test
    #> 
    #> Fasano Franceschini Test (1987)
    #> Data:  sample1Data and sample2Data 
    #> D-stat =  0.3 , p-value =  0.002057127 
    #> Run Time (s) =  0.04069901

#### Underlying Distributions Are The Same

    #set seed for reproducible example
    set.seed(123)

    #create 2-D samples with the same underlying distributions
    sample1Data <- data.frame(
      x = rnorm(n = 100, mean = 0, sd = 1),
      y = rnorm(n = 100, mean = 0, sd = 1)
    )
    sample2Data <- data.frame(
      x = rnorm(n = 100, mean = 0, sd = 1),
      y = rnorm(n = 100, mean = 0, sd = 1)
    )

    fasano.franceschini.test(sample1Data,sample2Data)
    #> 
    #>      2-D Two-sample Kolmogorov-Smirnov Test
    #> 
    #> Fasano Franceschini Test (1987)
    #> Data:  sample1Data and sample2Data 
    #> D-stat =  0.14 , p-value =  0.4420642 
    #> Run Time (s) =  0.03168201
