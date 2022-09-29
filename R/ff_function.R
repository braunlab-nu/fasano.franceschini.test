#' Fasano-Franceschini Test
#'
#' Performs a two-sample multidimensional Kolmogorov-Smirnov test as described
#' by Fasano and Franceschini (1987). This test evaluates the null hypothesis
#' that two i.i.d. random samples were drawn from the same underlying
#' probability distribution. The data can be of any dimension, and can be of
#' any type (continuous, discrete, or mixed).
#'
#' @param S1 \code{matrix} or \code{data.frame}.
#' @param S2 \code{matrix} or \code{data.frame}.
#' @param nPermute A nonnegative \code{integer} setting the number of permuted
#' samples to generate when estimating the permutation test p-value. Default is
#' \code{100}. If set to \code{0}, only the test statistic is computed.
#' @param threads A positive \code{integer} or \code{"auto"} setting the number
#' of threads used for performing the permutation test. If set to \code{"auto"},
#' the number of threads is determined by \code{RcppParallel::defaultNumThreads()}.
#' Default is \code{1}.
#' @param seed An optional integer to seed the PRNG used for the permutation test.
#' A seed must be passed to reproducibly compute p-values.
#' @param p.conf.level Confidence level for the confidence interval of the
#' permutation test p-value.
#' @param verbose A \code{boolean} indicating whether to display a progress bar.
#' Default is \code{TRUE}. Only available when \code{threads = 1}.
#' @param method An optional \code{character} indicating which method to use to
#' compute the test statistic. The two methods are \code{'r'} (range tree) and
#' \code{'b'} (brute force). Both methods return the same results but may vary in
#' computation speed. If this argument is not passed, the sample sizes and dimension
#' of the data are used to infer which method is likely faster. See the Details
#' section for more information.
#' @return A list with class \code{htest} containing the following components:
#'   \item{statistic}{The value of the test statistic \emph{D}.}
#'   \item{estimate}{The value of the difference statistics \emph{D1} and \emph{D2}.}
#'   \item{p.value}{The permutation test p-value.}
#'   \item{conf.int}{A binomial confidence interval for the p-value.}
#'   \item{method}{A character string indicating what type of test was performed.}
#'   \item{data.name}{A character string giving the names of the data.}
#' @references{
#' \itemize{
#'   \item{Fasano, G. & Franceschini, A. (1987). A multidimensional version of the
#'   Kolmogorov-Smirnov test. \emph{Monthly Notices of the Royal Astronomical Society},
#'   225:155-170. \doi{10.1093/mnras/225.1.155}.}
#'   \item{Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial
#'   limits illustrated in the case of the binomial. \emph{Biometrika}, 26, 404–413.
#'   \doi{10.2307/2331986}.}
#' } }
#' @examples
#' set.seed(0)
#'
#' # create 2-D samples
#' S1 <- data.frame(x = rnorm(n = 20, mean = 0, sd = 1),
#'                  y = rnorm(n = 20, mean = 1, sd = 2))
#' S2 <- data.frame(x = rnorm(n = 40, mean = 0, sd = 1),
#'                  y = rnorm(n = 40, mean = 1, sd = 2))
#'
#' # perform test
#' fasano.franceschini.test(S1, S2)
#'
#' # perform test with more permutations
#' fasano.franceschini.test(S1, S2, nPermute = 150)
#'
#' # set seed for reproducible p-value
#' fasano.franceschini.test(S1, S2, seed = 0)$p.value
#' fasano.franceschini.test(S1, S2, seed = 0)$p.value
#'
#' # change confidence level for p-value confidence interval
#' fasano.franceschini.test(S1, S2, p.conf.level = 0.99)
#'
#' # perform test using range tree method
#' fasano.franceschini.test(S1, S2, method = 'r')
#'
#' # perform test using brute force method
#' fasano.franceschini.test(S1, S2, method = 'b')
#'
#' # perform test using multiple threads to speed up p-value computation
#' \dontrun{
#' fasano.franceschini.test(S1, S2, threads = 2)
#' }
#'
#' @details The test statistic can be computed using two different methods.
#' Both methods return identical results, but have different time complexities:
#' \itemize{
#'   \item Range tree method: This method has a time complexity of
#'   \emph{O(N*log(N)^(d-1))}, where \emph{N} is the size of the larger sample
#'   and \emph{d} is the dimension of the data.
#'   \item Brute force method: This method has a time complexity of \emph{O(N^2)}.
#' }
#' The range tree method tends to be faster for low dimensional data or large
#' sample sizes, while the brute force method tends to be faster for high
#' dimensional data or small sample sizes. When \code{method} is not passed,
#' the sample sizes and dimension of the data are used to infer which method will
#' likely be faster. However, as the geometry of the samples can greatly influence
#' computation time, the method inferred to be faster may not actually be faster. To
#' perform more comprehensive benchmarking for a specific dataset, \code{nPermute}
#' can be set equal to \code{0}, which bypasses the permutation test and only
#' computes the test statistic.
#'
#' The p-value for the test is computed empirically using a permutation test. As
#' it is almost always infeasible to compute the exact permutation test p-value,
#' a Monte Carlo approximation is made instead. This estimate is a binomially
#' distributed random variable, and thus a confidence interval can be computed.
#' The confidence interval is obtained using the procedure given in Clopper and
#' Pearson (1934).
#'
#' @export
fasano.franceschini.test <- function(S1,
                                     S2,
                                     nPermute = 100,
                                     threads = 1,
                                     seed = NULL,
                                     p.conf.level = 0.95,
                                     verbose = TRUE,
                                     method = c('r', 'b')) {
    # Store names of samples for output
    dname <- paste(deparse(substitute(S1)), "and", deparse(substitute(S2)))

    # Record sample sizes and dimension of data
    d <- ncol(S1)
    n1 <- nrow(S1)
    n2 <- nrow(S2)

    ## Validate inputs
    # Validate S1 and S2
    if (is.data.frame(S1)) {
        S1 <- as.matrix(S1)
    }
    if (is.data.frame(S2)) {
        S2 <- as.matrix(S2)
    }
    if (!is.matrix(S1) || !is.matrix(S2) || d != ncol(S2) || n1 == 0 || n2 == 0) {
        stop(paste("'S1' and 'S2' must be nonempty matrices or data frames with the",
                   "same number of columns"))
    }
    # Validate nPermute
    if (!is.numeric(nPermute) || nPermute < 0 || (nPermute %% 1 != 0)) {
        stop("'nPermute' must be a nonnegative integer")
    }
    # Validate threads
    if (threads == "auto") {
        threads <- RcppParallel::defaultNumThreads()
    }
    if (!is.numeric(threads) || threads < 1 || (threads %% 1 != 0)) {
        stop("'threads' must be a positive integer or \"auto\"")
    }
    # Validate verbose
    if (!is.logical(verbose)) {
        stop("'verbose' must be of type logical")
    }
    # Validate p.conf.level
    if (!is.numeric(p.conf.level) || p.conf.level <= 0 || p.conf.level >= 1) {
        stop("'p.conf.level' must be a number between 0 and 1")
    }
    # Validate method
    if (missing(method)) {
        N <- max(n1, n2)
        if ((d == 2 && N > 25) || (d == 3 && N > 150) || (d == 4 && N > 3500)) {
            method <- 'r'
        } else {
            method <- 'b'
        }
    } else {
        method <- match.arg(method)
    }

    # Perform FF test
    ffStats <- ffTestStatistic(S1, S2, method)
    estimate <- c(ffStats[1], ffStats[2])
    names(estimate) <- c("D1", "D2")
    Dff <- ffStats[3]
    names(Dff) <- "D"

    pval <- NULL
    p.conf.int <- NULL
    if (nPermute > 0) {
        if (threads > 1) {
            # Run parallel version of permutation test
            RcppParallel::setThreadOptions(numThreads = threads)
            if (is.null(seed)) {
                count <- permutationTestParallel(S1, S2, nPermute, method)
            } else {
                count <- permutationTestParallelSeeded(S1, S2, nPermute, method, seed)
            }
        } else {
            # Run serial version of permutation test
            if (is.null(seed)) {
                count <- permutationTest(S1, S2, nPermute, verbose, method)
            } else {
                count <- permutationTestSeeded(S1, S2, nPermute, verbose, method, seed)
            }
        }

        # Exact Monte-Carlo p-value
        pval <- (count + 1) / (nPermute + 1)
        names(pval) <- "p-value"

        # Compute confidence interval for p-value
        p.conf.int <- binom.test(count + 1,
                                 nPermute + 1,
                                 alternative = "two.sided",
                                 conf.level = p.conf.level)$conf.int
    }

    # Construct output
    result <- list(statistic = Dff,
                   p.value = pval,
                   conf.int = p.conf.int,
                   estimate = estimate,
                   method = "Fasano-Francheschini Test",
                   data.name = dname)
    class(result) <- "htest"
    return(result)
}
