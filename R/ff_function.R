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
#' @param nPermute a nonnegative \code{integer} setting the number of permuted
#' samples to generate when estimating the permutation test p-value. Default is
#' 100. If set to 0, no p-value is estimated.
#' @param threads a positive \code{integer} or \code{"auto"} setting the number
#' of threads used for performing the permutation test. If set to \code{"auto"},
#' the number of threads is determined by \code{RcppParallel::defaultNumThreads()}.
#' Default is 1.
#' @param cores allowed for backwards compatibility. \code{threads} is now the
#' preferred argument name.
#' @param seed optional integer to seed the PRNG used for the permutation test.
#' Default is \code{NULL}. Only available for serial version (\code{threads} = 1).
#' @param p.conf.level confidence level for the confidence interval of the
#' permutation test p-value.
#' @param verbose a \code{boolean} indicating whether to display a progress bar.
#' Default is \code{TRUE}. Only available for serial version (\code{threads} = 1).
#' @param method a \code{character} indicating which method to use to compute the
#' test statistic. Must be either \code{'r'} for the range-tree method (default),
#' or \code{'b'} for the brute force method. Both return the same results, but
#' may vary in computation speed. See the Details section for more information.
#' @return A list with class \code{htest} containing the following components:
#'   \item{statistic}{the value of the test statistic Z.}
#'   \item{estimate}{the value of the difference statistics D1 and D2.}
#'   \item{p.value}{the permutation test p-value.}
#'   \item{conf.int}{a binomial confidence interval for the p-value.}
#'   \item{method}{a character string indicating what type of test was performed.}
#'   \item{data.name}{a character string giving the names of the data.}
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
#' # create 2-D samples using data frames
#' S1 <- data.frame(x = rnorm(n = 50, mean = 1, sd = 2),
#'                  y = rnorm(n = 50, mean = 3, sd = 1))
#' S2 <- data.frame(x = rnorm(n = 150, mean = 1, sd = 2),
#'                  y = rnorm(n = 150, mean = 3, sd = 1))
#'
#' # perform test (serial version)
#' fasano.franceschini.test(S1, S2)
#'
#' # perform test with more permutations
#' fasano.franceschini.test(S1, S2, nPermute = 200)
#'
#' # set seed for reproducible p-value
#' fasano.franceschini.test(S1, S2, seed = 0)
#'
#' # change confidence level for p-value confidence interval
#' fasano.franceschini.test(S1, S2, p.conf.level = 0.99)
#'
#' # perform test (parallel version, 2 threads)
#' \dontrun{
#' fasano.franceschini.test(S1, S2, threads = 2)
#' }
#'
#'
#' # create 3-D mixed samples using matrices
#' S1 <- cbind(rgamma(n = 43, shape = 2),
#'             rpois(n = 43, lambda = 5),
#'             rpois(n = 43, lambda = 3.5))
#' S2 <- cbind(rgamma(n = 72, shape = 2),
#'             rpois(n = 72, lambda = 5),
#'             rpois(n = 72, lambda = 5))
#'
#' # perform test using range tree method
#' fasano.franceschini.test(S1, S2, seed = 0, method = 'r')
#'
#' # perform test using brute force method
#' fasano.franceschini.test(S1, S2, seed = 0, method = 'b')
#'
#' @details The test statistic can be computed using two different methods.
#' Both methods return identical results, but vary in computation time.
#' \itemize{
#'   \item Range tree method (\code{method = 'r'}): This method has a time
#'   complexity of \emph{O(n*log(n)^(d-1))}, where \emph{n} is the size of
#'   the larger sample and \emph{d} is the dimension of the data.
#'   \item Brute force method (\code{method = 'b'}): This method has a time
#'   complexity of \emph{O(n^2)}.
#' }
#' When \emph{d = 2} (regardless of \emph{n}), or \emph{d > 2} and \emph{n} is
#' large, the range tree method tends to outperform the brute force method.
#' When \emph{d > 2} and \emph{n} is small, the brute force method tends to
#' outperform the range tree method.
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
                                     cores,
                                     seed = NULL,
                                     p.conf.level = 0.95,
                                     verbose = TRUE,
                                     method = c('r','b')) {
    # Store names of samples for output
    dname <- paste(deparse(substitute(S1)), "and", deparse(substitute(S2)))

    ## Validate inputs
    # Validate S1 and S2
    if (is.data.frame(S1)) {
        S1 <- as.matrix(S1)
    }
    if (is.data.frame(S2)) {
        S2 <- as.matrix(S2)
    }
    if (!is.matrix(S1) || !is.matrix(S2) || ncol(S1) != ncol(S2)) {
        stop(paste("'S1' and 'S2' must be matrices or data frames with the same",
                   "number of columns"))
    }
    # Validate nPermute
    if (!is.numeric(nPermute) || nPermute < 0 || (nPermute %% 1 != 0)) {
        stop("'nPermute' must be a nonnegative integer")
    }
    # Check if cores was passed
    if (!missing(cores)) {
        warning(paste("'cores' is deprecated. Use 'threads' instead. 'threads' is",
                      "set equal to 'cores'"))
        threads <- cores
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
    method <- match.arg(method)

    # Perform FF test
    ffStats <- ffTestStatistic(S1, S2, method)
    estimate <- c(ffStats[1], ffStats[2])
    names(estimate) <- c("D1", "D2")
    Z <- ffStats[3]
    names(Z) <- "Z"

    pval <- NULL
    p.conf.int <- NULL
    if (nPermute > 0) {
        if (threads > 1) {
            # Run parallel version of permutation test
            if (!is.null(seed)) {
                warning("Test cannot be seeded if using multiple threads.")
            }
            RcppParallel::setThreadOptions(numThreads = threads)
            count <- permutationTestParallel(S1, S2, nPermute, method)
        } else {
            # Run serial version of permutation test
            if (is.null(seed)) {
                count <- permutationTest(S1, S2, nPermute, verbose, method)
            } else {
                count <- permutationTestSeeded(S1, S2, nPermute, verbose, seed, method)
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
    result <- list(statistic = Z,
                   p.value = pval,
                   conf.int = p.conf.int,
                   estimate = estimate,
                   method = "Fasano-Francheschini Test",
                   data.name = dname)
    class(result) <- "htest"
    return(result)
}
