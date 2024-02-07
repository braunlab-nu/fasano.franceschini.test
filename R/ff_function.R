#' Fasano-Franceschini Test
#'
#' Performs a two-sample multivariate Kolmogorov-Smirnov test as described
#' by Fasano and Franceschini (1987). This test evaluates the null hypothesis
#' that two i.i.d. random samples were drawn from the same underlying
#' probability distribution. The data can be of any dimension and of any
#' type (continuous, discrete, or mixed).
#'
#' @param S1 \code{matrix} or \code{data.frame}.
#' @param S2 \code{matrix} or \code{data.frame}.
#' @param nPermute A nonnegative \code{integer} setting the number of permutations
#' to use for performing the permutation test. Default is \code{100}. If set to
#' \code{0}, only the test statistic is computed.
#' @param threads A positive \code{integer} or \code{"auto"} setting the number
#' of threads to use during the permutation test. If set to \code{"auto"}, the
#' number of threads is determined by \code{RcppParallel::defaultNumThreads()}.
#' Default is \code{1}.
#' @param seed An optional integer to seed the PRNG used for the permutation test.
#' A seed must be passed to reproducibly compute p-values.
#' @param verbose A \code{boolean} indicating whether to display a progress bar.
#' Default is \code{TRUE}. Only available when \code{threads = 1}.
#' @param method An optional \code{character} indicating which method to use to
#' compute the test statistic. The two methods are \code{'r'} (range tree) and
#' \code{'b'} (brute force). Both methods return the same results but may vary in
#' computation speed. If this argument is not passed, the sample sizes and dimension
#' of the data are used to infer which method is likely faster. See the Details
#' section for more information.
#' @return A list of class \code{htest} containing the following components:
#'   \item{statistic}{The value of the test statistic.}
#'   \item{p.value}{The permutation test p-value.}
#'   \item{method}{The name of the test.}
#'   \item{data.name}{The names of the original data objects.}
#' @references{
#' \itemize{
#'   \item{Fasano, G. & Franceschini, A. (1987). A multidimensional version of the
#'   Kolmogorov-Smirnov test. \emph{Monthly Notices of the Royal Astronomical Society},
#'   225:155-170. \doi{10.1093/mnras/225.1.155}.}
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
#' likely be faster. However, as the geometry of the samples can influence
#' computation time, the method inferred to be faster may not actually be faster. To
#' perform more comprehensive benchmarking for a specific dataset, \code{nPermute}
#' can be set equal to \code{0}, which bypasses the permutation test and only
#' computes the test statistic.
#'
#' @export
fasano.franceschini.test <- function(S1,
                                     S2,
                                     nPermute = 100,
                                     threads = 1,
                                     seed = NULL,
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
    } else if (!is.numeric(threads) || threads < 1 || (threads %% 1 != 0)) {
        stop("'threads' must be a positive integer or \"auto\"")
    }
    # Validate seed
    if (!is.null(seed) && (!is.numeric(seed) || (seed %% 1 != 0))) {
        stop("'seed' must be an integer")
    }
    # Validate verbose
    if (!is.logical(verbose)) {
        stop("'verbose' must be of type logical")
    }
    # Validate method
    if (missing(method)) {
        N <- max(n1, n2)
        if ((d == 2 && N > 25) || (d == 3 && N > 200)) {
            method <- 'r'
        } else {
            method <- 'b'
        }
    } else {
        method <- match.arg(method)
    }

    # Compute the test statistic
    Dff <- ffTestStatistic(S1, S2, method)
    names(Dff) <- "D"

    # Compute the p-value (if necessary)
    pval <- NULL
    if (nPermute > 0) {
        if (threads > 1) {
            # Run parallel version of permutation test
            RcppParallel::setThreadOptions(numThreads = threads)
            if (is.null(seed)) {
                counts <- permutationTestParallel(S1, S2, nPermute, method)
            } else {
                counts <- permutationTestParallelSeeded(S1, S2, nPermute, method, seed)
            }
        } else {
            # Run serial version of permutation test
            if (is.null(seed)) {
                counts <- permutationTest(S1, S2, nPermute, verbose, method)
            } else {
                counts <- permutationTestSeeded(S1, S2, nPermute, verbose, method, seed)
            }
        }

        if (is.null(seed)) {
            pval <- permutationTestPvalue(counts[1], counts[2], nPermute)
        } else {
            pval <- permutationTestPvalueSeeded(counts[1], counts[2], nPermute, seed)
        }
        names(pval) <- "p-value"
    }

    # Construct output
    result <- list(statistic = Dff,
                   p.value = pval,
                   method = "Fasano-Franceschini Test",
                   data.name = dname)
    class(result) <- "htest"
    return(result)
}
