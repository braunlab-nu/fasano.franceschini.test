#' Benchmarks methods for performing the Fasano-Franceschini Test
#'
#' This function estimates which method (range tree or brute force) is faster for
#' the given samples. Both methods return the same results but vary in computation
#' time. The range tree method has a better time complexity than the brute force
#' method, but the latter may perform better for high dimensional data or small
#' sample sizes.
#'
#' @param S1 \code{matrix} or \code{data.frame}.
#' @param S2 \code{matrix} or \code{data.frame}.
#' @param times Number of times to benchmark each method.
#' @return A list containing the following components:
#'   \item{method}{A character ('r' or 'b') indicating which method was fastest.}
#'   \item{time}{The mean time to compute the test statistic using the faster method.}
#'
#' @examples
#' set.seed(0)
#' S1 <- data.frame(x = rnorm(n = 50, mean = 0, sd = 1),
#'                  y = rnorm(n = 50, mean = 0, sd = 1))
#' S2 <- data.frame(x = rnorm(n = 50, mean = 1, sd = 1),
#'                  y = rnorm(n = 50, mean = 1, sd = 1))
#' benchmark(S1, S2)
#'
#' @export
benchmark <- function(S1, S2, times = 3) {
    time.r <- mean(microbenchmark(
        fasano.franceschini.test(S1, S2, nPermute = 0, threads = 1, method = 'r'))$time)
    time.b <- mean(microbenchmark(
        fasano.franceschini.test(S1, S2, nPermute = 0, threads = 1, method = 'b'))$time)
    if (time.r <= time.b) {
        return(list(method = 'r', time = time.r/10^6))
    } else {
        return(list(method = 'b', time = time.b/10^6))
    }
}
