#' Fasano Franceschini Test
#'
#' Computes the 2-D Kolmogorov-Smirnov two-sample test as described by Fasano and Franceschini (1987).
#'
#' Code adapted from Press, W. H., Teukolsky, S. A., Vetterling, W. T.,, Flannery, B. P. (2007). Numerical Recipes 3rd Edition: The Art of Scientific Computing. Cambridge University Press. ISBN: 0521880688
#'
#' @param S1 a `[n by 2]` `data.frame` of x and y coordinates of sample 1
#' @param S2 a `[n by 2]` `data.frame` of x and y coordinates of sample 2
#' @param nBootstrap a `numeric` defining the number of bootstrapped samples to be generated for computing the empirical p-value (note this procedure is slow and computationally expensive on the order of nBootStrap*O(n^2). Default is set to 0.
#' If nBootstrap is 0, the Fasano Franceschini distributional approximation is used for defining the p-value. See Fasano and Franceschini test (1987) for details.
#' @param cores a `numeric` defining the number of cores to use for processing
#'
#' @return the 2-D ks statistic and p-value
#'
#' @references{
#' \itemize{
#' \item{Fasano, G., Franceschini, A. (1987) \doi{10.1093/mnras/225.1.155}. A multidimensional version of the Kolmogorov-Smirnov test. Monthly Notices of the Royal Astronomical Society 225:155-170.}
#' \item{Peacock J.A. (1983) \doi{10.1093/mnras/202.3.615}. Two-dimensional goodness-of-fit testing in astronomy. Monthly Notices of the Royal Astronomical Society 202:615-627.}
#' \item{Press, W. H., Teukolsky, S. A., Vetterling, W. T.,, Flannery, B. P. (2007). Numerical Recipes 3rd Edition: The Art of Scientific Computing. Cambridge University Press. ISBN: 0521880688}
#' }
#' }
#'
#' @examples
#' #Underlying distributions are different
#' #set seed for reproducible example
#' set.seed(123)
#'
#' #create 2-D samples with different underlying distributions
#' sample1Data <- data.frame(x = rnorm(n = 10,mean = 0, sd = 3), y = rnorm(n = 10,mean = 0, sd = 1))
#' sample2Data <- data.frame(x = rnorm(n = 10,mean = 0, sd = 1), y = rnorm(n = 10,mean = 0, sd = 3))
#'
#' fasano.franceschini.test(S1 = sample1Data, S2 = sample2Data)
#'
#'
#' #Underlying distributions are the same
#' #set seed for reproducible example
#' set.seed(123)
#'
#' #create 2-D samples with the same underlying distributions
#' sample1Data <- data.frame(x = rnorm(n = 10,mean = 0, sd = 1), y = rnorm(n = 10,mean = 0, sd = 1))
#' sample2Data <- data.frame(x = rnorm(n = 10,mean = 0, sd = 1), y = rnorm(n = 10,mean = 0, sd = 1))
#'
#' fasano.franceschini.test(S1 = sample1Data, S2 = sample2Data)
#'
#' @import methods
#' @export

fasano.franceschini.test <- function(S1, S2, nBootstrap = 0, cores = 1) {

  #validate inputs
  if(!is.data.frame(S1) | !is.data.frame(S2)){
    stop("S1 and S2 must be a `data.frame`")
  }
  if(ncol(S1) != 2 | ncol(S2) != 2){
    stop("S1 and S2 must be a `data.frame` of dim nrows x 2 cols, more than 2 cols detected")
  }
  if(nBootstrap < 0){
    stop("nBootstrap must be a positive value")
  }


  start <- Sys.time()
  # determine number of samples in each data set
  n1 <- dim(S1)[1]
  n2 <- dim(S2)[1]

  # determine max difference assuming first sample as origins
  d1 <- getDstat(originSamples = S1, S1 = S1, S2 = S2, cores = cores)
  # determine max difference assuming second sample as origins
  d2 <- getDstat(originSamples = S2, S1 = S1, S2 = S2, cores = cores)

  # average KS stat
  D <- (d1 + d2) / 2

  # if bootstrap is enabled, compute the bootstrapped null d
  if (nBootstrap > 0) {
    x_marg <- c(S1[, 1], S2[, 1])
    y_marg <- c(S1[, 2], S2[, 2])

    d <- parallel::mclapply(X = 1:nBootstrap, mc.cores = cores, FUN = function(i) {
      S1_resample <- data.frame(
        x = sample(x = x_marg, size = n1, replace = T),
        y = sample(x = y_marg, size = n1, replace = T)
      )
      S2_resample <- data.frame(
        x = sample(x = x_marg, size = n2, replace = T),
        y = sample(x = y_marg, size = n2, replace = T)
      )
      getDstat(originSamples = S1_resample, S1 = S1_resample, S2 = S2_resample, cores = 1)
    })

    # count the number of bootstrapped d stats that are larger than the observed
    pval <- (sum(unlist(d) > D) + 1) / nBootstrap

    new("Fasano Franceschini Test",
        list(D = D,
             pval = pval,
             time = Sys.time() - start,
             S1 = deparse(substitute(S1)),
             S2 = deparse(substitute(S2))))
  } else {
    # Use the fasano.franceschini distributional approximation
    # average Rsquared
    r1 <- stats::cor(S1[, 1], S1[, 2])
    r2 <- stats::cor(S2[, 1], S2[, 2])
    rr <- (r1^2 + r2^2) / 2

    # compute KS stat
    n <- n1 * n2 / (n1 + n2)
    lambda <- sqrt(n) * D / (1 + sqrt(1 - rr) * (0.25 - 0.75 / sqrt(n)))
    pval <- ksCDF(lambda)
    new("Fasano Franceschini Test",
        list(D = D,
             pval = pval,
             time = Sys.time() - start,
             S1 = deparse(substitute(S1)),
             S2 = deparse(substitute(S2))))
  }
}

methods::setClass( "Fasano Franceschini Test", representation("list"))


methods::setMethod("show", "Fasano Franceschini Test", function(object) {
  cat("\n")
  cat("\t\t2-D Two-sample Kolmogorov-Smirnov Test\n")
  cat("\n")
  cat("Fasano Franceschini Test (1987)\n")
  cat("Data: ", object$S1, "and", object$S2,"\n")
  cat("D-stat = ", object$D,", p-value = ", object$pval,"\n")
  cat("Run Time (s) = ", object$time,"\n")
})
