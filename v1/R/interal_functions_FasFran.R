#' Quad Count
#'
#' Counts the frequency of points in the four quadrants - starting from the upper right going counter clockwise. Quadrants defined by the origin points x and y.
#'
#' Code adapted from Press, W. H., Teukolsky, S. A., Vetterling, W. T.,, Flannery, B. P. (2007). Numerical Recipes 3rd Edition: The Art of Scientific Computing. Cambridge University Press. ISBN: 0521880688
#'
#'
#' @param x_origin a `numeric` defining the x coordinate of the origin defining the 4 quadrants
#' @param y_origin a `numeric` defining the y coordinate of the origin defining the 4 quadrants
#' @param x a `vector` of `numeric` x coordinates
#' @param y a `vector` of `numeric` y coordinates
#'
#' @return a `vector` of frequencies of the number of points in each of the four quadrants defined by the origin point
#'
#'
#'
quadCount <- function(x_origin, y_origin, x, y) {

  #number of points being checked
  n <- length(x)

  # sum number of points in each quadrant (counter clockwise from 1 to 4)
  # sign(x - x_origin)
  # sign(y - y_origin)
  yGreater <- y > y_origin
  xGreater <- x > x_origin
  yLess <- y < y_origin
  xLess <- x < x_origin
  xequals <- x == x_origin
  yequals <- y == y_origin

  nq1 <- sum(yGreater & xGreater)
  nq2 <- sum(yGreater & xLess)
  nq4 <- sum(yLess & xGreater)
  nq3 <- sum(yLess & xLess)

  #origin point is divided equally across the four quadrants
  #as well as any other point they may be tied with the origin
  origin <- 0.25*sum(xequals & yequals)
  nq1 <- nq1 + origin
  nq2 <- nq2 + origin
  nq3 <- nq3 + origin
  nq4 <- nq4 + origin

  #if there are any ties that are NOT the origin
  #i.e. a point is exactly on the line between two quadrants
  #split the point count equally between the quadrants
  if(any(xor(xequals, yequals))){
    #add warning message
    yties_12 <- sum(yGreater & xequals)
    yties_34 <- sum(yLess & xequals)

    xties_14 <- sum(yequals & xGreater)
    xties_23 <- sum(yequals & xLess)

    nq1 <- nq1 + 0.5*xties_14 + 0.5*yties_12
    nq2 <- nq2 + 0.5*xties_23 + 0.5*yties_12
    nq4 <- nq4 + 0.5*xties_14 + 0.5*yties_34
    nq3 <- nq3 + 0.5*xties_23 + 0.5*yties_34
  }

  freqQuad1 <- nq1 / n
  freqQuad2 <- nq2 / n
  freqQuad3 <- nq3 / n
  freqQuad4 <- nq4 / n

  return(c(freqQuad1, freqQuad2, freqQuad3, freqQuad4))
}


#' Get KS Stat
#'
#' Loop through each row as(i.e. data point)
#' and defines D stat as the largest difference between the quadfrequencies, looping though each point in the sample as the origin
#'
#' Code adapted from Press, W. H., Teukolsky, S. A., Vetterling, W. T.,, Flannery, B. P. (2007). Numerical Recipes 3rd Edition: The Art of Scientific Computing. Cambridge University Press. ISBN: 0521880688
#'
#' @param S1 a `[n by 2]` `data.frame` of x and y coordinates of sample 1
#' @param S2 a `[n by 2]` `data.frame` of x and y coordinates of sample 2
#' @param cores a `numeric` defining the number of cores to use of processing
#' @param originSamples a `[n by 2]` `data.frame` of x and y coordinates that defines the origins data points.
#'
#' @return a `numeric` defining the D stat with the largest difference between the quad frequencies, after checking each point as the origin
#'
getDstat <- function(originSamples, S1, S2, cores = 1) {
  d <- 0
  n <- dim(originSamples)[1]
  dList <- parallel::mclapply(X = 1:n, mc.cores = cores, FUN = function(j) {
    quadct_S1 <- quadCount(originSamples[j, 1], originSamples[j, 2], S1[, 1], S1[, 2])
    quadct_S2 <- quadCount(originSamples[j, 1], originSamples[j, 2], S2[, 1], S2[, 2])
    d <- max(
      abs(quadct_S1[1] - quadct_S2[1]),
      abs(quadct_S1[2] - quadct_S2[2]),
      abs(quadct_S1[3] - quadct_S2[3]),
      abs(quadct_S1[4] - quadct_S2[4])
    )
  })
  return(max(unlist(dList)))
}


#' KS probability
#'
#' p-value of getting the specified 2-D KS stat.
#'
#' Code adapted from Press, W. H., Teukolsky, S. A., Vetterling, W. T.,, Flannery, B. P. (2007). Numerical Recipes 3rd Edition: The Art of Scientific Computing. Cambridge University Press. ISBN: 0521880688
#'
#'
#' @param lambda a `numeric` defining the difference in cumulative distribution function between two data sets
#'
#' @return a `numeric` defining the p-value of observing the given 2-D KS stat
#'
ksCDF <- function(lambda) {
  if(lambda < 0){
    stop("lambda must be between 0 and Inf, negative number supplied")
  }

  eps1 <- 0.001
  eps2 <- 1E-8

  range <- seq(1, 100)
  terms <- (-1)^(range - 1) * exp(-2 * range^2 %*% t(lambda^2))
  sums <- 2 * colSums(terms)
  pterms <- abs(terms)
  prev_pterms <- rbind(0, pterms[-nrow(pterms), , drop = FALSE])
  converged <- apply(pterms <= eps1 * prev_pterms | pterms <= eps2 * sums, 2L, any)
  sums[!converged] <- 1
  return(sums)
}

