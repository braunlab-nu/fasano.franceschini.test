#####################################################
### These functions are for testing purposes only ###
#####################################################

# Naive 2D count
#
# @param origin numeric vector
# @param S n x 2 dimensional numeric matrix
# @return numeric vector
count2D <- function(origin, S) {
    xLess <- S[,1] < origin[[1]]
    yLess <- S[,2] < origin[[2]]
    xGreater <- S[,1] > origin[[1]]
    yGreater <- S[,2] > origin[[2]]

    nq <- c()
    nq[1] <- sum(yGreater & xGreater)
    nq[2] <- sum(yGreater & xLess)
    nq[3] <- sum(yLess & xLess)
    nq[4] <- sum(yLess & xGreater)

    return(nq / dim(S)[1])
}

# Naive 3D count
#
# @param origin numeric vector
# @param S n x 3 dimensional numeric matrix
# @return numeric vector
count3D <- function(origin, S) {
    xGreater <- S[,1] > origin[[1]]
    yGreater <- S[,2] > origin[[2]]
    zGreater <- S[,3] > origin[[3]]
    xLess <- S[,1] < origin[[1]]
    yLess <- S[,2] < origin[[2]]
    zLess <- S[,3] < origin[[3]]

    nq <- c()
    nq[1] <- sum(xLess & yLess & zLess)
    nq[2] <- sum(xGreater & yLess & zLess)
    nq[3] <- sum(xLess & yGreater & zLess)
    nq[4] <- sum(xLess & yLess & zGreater)
    nq[5] <- sum(xGreater & yGreater & zLess)
    nq[6] <- sum(xGreater & yLess & zGreater)
    nq[7] <- sum(xLess & yGreater & zGreater)
    nq[8] <- sum(xGreater & yGreater & zGreater)

    return(nq / dim(S)[1])
}

# Naive 4D count
#
# @param origin numeric vector
# @param S n x 4 dimensional numeric matrix
# @return numeric vector
count4D <- function(origin, S) {
    xLess <- S[,1] < origin[[1]]
    yLess <- S[,2] < origin[[2]]
    zLess <- S[,3] < origin[[3]]
    tLess <- S[,4] < origin[[4]]
    xGreater <- S[,1] > origin[[1]]
    yGreater <- S[,2] > origin[[2]]
    zGreater <- S[,3] > origin[[3]]
    tGreater <- S[,4] > origin[[4]]

    nq <- c()
    nq[1]  <- sum(xLess    & yLess    & zLess    & tLess)
    nq[2]  <- sum(xGreater & yLess    & zLess    & tLess)
    nq[3]  <- sum(xLess    & yGreater & zLess    & tLess)
    nq[4]  <- sum(xLess    & yLess    & zGreater & tLess)
    nq[5]  <- sum(xLess    & yLess    & zLess    & tGreater)
    nq[6]  <- sum(xGreater & yGreater & zLess    & tLess)
    nq[7]  <- sum(xGreater & yLess    & zGreater & tLess)
    nq[8]  <- sum(xGreater & yLess    & zLess    & tGreater)
    nq[9]  <- sum(xLess    & yGreater & zGreater & tLess)
    nq[10] <- sum(xLess    & yGreater & zLess    & tGreater)
    nq[11] <- sum(xLess    & yLess    & zGreater & tGreater)
    nq[12] <- sum(xGreater & yGreater & zGreater & tLess)
    nq[13] <- sum(xGreater & yGreater & zLess    & tGreater)
    nq[14] <- sum(xGreater & yLess    & zGreater & tGreater)
    nq[15] <- sum(xLess    & yGreater & zGreater & tGreater)
    nq[16] <- sum(xGreater & yGreater & zGreater & tGreater)

    return(nq / dim(S)[1])
}

# Naive 5D count
#
# @param origin numeric vector
# @param S n x 5 dimensional numeric matrix
# @return numeric vector
count5D <- function(origin, S) {
    xl <- S[,1] < origin[[1]]
    yl <- S[,2] < origin[[2]]
    zl <- S[,3] < origin[[3]]
    tl <- S[,4] < origin[[4]]
    vl <- S[,5] < origin[[5]]
    xg <- S[,1] > origin[[1]]
    yg <- S[,2] > origin[[2]]
    zg <- S[,3] > origin[[3]]
    tg <- S[,4] > origin[[4]]
    vg <- S[,5] > origin[[5]]

    nq <- c()
    nq[1]  <- sum(xl & yl & zl & tl & vl)
    nq[2]  <- sum(xg & yl & zl & tl & vl)
    nq[3]  <- sum(xl & yg & zl & tl & vl)
    nq[4]  <- sum(xl & yl & zg & tl & vl)
    nq[5]  <- sum(xl & yl & zl & tg & vl)
    nq[6]  <- sum(xl & yl & zl & tl & vg)
    nq[7]  <- sum(xg & yg & zl & tl & vl)
    nq[8]  <- sum(xg & yl & zg & tl & vl)
    nq[9]  <- sum(xg & yl & zl & tg & vl)
    nq[10] <- sum(xg & yl & zl & tl & vg)
    nq[11] <- sum(xl & yg & zg & tl & vl)
    nq[12] <- sum(xl & yg & zl & tg & vl)
    nq[13] <- sum(xl & yg & zl & tl & vg)
    nq[14] <- sum(xl & yl & zg & tg & vl)
    nq[15] <- sum(xl & yl & zg & tl & vg)
    nq[16] <- sum(xl & yl & zl & tg & vg)
    nq[17] <- sum(xg & yg & zg & tl & vl)
    nq[18] <- sum(xg & yg & zl & tg & vl)
    nq[19] <- sum(xg & yg & zl & tl & vg)
    nq[20] <- sum(xg & yl & zg & tg & vl)
    nq[21] <- sum(xg & yl & zg & tl & vg)
    nq[22] <- sum(xg & yl & zl & tg & vg)
    nq[23] <- sum(xl & yg & zg & tg & vl)
    nq[24] <- sum(xl & yg & zg & tl & vg)
    nq[25] <- sum(xl & yg & zl & tg & vg)
    nq[26] <- sum(xl & yl & zg & tg & vg)
    nq[27] <- sum(xg & yg & zg & tg & vl)
    nq[28] <- sum(xg & yg & zg & tl & vg)
    nq[29] <- sum(xg & yg & zl & tg & vg)
    nq[30] <- sum(xg & yl & zg & tg & vg)
    nq[31] <- sum(xl & yg & zg & tg & vg)
    nq[32] <- sum(xg & yg & zg & tg & vg)

    return(nq / dim(S)[1])
}

# Naive statistic computation
#
# @param originSamples numeric vector
# @param S1 numeric vector
# @param S2 numeric vector
# @return numeric vector
getDstat_naive <- function(originSamples, S1, S2) {
    n <- dim(originSamples)[1]
    d <- dim(originSamples)[2]
    dList <- c()
    for (i in 1:n) {
        count <- switch(d, NULL, count2D, count3D, count4D, count5D)
        ct_S1 <- count(originSamples[i,], S1)
        ct_S2 <- count(originSamples[i,], S2)
        dList[i] <- max(abs(ct_S1 - ct_S2))
    }
    return(max(dList))
}
