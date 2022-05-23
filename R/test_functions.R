# Naive 2D count
#
# @param origin numeric vector
# @param x numeric vector
# @param y numeric vector
# @return numeric vector
count2D <- function(origin, x, y) {
    xLess <- x < origin[[1]]
    yLess <- y < origin[[2]]
    xGreater <- x > origin[[1]]
    yGreater <- y > origin[[2]]

    nq1 <- sum(yGreater & xGreater)
    nq2 <- sum(yGreater & xLess)
    nq4 <- sum(yLess & xGreater)
    nq3 <- sum(yLess & xLess)

    return(c(nq1, nq2, nq3, nq4) / length(x))
}

# Naive 3D count
#
# @param origin numeric vector
# @param x numeric vector
# @param y numeric vector
# @param z numeric vector
# @return numeric vector
count3D <- function(origin, x, y, z) {
    xGreater <- x > origin[[1]]
    yGreater <- y > origin[[2]]
    zGreater <- z > origin[[3]]
    xLess <- x < origin[[1]]
    yLess <- y < origin[[2]]
    zLess <- z < origin[[3]]

    nq1 <- sum(xLess & yLess & zLess)
    nq2 <- sum(xGreater & yLess & zLess)
    nq3 <- sum(xLess & yGreater & zLess)
    nq4 <- sum(xLess & yLess & zGreater)
    nq5 <- sum(xGreater & yGreater & zLess)
    nq6 <- sum(xGreater & yLess & zGreater)
    nq7 <- sum(xLess & yGreater & zGreater)
    nq8 <- sum(xGreater & yGreater & zGreater)

    return(c(nq1, nq2, nq3, nq4, nq5, nq6, nq7, nq8) / length(x))
}

# Naive 4D count
#
# @param origin numeric vector
# @param x numeric vector
# @param y numeric vector
# @param z numeric vector
# @param t numeric vector
# @return numeric vector
count4D <- function(origin, x, y, z, t) {
    xLess <- x < origin[[1]]
    yLess <- y < origin[[2]]
    zLess <- z < origin[[3]]
    tLess <- t < origin[[4]]
    xGreater <- x > origin[[1]]
    yGreater <- y > origin[[2]]
    zGreater <- z > origin[[3]]
    tGreater <- t > origin[[4]]

    nq1  <- sum(xLess    & yLess    & zLess    & tLess)
    nq2  <- sum(xGreater & yLess    & zLess    & tLess)
    nq3  <- sum(xLess    & yGreater & zLess    & tLess)
    nq4  <- sum(xLess    & yLess    & zGreater & tLess)
    nq5  <- sum(xLess    & yLess    & zLess    & tGreater)
    nq6  <- sum(xGreater & yGreater & zLess    & tLess)
    nq7  <- sum(xGreater & yLess    & zGreater & tLess)
    nq8  <- sum(xGreater & yLess    & zLess    & tGreater)
    nq9  <- sum(xLess    & yGreater & zGreater & tLess)
    nq10 <- sum(xLess    & yGreater & zLess    & tGreater)
    nq11 <- sum(xLess    & yLess    & zGreater & tGreater)
    nq12 <- sum(xGreater & yGreater & zGreater & tLess)
    nq13 <- sum(xGreater & yGreater & zLess    & tGreater)
    nq14 <- sum(xGreater & yLess    & zGreater & tGreater)
    nq15 <- sum(xLess    & yGreater & zGreater & tGreater)
    nq16 <- sum(xGreater & yGreater & zGreater & tGreater)

    return(c(nq1, nq2, nq3, nq4, nq5, nq6, nq7, nq8, nq9,
             nq10, nq11, nq12, nq13, nq14, nq15, nq16) / length(x))
}

# Naive 5D count
#
# @param origin numeric vector
# @param x numeric vector
# @param y numeric vector
# @param z numeric vector
# @param t numeric vector
# @param v numeric vector
# @return numeric vector
count5D <- function(origin, x, y, z, t, v) {
    xl <- x < origin[[1]]
    yl <- y < origin[[2]]
    zl <- z < origin[[3]]
    tl <- t < origin[[4]]
    vl <- v < origin[[5]]
    xg <- x > origin[[1]]
    yg <- y > origin[[2]]
    zg <- z > origin[[3]]
    tg <- t > origin[[4]]
    vg <- v > origin[[5]]

    nq1  <- sum(xl & yl & zl & tl & vl)
    nq2  <- sum(xg & yl & zl & tl & vl)
    nq3  <- sum(xl & yg & zl & tl & vl)
    nq4  <- sum(xl & yl & zg & tl & vl)
    nq5  <- sum(xl & yl & zl & tg & vl)
    nq6  <- sum(xl & yl & zl & tl & vg)
    nq7  <- sum(xg & yg & zl & tl & vl)
    nq8  <- sum(xg & yl & zg & tl & vl)
    nq9  <- sum(xg & yl & zl & tg & vl)
    nq10 <- sum(xg & yl & zl & tl & vg)
    nq11 <- sum(xl & yg & zg & tl & vl)
    nq12 <- sum(xl & yg & zl & tg & vl)
    nq13 <- sum(xl & yg & zl & tl & vg)
    nq14 <- sum(xl & yl & zg & tg & vl)
    nq15 <- sum(xl & yl & zg & tl & vg)
    nq16 <- sum(xl & yl & zl & tg & vg)
    nq17 <- sum(xg & yg & zg & tl & vl)
    nq18 <- sum(xg & yg & zl & tg & vl)
    nq19 <- sum(xg & yg & zl & tl & vg)
    nq20 <- sum(xg & yl & zg & tg & vl)
    nq21 <- sum(xg & yl & zg & tl & vg)
    nq22 <- sum(xg & yl & zl & tg & vg)
    nq23 <- sum(xl & yg & zg & tg & vl)
    nq24 <- sum(xl & yg & zg & tl & vg)
    nq25 <- sum(xl & yg & zl & tg & vg)
    nq26 <- sum(xl & yl & zg & tg & vg)
    nq27 <- sum(xg & yg & zg & tg & vl)
    nq28 <- sum(xg & yg & zg & tl & vg)
    nq29 <- sum(xg & yg & zl & tg & vg)
    nq30 <- sum(xg & yl & zg & tg & vg)
    nq31 <- sum(xl & yg & zg & tg & vg)
    nq32 <- sum(xg & yg & zg & tg & vg)

    return(c(nq1,  nq2,  nq3,  nq4,  nq5,  nq6,  nq7,  nq8,
             nq9,  nq10, nq11, nq12, nq13, nq14, nq15, nq16,
             nq17, nq18, nq19, nq20, nq21, nq22, nq23, nq24,
             nq25, nq26, nq27, nq28, nq29, nq30, nq31, nq32) / length(x))
}

# Naive statistic computation
#
# @param originSamples numeric vector
# @param S1 numeric vector
# @param S2 numeric vector
# @return numeric vector
getDstat_naive <- function(originSamples, S1, S2) {
    n1 <- as.numeric(dim(S1)[1])
    n2 <- as.numeric(dim(S2)[1])

    n <- dim(originSamples)[1]
    d <- dim(originSamples)[2]
    dList <- rep(0, n)
    for (i in 1:n) {
        if (d == 2) {
            ct_S1 <- count2D(originSamples[i, ], S1[, 1], S1[, 2])
            ct_S2 <- count2D(originSamples[i, ], S2[, 1], S2[, 2])
        } else if (d == 3) {
            ct_S1 <- count3D(originSamples[i, ], S1[, 1], S1[, 2], S1[, 3])
            ct_S2 <- count3D(originSamples[i, ], S2[, 1], S2[, 2], S2[, 3])
        } else if (d == 4) {
            ct_S1 <- count4D(originSamples[i, ], S1[, 1], S1[, 2], S1[, 3],
                             S1[, 4])
            ct_S2 <- count4D(originSamples[i, ], S2[, 1], S2[, 2], S2[, 3],
                             S2[, 4])
        } else if (d == 5) {
            ct_S1 <- count5D(originSamples[i, ], S1[, 1], S1[, 2], S1[, 3],
                             S1[, 4], S1[, 5])
            ct_S2 <- count5D(originSamples[i, ], S2[, 1], S2[, 2], S2[, 3],
                             S2[, 4], S2[, 5])
        }
        dList[i] <- max(abs(ct_S1 - ct_S2))
    }
    return(max(dList))
}
