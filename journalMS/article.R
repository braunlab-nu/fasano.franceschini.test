## Package and options
library(devtools)
load_all()
library("fasano.franceschini.test")
library("parallel")
library("ggplot2")
library(viridis)
library(mvtnorm)
library(gridExtra)

#-------------------------------------#
#    fasano.franceschini.test Usage
#-------------------------------------#
# set seed for reproducible example
set.seed(123)

# create 2-D samples with the same underlying distributions
sample1Data <- data.frame(
  x = rnorm(n = 100, mean = 0, sd = 1),
  y = rnorm(n = 100, mean = 0, sd = 1)
)
sample2Data <- data.frame(
  x = rnorm(n = 100, mean = 0, sd = 1),
  y = rnorm(n = 100, mean = 0, sd = 1)
)

fasano.franceschini.test(S1 = sample1Data, S2 = sample2Data)

#-------------------------------------#
# fasano.franceschini.test Bootstrap
#-------------------------------------#
# set seed for reproducible example
set.seed(123)

# create 2-D samples with the same underlying distributions
sample1Data <- data.frame(
  x = rnorm(n = 100, mean = 0, sd = 1),
  y = rnorm(n = 100, mean = 0, sd = 1)
)
sample2Data <- data.frame(
  x = rnorm(n = 100, mean = 0, sd = 1),
  y = rnorm(n = 100, mean = 0, sd = 1)
)

fasano.franceschini.test(S1 = sample1Data, S2 = sample2Data, nBootstrap = 1000, cores = 6)

#-------------------------------------#
#  Visualization - 2-D KS test
#-------------------------------------#
png(file = paste0("~/Desktop/fftestOutput.png"),units = "in",res = 300,width = 14,height = 7)
par(mfrow = c(2,5),oma = c(0, 0, 3, 0))
set.seed(12)

spread <- 2
N <- 10
cexSize <- 1.5
S1col <- "orange"
S2col <- "blue"

S1 <- data.frame(
  x = rnorm(n = N, mean = 1, sd = 1),
  y = rnorm(n = N, mean = 0, sd = 1)
)
S2 <- data.frame(
  x = rnorm(n = N, mean = -1, sd = 1),
  y = rnorm(n = N, mean = 0, sd = 1)
)

S1 <- S1[with(S1, order(S1[, 1], S1[, 2],decreasing = F)), ]
originSamples <- S1
d <- rep(0, nrow(S1))
for (j in 1:nrow(S1)) {
  quadct_S1 <- quadCount(originSamples[j, 1], originSamples[j, 2], S1[, 1], S1[, 2])
  quadct_S2 <- quadCount(originSamples[j, 1], originSamples[j, 2], S2[, 1], S2[, 2])
  d[j] <- max(
    abs(quadct_S1[1] - quadct_S2[1]),
    abs(quadct_S1[2] - quadct_S2[2]),
    abs(quadct_S1[3] - quadct_S2[3]),
    abs(quadct_S1[4] - quadct_S2[4])
  )
  idx <- which.max(c(abs(quadct_S1[1] - quadct_S2[1]),
                     abs(quadct_S1[2] - quadct_S2[2]),
                     abs(quadct_S1[3] - quadct_S2[3]),
                     abs(quadct_S1[4] - quadct_S2[4])))
  par(pty="s")
  plot(x = 0, y = 0, main = paste0("D = | ",sprintf(quadct_S1[idx], fmt = "%.2f")," - ",sprintf(quadct_S2[idx], fmt = "%.2f")," | = ", sprintf(d[j], fmt = "%.2f")),
       xlab = "",
       ylab = "",
       cex = 0,
       ylim = range(-3, 3),
       xlim = range(-3, 3),
       asp = 1)
  abline(v = S1[j, 1], col = "grey", lty = "dashed", lwd = 3)
  abline(h = S1[j, 2], col = "grey", lty = "dashed", lwd = 3)
  points(S1, bg = S1col, pch = 21, cex = cexSize, lwd = 1)
  points(S2, bg = S2col, pch = 21, cex = cexSize, lwd = 1)
  points(S1[j, 1],S1[j, 2], bg = S1col, cex = cexSize+2, lwd = 1)
  text(x = spread, y = spread + 1, labels = sprintf(quadct_S1[1], fmt = "%.2f"), col = S1col)
  text(x = spread, y = spread + 1, labels = " | ", adj = -1, col = "black")
  text(x = spread, y = spread + 1, labels = sprintf(quadct_S2[1], fmt = "%.2f"), adj = -0.7, col = S2col)

  text(x = -spread, y = spread + 1, labels = sprintf(quadct_S1[2], fmt = "%.2f"), adj = 1.5, col = S1col)
  text(x = -spread, y = spread + 1, labels = " | ", adj = 1.5, col = "black")
  text(x = -spread, y = spread + 1, labels = sprintf(quadct_S2[2], fmt = "%.2f"), adj = 0.35, col = S2col)

  text(x = -spread, y = -spread - 1, labels = sprintf(quadct_S1[3], fmt = "%.2f"), adj = 1.5, col = S1col)
  text(x = -spread, y = -spread - 1, labels = " | ", adj = 1.5, col = "black")
  text(x = -spread, y = -spread - 1, labels = sprintf(quadct_S2[3], fmt = "%.2f"), adj = 0.35, col = S2col)

  text(x = spread, y = -spread - 1, labels = sprintf(quadct_S1[4], fmt = "%.2f"), col = S1col)
  text(x = spread, y = -spread - 1, labels = " | ", adj = -1, col = "black")
  text(x = spread, y = -spread - 1, labels = sprintf(quadct_S2[4], fmt = "%.2f"), adj = -0.7, col = S2col)

}
dmax <- sprintf(max(d), fmt = "%.2f")
title <- bquote(paste("  D"["max"]*" = ", .(dmax)))
mtext(title, outer = TRUE, side = 3,cex = 2, font = 1, col = S1col)
mtext(bquote(paste(" D"["max"]*" =       ")), outer = TRUE, side = 3,cex = 2, font = 1)
dev.off()

#-------------------------------------#
# Visualization - 1-D KS test
#-------------------------------------#
png(file = paste0("~/Desktop/pdfvsCDF.png"), units = "in", res = 300, width = 12, height = 5.5)
par(mfrow = c(1, 2))

# simulate two distributions
sample1 <- rnorm(10000, 0, 3)
sample2 <- rnorm(10000, 4, 3)
group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
# create ECDF of data
cdf1 <- ecdf(sample1)
cdf2 <- ecdf(sample2)
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out = length(sample1))
x0 <- minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))))]
y0 <- cdf1(x0)
y1 <- cdf2(x0)
lwd <- 5
x <- seq(-15, 15, length = 100)
plot(x, dnorm(x, 0, 3), xlim = c(-15, 15), main = "Underlying Probability Density Function (pdf)\n Sample1 vs Sample2", ylab = "Pdf(x)", col = S1col, type = "l", lwd = lwd)
lines(x, dnorm(x, 4, 3), col = S2col, type = "l", lwd = lwd)
plot(cdf1, main = "Empirical Cumulative Density Function (eCDF) \n Sample1 vs Sample2", ylab = "eCDF(x)", verticals = TRUE, do.points = FALSE, col = S1col, lwd = lwd)
plot(cdf2, verticals = F, do.points = FALSE, col = S2col, add = TRUE, lwd = lwd)

points(c(x0, x0), c(y0, y1), pch = 16, cex = 2, col = "black", lwd = lwd)
segments(x0, y0, x0, y1, col = "black", lty = "dotted", lwd = lwd)
points(c(x0, x0), c(y0, y1), pch = 16, cex = 1.5, col = "grey")
dev.off()

#-------------------------------------#
# Visualization - 2-D KS issue
#-------------------------------------#
m <- c(0,0)
sigma <- matrix(c(1,0,0,1), nrow=2)
data.grid <- expand.grid(x = seq(-3, 3, length.out=200), y = seq(-3, 3, length.out=200))
data <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m, sigma = sigma))
recFill <- 'black'

png(file = paste0("~/Desktop/CDF2Dissue.png"), units = "in", res = 300, width = 8, height = 8)
theme_update(plot.title = element_text(hjust = 0.5, face = "bold"),
             axis.title = element_text(face="bold"),axis.text = element_text(face="bold") )
p1 <- ggplot(data, aes(x=x, y=y)) +
  geom_raster(aes(fill = prob)) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)+
  annotate(geom = "rect",xmin=-3, xmax=-1, ymin=-3, ymax=-1, fill = recFill, alpha = 0.5)+
  annotate(geom = "segment",x = -3, y = -1, xend = -1, yend = -1, colour = "black")+
  annotate(geom = "segment",x = -1, y = -3, xend = -1, yend = -1, colour = "black")+
  annotate(geom = "point", x=-1, y=-1, size = 3, pch = 21, fill="orange") +
  scale_fill_gradientn(colours = viridis(10)) +
  theme(legend.position = "none",panel.background = element_blank())+
  ggtitle("P(x < X, y < Y)")

p2 <- ggplot(data, aes(x=x, y=y)) +
  geom_raster(aes(fill = prob)) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)+
  annotate(geom = "rect",xmin=-3, xmax=-1, ymin=-3, ymax=3, fill = recFill, alpha = 0.5)+
  annotate(geom = "rect", xmin=-1, xmax=3, ymin=-3, ymax=-1, fill = recFill, alpha = 0.5)+
  annotate(geom = "segment",x = -1, y = -1, xend = 3, yend = -1, colour = "black")+
  annotate(geom = "segment",x = -1, y = -1, xend = -1, yend = 3, colour = "black")+
  annotate(geom = "point", x=-1, y=-1, size = 3, pch = 21, fill="orange") +
  scale_fill_gradientn(colours = viridis(10)) +
  theme(legend.position = "none",panel.background = element_blank())+
  ggtitle("1 - P(x > X, y > Y)")

p3 <- ggplot(data, aes(x=x, y=y)) +
  geom_raster(aes(fill = prob)) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)+
  annotate(geom = "rect", xmin=-3, xmax=-1, ymin=-1, ymax=3, fill = recFill, alpha = 0.5)+
  annotate(geom = "segment",x = -3, y = -1, xend = -1, yend = -1, colour = "black")+
  annotate(geom = "segment",x = -1, y = -1, xend = -1, yend = 3, colour = "black")+
  annotate(geom = "point", x=-1, y=-1, size = 3, pch = 21, fill="orange") +
  scale_fill_gradientn(colours = viridis(10)) +
  theme(legend.position = "none",panel.background = element_blank())+
  ggtitle("P(x < X, y > Y)")

p4 <- ggplot(data, aes(x=x, y=y)) +
  geom_raster(aes(fill = prob)) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)+
  annotate(geom = "rect", xmin=-1, xmax=3, ymin=-3, ymax=-1, fill = recFill, alpha = 0.5)+
  annotate(geom = "segment", x = -1, y = -1, xend = 3, yend = -1, colour = "black")+
  annotate(geom = "segment", x = -1, y = -3, xend = -1, yend = -1, colour = "black")+
  annotate(geom = "point", x=-1, y=-1, size = 3, pch = 21, fill="orange") +
  scale_fill_gradientn(colours = viridis(10)) +
  theme(legend.position = "none",panel.background = element_blank())+
  ggtitle("P(x > X, y < Y)")

pTotal <- ggarrange(p1,p2,p3,p4,
                    labels = c("A", "B","C","D"),
                    ncol = 2, nrow = 2)
pTotal
dev.off()

