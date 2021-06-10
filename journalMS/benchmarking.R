# benchmarking Scripts
# get arguments from command line
args <- commandArgs(trailingOnly = TRUE)
args <- as.numeric(args)
print(args)

# install.packages("Peacock.test")
# devtools::install_github("eddelbuettel/rbenchmark")
library(Peacock.test)
library(devtools)
load_all("./")
library(fasano.franceschini.test)
library(rbenchmark)

sample1 <- data.frame(
  x =  rnorm(args[1] / 2, mean = 0, sd = 1),
  y =  rnorm(args[1] / 2, mean = 0, sd = 1)
)

sample2 <- data.frame(
  x =  rnorm(args[1] / 2, mean = 0, sd = 1),
  y =  rnorm(args[1] / 2, mean = 0, sd = 1)
)

if (args[2] == 0 & args[3] == 1) {
  bench <- benchmark(
    replications = 10,
    "peacock" = {
      peacock2(x = sample1, y = sample2)
    },
    "fasano" = {
      fasano.franceschini.test(S1 = sample1, S2 = sample2, nBootstrap = args[2], cores = args[3])
    }
  )

  out <- bench$elapsed

  results <- data.frame(
    N = args[1],
    boots = args[2],
    cores = args[3],
    method = c("fasano", "peacock"),
    time = out
  )

} else {

  bench <- benchmark(
    replications = 10,
    "fasano" = {
      fasano.franceschini.test(S1 = sample1, S2 = sample2, nBootstrap = args[2], cores = args[3])
    }
  )

  out <- bench$elapsed

  results <- data.frame(
    N = args[1],
    boots = args[2],
    cores = args[3],
    method = c("fasano"),
    time = out
  )
}

write.table(results,
  file = paste0("~/Desktop/fasano.franceschini.test/journalMS/benchmarking/result_", args[1], "_", args[2], "_", args[3], ".txt"), sep = "\t",
  row.names = FALSE, col.names = TRUE
)
