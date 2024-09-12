###########################
# Simulation of data sets #
###########################

# load package
library(basksim)

# required parameters
nSim <- 10000
k <- 5
type <- c("matrix", "bhmbasket")

scenarios <- c("Null", "Alternative", "Ascending", "Descending", "Big Good Nugget", "Small Good Nugget")

probs <- matrix(c(0.15, 0.15, 0.15, 0.15, 0.15,
                  0.35, 0.35, 0.35, 0.35, 0.35,
                  0.15, 0.15, 0.25, 0.35, 0.35,
                  0.35, 0.35, 0.25, 0.15, 0.15,
                  0.15, 0.15, 0.15, 0.15, 0.40,
                  0.40, 0.15, 0.15, 0.15, 0.15), ncol = k, byrow = TRUE)

numbers <- matrix(c(10, 15, 20, 25, 30,
                    10, 10, 25, 25, 30,
                    10, 10, 10, 20, 50), ncol = k, byrow = TRUE)

names <- c("linear", "grouped", "high_variance")

# data simulation
for(t in 1:length(type)){

  # Use a seed to ensure that the response rates are the same for both data types
  set.seed(14122023)

  for(p in 1:dim(probs)[1]){

    for(n in 1:dim(numbers)[1]){

      data <- get_data(k = k, n = numbers[n,], p = probs[p,], iter = nSim, type = type[t])

      save(data, file = paste0("U:/Thesis/R/Data/", scenarios[p], "_", names[n], "_", type[t], ".Rdata"))


    }
  }
}












