####################################################
# Design Optimization                              #
# Select optimal tuning parameters for each design #
####################################################

# required packages
library(writexl)
library(progressr)
library(basksim)
library(doFuture)


# path changes depending on the operating system
if (Sys.info()[["sysname"]]=="Linux") {
  path <- "~/Home/Thesis/R/"
} else {
  path <- "U:/Thesis/R/"
}


# ----- Setup -----
K <- 5
h0 <- 0.15
h1 <- 0.35
alpha <- 0.05
numbers <- list(c(10,15,20,25,30),
                c(10,10,25,25,30),
                c(10,10,10,20,50))
names <- c("linear", "grouped", "high_variance")

pattern <- c("Null","Alternative","Ascending","Descending","Big Good Nugget",
             "Small Good Nugget")

scenarios <- matrix(data = c(0.15, 0.15, 0.15, 0.15, 0.15,
                             0.35, 0.35, 0.35, 0.35, 0.35,
                             0.15, 0.15, 0.25, 0.35, 0.35,
                             0.35, 0.35, 0.25, 0.15, 0.15,
                             0.15, 0.15, 0.15, 0.15, 0.40,
                             0.40, 0.15, 0.15, 0.15, 0.15),
                    ncol=6, byrow = FALSE)

colnames(scenarios) <- pattern



# ----- Model parameters fixed -----
# beta prior: Beta(1,1) for fujikawa, cpp, app, cpplim, bma
shape1 <- 1
shape2 <- 1

# fixed prior distribution for mu (bhm + exnex)
mu_mean_bhm <- round(log(h0/(1-h0)) - log(h1/(1-h1)), digits = 4)
mu_mean_exnex <- round(log(h0/(1-h0)), digits = 4)
mu_sd <- 100


# ----- Selecting potential tuning parameters -----
# Fujikawa
epsilon <- seq(0.5,3, by = 0.5)
tau <- seq(0,0.5, by = 0.1)

# CPP  + CPPlim
tune_a <- seq(0.5,5, by = 0.5)
tune_b <- seq(0.5,5, by = 0.5)

# BMA
pmp0 <- seq(-4,4, by = 0.5)

# BHM + EXNEX
tau_scale <- seq(0.125,2, length = 8)

# EXNEX
w <- seq(0.1, 0.9, by = 0.1)


# ----- Create design_params -----
design_params_fujikawa <- list(epsilon = epsilon, tau = tau)
design_params_cpp <- list(tune_a = tune_a, tune_b = tune_b)
design_params_cpplim <- list(tune_a = tune_a, tune_b = tune_b)
design_params_bma <- list(pmp0 = pmp0)
design_params_bhm <- list(tau_scale = tau_scale)
design_params_exnex <- list(tau_scale = tau_scale, w = w)



# ----- Parameter Tuning -----
# Fujikawa
design_fujukawa <- setup_fujikawa(k = K, p0 = h0, shape1 = shape1,
                                  shape2 = shape2)
set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

    # data as list (one sample size scenario)
    data_list <- list()

      for(p in 1:length(pattern)){
        load(paste0(path, "Data/", pattern[p], "_", names[n], "_matrix.Rdata"))
        data_list[[p]] <- data
      }

    names(data_list) <- pattern

    # optimization
    res[[n]] <- opt_design(design = design_fujukawa, n = numbers[[n]],
                           alpha = alpha, design_params = design_params_fujikawa,
                           scenarios = scenarios, prec_digits = 3, iter = 10000,
                           data = data_list)

}

names(res) <- names

save(res, file = paste0(path, "Tuning Parameter/optParams_fujikawa.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_fujikawa.xlsx"))




# CPP
design_cpp <- setup_cpp(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)

set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

  # data as list (one sample size scenario)
  data_list <- list()

  for(p in 1:length(pattern)){
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_matrix.Rdata"))
    data_list[[p]] <- data
  }

  names(data_list) <- pattern

  # optimization
  res[[n]] <- opt_design(design = design_cpp, n = numbers[[n]],
                         alpha = alpha, design_params = design_params_cpp,
                         scenarios = scenarios, prec_digits = 3, iter = 10000,
                         data = data_list)

}

names(res) <- names

save(res, file = paste0(path, "Tuning Parameter/optParams_cpp.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_cpp.xlsx"))




# APP
design_app <- setup_app(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)

set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

  # data as list (one sample size scenario)
  data_list <- list()

  for(p in 1:length(pattern)){
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_matrix.Rdata"))
    data_list[[p]] <- data
  }

  names(data_list) <- pattern

  # optimization
  res[[n]] <- opt_design(design = design_app, n = numbers[[n]],
                         alpha = alpha, design_params = NULL,
                         scenarios = scenarios, prec_digits = 3, iter = 10000,
                         data = data_list)

}

names(res) <- names

for(i in 1:3){
  res[[i]] <- as.data.frame(res[[i]])
}

save(res, file = paste0(path, "Tuning Parameter/optParams_app.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_app.xlsx"))





# CPPlim
design_cpplim <- setup_cpplim(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)

set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

  # data as list (one sample size scenario)
  data_list <- list()

  for(p in 1:length(pattern)){
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_matrix.Rdata"))
    data_list[[p]] <- data
  }

  names(data_list) <- pattern

  # optimization
  res[[n]] <- opt_design(design = design_cpplim, n = numbers[[n]],
                         alpha = alpha, design_params = design_params_cpplim,
                         scenarios = scenarios, prec_digits = 3, iter = 10000,
                         data = data_list)

}

names(res) <- names

save(res, file = paste0(path, "Tuning Parameter/optParams_cpplim.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_cpplim.xlsx"))






# BMA
design_bma <- setup_bma(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)

set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 15)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

  # data as list (one sample size scenario)
  data_list <- list()

  for(p in 1:length(pattern)){
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_matrix.Rdata"))
    data_list[[p]] <- data
  }

  names(data_list) <- pattern

  # optimization
  res[[n]] <- opt_design(design = design_bma, n = numbers[[n]],
                         alpha = alpha, design_params = design_params_bma,
                         scenarios = scenarios, prec_digits = 3, iter = 10000,
                         data = data_list)

}

names(res) <- names

save(res, file = paste0(path, "Tuning Parameter/optParams_bma.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_bma.xlsx"))




# BHM
design_bhm <- setup_bhm(k = K, p0 = h0, p_target = h1, mu_mean = mu_mean_bhm,
                        mu_sd = mu_sd)

set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

  # data as list (one sample size scenario)
  data_list <- list()

  for(p in 1:length(pattern)){
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_bhmbasket.Rdata"))
    data_list[[p]] <- data
  }

  names(data_list) <- pattern

  # optimization
  res[[n]] <- opt_design(design = design_bhm, n = numbers[[n]],
                         alpha = alpha, design_params = design_params_bhm,
                         scenarios = scenarios, prec_digits = 3, iter = 10000,
                         data = data_list)

}

names(res) <- names

save(res, file = paste0(path, "Tuning Parameter/optParams_bhm.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_bhm.xlsx"))





# EXNEX
design_exnex <- setup_exnex(k = K, p0 = h0, basket_mean = mu_mean_exnex,
                            basket_sd = mu_sd, mu_mean = mu_mean_exnex,
                            mu_sd = mu_sd)

set.seed(19012024)
res <- list()

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

# through all sample sizes
for(n in 1:length(names)){
  print(paste0("Sample size Scenario: ", names[n]))

  # data as list (one sample size scenario)
  data_list <- list()

  for(p in 1:length(pattern)){
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_bhmbasket.Rdata"))
    data_list[[p]] <- data
  }

  names(data_list) <- pattern

  # optimization
  res[[n]] <- opt_design(design = design_exnex, n = numbers[[n]],
                         alpha = alpha, design_params = design_params_exnex,
                         scenarios = scenarios, prec_digits = 3, iter = 10000,
                         data = data_list)

}

names(res) <- names

save(res, file = paste0(path, "Tuning Parameter/optParams_exnex.Rdata"))
write_xlsx(res, paste0(path, "Tuning Parameter/optParams_exnex.xlsx"))


