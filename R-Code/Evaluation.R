##################################################################
# Evaluation using rejection probabilities, FWER, mean and bias  #
##################################################################

# required packages
library(basksim)
library(dplyr)
library(xtable)
library(writexl)
library(doFuture)
library(progressr)


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

names <- c("linear", "grouped", "high_variance")

pattern <- c("Null","Alternative","Ascending","Descending","Big Good Nugget",
             "Small Good Nugget")

designs <- c("fujikawa", "cpp", "app", "cpplim", "bma", "bhm", "exnex")

# sample size scenarios
numbers <- matrix(c(10,15,20,25,30,
                    10,10,25,25,30,
                    10,10,10,20,50),
                  ncol = 5, byrow = TRUE)

# response rate scenarios
scenarios <- matrix(data = c(0.15, 0.15, 0.15, 0.15, 0.15,
                             0.35, 0.35, 0.35, 0.35, 0.35,
                             0.15, 0.15, 0.25, 0.35, 0.35,
                             0.35, 0.35, 0.25, 0.15, 0.15,
                             0.15, 0.15, 0.15, 0.15, 0.40,
                             0.40, 0.15, 0.15, 0.15, 0.15),
                    ncol=5, byrow = TRUE)


# ----- functions -----

# Functions to extract the optimal tuning parameters and lambda values for each
# design from the lists generated in tuning_parameter.R
extract_optParams <- function(res, design){
  design_params  <- list()

  if(design == "fujikawa"){
    for(n in 1:length(names)){
      design_params[[n]] <- list(epsilon = res[[n]][1,1], tau = res[[n]][1,2])
    }
  }else if(design == "cpp"){
    for(n in 1:length(names)){
      design_params[[n]] <- list(tune_a = res[[n]][1,1], tune_b = res[[n]][1,2])
    }
  }else if(design == "cpplim"){
    for(n in 1:length(names)){
      design_params[[n]] <- list(tune_a = res[[n]][1,1], tune_b = res[[n]][1,2])
    }
  }else if(design == "bma"){
    for(n in 1:length(names)){
      design_params[[n]] <- list(pmp0 = res[[n]][1,1])
    }
  }else if(design == "bhm"){
    for(n in 1:length(names)){
      design_params[[n]] <- list(tau_scale = res[[n]][1,1])
    }
  } else if(design == "exnex"){
    for(n in 1:length(names)){
      design_params[[n]] <- list(tau_scale = res[[n]][1,1], w = res[[n]][1,2])
    }
  } else if(design == "app"){
    design_params <- NULL
  }
  return(design_params)

}

extract_lambda <- function(res, design){
  lambda <- c()
  if(design == "fujikawa"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,3]
    }
  } else if(design == "cpp"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,3]
    }
  } else if(design == "app"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,1]
    }
  } else if(design == "cpplim"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,3]
    }
  } else if(design == "bma"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,2]
    }
  } else if(design == "bhm"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,2]
    }
  } else if(design == "exnex"){
    for(n in 1:length(names)){
      lambda[n] <- res[[n]][1,3]
    }
  }
  return(lambda)
}



# ----- Model Parameters fixed -----
# beta prior: Beta(1,1) for fujikawa, cpp, app, cpplim, bma
shape1 <- 1
shape2 <- 1

# fixed prior distribution for mu (bhm + exnex)
mu_mean_bhm <- round(log(h0/(1-h0)) - log(h1/(1-h1)), digits = 4)
mu_mean_exnex <- round(log(h0/(1-h0)), digits = 4)
mu_sd <- 100

# ----- setup all designs -----
design_fujiukawa <- setup_fujikawa(k = K, p0 = h0, shape1 = shape1,
                                  shape2 = shape2)
design_cpp <- setup_cpp(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_app <- setup_app(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_cpplim <- setup_cpplim(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_bma <- setup_bma(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_bhm <- setup_bhm(k = K, p0 = h0, p_target = h1, mu_mean = mu_mean_bhm,
                        mu_sd = mu_sd)
design_exnex <- setup_exnex(k = K, p0 = h0, basket_mean = mu_mean_exnex,
                            basket_sd = mu_sd, mu_mean = mu_mean_exnex,
                            mu_sd = mu_sd)






# ----- Extract design_params + lambda -----

# fujikawa
load(paste0(path, "Tuning Parameter/optParams_fujikawa.Rdata"))
design_params_fujikawa <- extract_optParams(res = res, design = "fujikawa")
lambda_fujikawa <- extract_lambda(res = res, design = "fujikawa")

# cpp
load(paste0(path, "Tuning Parameter/optParams_cpp.Rdata"))
design_params_cpp <- extract_optParams(res = res, design = "cpp")
lambda_cpp <- extract_lambda(res = res, design = "cpp")

# app
load(paste0(path, "Tuning Parameter/optParams_app.Rdata"))
design_params_app <- NULL
lambda_app <- extract_lambda(res = res, design = "app")

# cpplim
load(paste0(path, "Tuning Parameter/optParams_cpplim.Rdata"))
design_params_cpplim <- extract_optParams(res = res, design = "cpplim")
lambda_cpplim <- extract_lambda(res = res, design = "cpplim")

# bma
load(paste0(path, "Tuning Parameter/optParams_bma.Rdata"))
design_params_bma <- extract_optParams(res = res, design = "bma")
lambda_bma <- extract_lambda(res = res, design = "bma")

# bhm
load(paste0(path, "Tuning Parameter/optParams_bhm.Rdata"))
design_params_bhm <- extract_optParams(res = res, design = "bhm")
lambda_bhm <- extract_lambda(res = res, design = "bhm")

# exnex
load(paste0(path, "Tuning Parameter/optParams_exnex.Rdata"))
design_params_exnex <- extract_optParams(res = res, design = "exnex")
lambda_exnex <- extract_lambda(res = res, design = "exnex")






# ----- Evaluation -----

# Calculation of the FWERs and the rejection rates, posterior means and bias
# values of the baskets for all patterns within each scenario. Matrices are
# generated containing the results of all designs (listed in the rows).

set.seed(29042024)

registerDoFuture()
plan(multisession, workers = 5)
handlers(global = TRUE)

for(n in 1:length(names)){
  for(p in 1:length(pattern)){
    # first matrix data
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_matrix.Rdata"))
    print(paste0("Pattern: ", pattern[p], "; Sample size scenario: ", names[n], "; Matrix Data"))

    # empty matrices to store the results
    results <- matrix(data = NA, length(designs), K+1)
    mean <- matrix(data = NA, length(designs), K)
    bias <- matrix(data = NA, length(designs), K)


    # fujikawa
    res_fujikawa <- get_details(design_fujiukawa, n = attr(data, "n"), p1 = attr(data, "p"),
                                lambda = lambda_fujikawa[n], epsilon = design_params_fujikawa[[n]]$epsilon, # n -> sample size scenario
                                tau = design_params_fujikawa[[n]]$tau, logbase = 2, iter = 10000,
                                data = data)

    results[1, 1:5] <- res_fujikawa$Rejection_Probabilities
    results[1, 6] <- res_fujikawa$FWER
    mean[1, ] <- res_fujikawa$Mean
    bias[1, ] <- abs(scenarios[p,] - res_fujikawa$Mean)

    # cpp
    res_cpp <- get_details(design_cpp,  n = attr(data, "n"), p1 = attr(data, "p"),
                           lambda = lambda_cpp[n], tune_a = design_params_cpp[[n]]$tune_a,
                           tune_b = design_params_cpp[[n]]$tune_b, iter = 10000,
                           data = data)

    results[2, 1:5] <- res_cpp$Rejection_Probabilities
    results[2, 6] <- res_cpp$FWER
    mean[2, ] <- res_cpp$Mean
    bias[2, ] <- abs(scenarios[p,] - res_cpp$Mean)

    # app
    res_app <- get_details(design_app, n = attr(data, "n"), p1 = attr(data, "p"),
                           lambda = lambda_app[n], iter = 10000, data = data)

    results[3, 1:5] <- res_app$Rejection_Probabilities
    results[3, 6] <- res_app$FWER
    mean[3, ] <- res_app$Mean
    bias[3, ] <- abs(scenarios[p,] - res_app$Mean)

    # cpplim
    res_cpplim <- get_details(design_cpplim, n = attr(data, "n"), p1 = attr(data, "p"),
                              lambda = lambda_cpplim[n], tune_a = design_params_cpplim[[n]]$tune_a,
                              tune_b = design_params_cpplim[[n]]$tune_b, iter = 10000,
                              data = data)

    results[4, 1:5] <- res_cpplim$Rejection_Probabilities
    results[4, 6] <- res_cpplim$FWER
    mean[4, ] <- res_cpplim$Mean
    bias[4, ] <- abs(scenarios[p,] - res_cpplim$Mean)

    # bma
    res_bma <- get_details(design_bma, n = attr(data, "n"), p1 = attr(data, "p"),
                           lambda = lambda_bma[n], pmp0 = design_params_bma[[n]]$pmp0,
                           iter = 10000, data = data)

    results[5, 1:5] <- res_bma$Rejection_Probabilities
    results[5, 6] <- res_bma$FWER
    mean[5, ] <- res_bma$Mean
    bias[5, ] <- abs(scenarios[p,] - res_bma$Mean)

    # load bhmbasket data
    load(paste0(path, "Data/", pattern[p], "_", names[n], "_bhmbasket.Rdata"))
    print(paste0("Pattern: ", pattern[p], "; Sample size scenario: ", names[n], "; bhmbasket Data"))

    # bhm
    res_bhm <- get_details(design_bhm, n = data$scenario_1$n_subjects[1,],
                           p1 = data$scenario_1$response_rates[1,], lambda = lambda_bhm[n],
                           tau_scale = design_params_bhm[[n]]$tau_scale, iter = 10000,
                           data = data)

    results[6, 1:5] <- res_bhm$Rejection_Probabilities
    results[6, 6] <- res_bhm$FWER
    mean[6, ] <- res_bhm$Mean
    bias[6, ] <- abs(scenarios[p,] - res_bhm$Mean)

    # exnex
    res_exnex <- get_details(design_exnex, n = data$scenario_1$n_subjects[1,],
                             p1 = data$scenario_1$response_rates[1,], lambda = lambda_exnex[n],
                             tau_scale = design_params_exnex[[n]]$tau_scale,
                             w = design_params_exnex[[n]]$w, iter = 10000, data = data)

    results[7, 1:5] <- res_exnex$Rejection_Probabilities
    results[7, 6] <- res_exnex$FWER
    mean[7, ] <- res_exnex$Mean
    bias[7, ] <- abs(scenarios[p,] - res_exnex$Mean)



    # rename columns and rows
    colnames(results) <- c("Basket 1", "Basket 2", "Basket 3", "Basket 4", "Basket 5", "FWER")
    rownames(results) <- c("Fujikawa", "CPP", "APP", "LCPP", "BMA", "BHM", "EXNEX")

    colnames(mean) <- c("Basket 1", "Basket 2", "Basket 3", "Basket 4", "Basket 5")
    rownames(mean) <- c("Fujikawa", "CPP", "APP", "LCPP", "BMA", "BHM", "EXNEX")

    colnames(bias) <- c("Basket 1", "Basket 2", "Basket 3", "Basket 4", "Basket 5")
    rownames(bias) <- c("Fujikawa", "CPP", "APP", "LCPP", "BMA", "BHM", "EXNEX")


    # Rdata
    save(results, file = paste0(path, "Results/", names[n], "/results/results_", pattern[p], "_", names[n],".Rdata"))
    save(mean, file = paste0(path, "Results/", names[n], "/mean/mean_", pattern[p], "_", names[n],".Rdata"))
    save(bias, file = paste0(path, "Results/", names[n], "/bias/bias_", pattern[p], "_", names[n],".Rdata"))

    # Excel
    write_xlsx(as.data.frame(results), paste0(path, "Results/", names[n], "/results/results_", pattern[p], "_", names[n],".xlsx"))
    write_xlsx(as.data.frame(mean), paste0(path, "Results/", names[n], "/mean/mean_", pattern[p], "_", names[n],".xlsx"))
    write_xlsx(as.data.frame(bias), paste0(path, "Results/", names[n], "/bias/bias_", pattern[p], "_", names[n],".xlsx"))

    # Latex
    print(xtable(results, type = "latex", digits = 4), file = paste0(path, "Results/", names[n], "/results/results_", pattern[p], "_", names[n],".tex"), include.rownames = TRUE)
    print(xtable(mean, type = "latex", digits = 4), file = paste0(path, "Results/", names[n], "/mean/mean_", pattern[p], "_", names[n],".tex"), include.rownames = TRUE)
    print(xtable(bias, type = "latex", digits = 4), file = paste0(path, "Results/", names[n], "/bias/bias_", pattern[p], "_", names[n],".tex"), include.rownames = TRUE)


  }

}






