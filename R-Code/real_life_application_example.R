#########################################################################
# Real-life application example based the data from Hyman et al. (2015) #
#########################################################################

# library(basksim)
library(xtable)
library(writexl)

# ----- setup -----
# data from Hyman et al. (2015)
K <- 6
h0 <- 0.15
h1 <- 0.35

n <- c(19, 10, 26, 8, 14, 7)
r <- c(8, 0, 1, 1, 6, 2)

baskets <- c("NSCLC", "Colorectal Cancer 1", "Colorectal Cancer 2", "Cholangiocarcinoma", "ECD or LCH", "Anaplastic Thyroid Cancer")

# scenarios
scenario <- c("linear", "grouped", "high_variance")
s <- 1 # linear
# s <- 2 # grouped
# s <- 3 # high variance

## fixed model parameters
# beta prior: Beta(1,1) for fujikawa, cpp, app, cpplim, bma
shape1 <- 1
shape2 <- 1

# fixed prior distribution for mu (bhm + exnex)
mu_mean_bhm <- round(log(h0/(1-h0)) - log(h1/(1-h1)), digits = 4)
mu_mean_exnex <- round(log(h0/(1-h0)), digits = 4)
mu_sd <- 100

## tuning parameters of the linear scenario
# CPP
cpp_a <- c(4, 4, 4)
cpp_b <- c(4.5, 4.5, 4)

# CPPlim
cpplim_a <- c(3, 3, 2.5)
cpplim_b <- c(4, 4.5, 5)

# fujikawa
epsilon <- c(1.5, 1.5, 2.5)
tau <- c(0.2, 0, 0.2)

# BMA
pmp0 <- c(-2, -2, -2)

# BHM + EXNEX
tau_scale <- c(0.661, 0.661, 0.661)

# EXNEX
w <- c(0.9, 0.9, 0.8)

# lambda values
lambda_cpp <- c(0.991, 0.992, 0.993)
lambda_app <- c(0.986, 0.986, 0.986)
lambda_cpplim <- c(0.987, 0.988, 0.984)
lambda_fujikawa <- c(0.997, 0.996, 0.997)
lambda_bma <- c(0.965, 0.964, 0.963)
lambda_bhm <- c(0.955, 0.954, 0.952)
lambda_exnex <- c(0.955, 0.957, 0.955)

# create all design objects
design_cpp <- setup_cpp(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_app <- setup_app(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_cpplim <- setup_cpplim(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_fujiukawa <- setup_fujikawa(k = K, p0 = h0, shape1 = shape1,
                                   shape2 = shape2)
design_bma <- setup_bma(k = K, p0 = h0, shape1 = shape1, shape2 = shape2)
design_bhm <- setup_bhm(k = K, p0 = h0, p_target = h1, mu_mean = mu_mean_bhm,
                        mu_sd = mu_sd)
design_exnex <- setup_exnex(k = K, p0 = h0, basket_mean = mu_mean_exnex,
                            basket_sd = mu_sd, mu_mean = mu_mean_exnex,
                            mu_sd = mu_sd)

# ----- evaluation -----
set.seed(20250212)

res_cpp <- get_evaluation.cpp(design = design_cpp, n = n, r = r, lambda = lambda_cpp[s], tune_a = cpp_a[s], tune_b = cpp_b[s])
res_cpplim <- get_evaluation.cpplim(design = design_cpplim, n = n, r = r, lambda = lambda_cpplim[s], tune_a = cpplim_a[s], tune_b = cpplim_b[s])
res_app <- get_evaluation.app(design = design_app, n = n, r = r, lambda = lambda_app[s])
res_fujikawa <- get_evaluation.fujikawa(design = design_fujiukawa, n = n, r = r, lambda = lambda_fujikawa[s], epsilon = epsilon[s], tau = tau[s])
res_bma <- get_evaluation.bma(design = design_bma, n = n, r = r, lambda = lambda_bma[s], pmp0 = pmp0[s])
res_bhm <- get_evaluation.bhm(design = design_bhm, n = n, r = r, lambda = lambda_bhm[s], tau_scale = tau_scale[s])
res_exnex <- get_evaluation.exnex(design = design_exnex, n = n, r = r, lambda = lambda_exnex[s], tau_scale = tau_scale[s], w = w[s])


## Posterior Probabilities from the bhmbasket package

trial_data <- bhmbasket:::createTrial(
  n_subjects = n,
  n_responders = r
)

# BHM
bhm_prior_parameters = bhmbasket::setPriorParametersBerry(
  mu_mean = design_bhm$mu_mean,
  mu_sd = design_bhm$mu_sd,
  tau_scale = tau_scale[s]
)

set.seed(20250212)
debug(bhmbasket:::getPosteriors)

bhmbasket::performAnalyses(
  scenario_list = trial_data,
  method_names = "berry",
  prior_parameters_list = bhm_prior_parameters,
  target_rates = rep(h1, K)
)

# Execute in debug mode after jags_fit has been created:
colMeans(jags_fit$BUGSoutput$sims.list$p > 0.15)

# stop debug mode
res_bhm$Posterior_Probabilities <- c(0.9907755, 0.2569372, 0.1445178, 0.5157492, 0.9786261, 0.7672116) # linear
# res_bhm$Posterior_Probabilities <- c(0.9907755, 0.2569372, 0.1445178, 0.5157492, 0.9786261, 0.7672116) # grouped
# res_bhm$Posterior_Probabilities <- c(0.9907755, 0.2569372, 0.1445178, 0.5157492, 0.9786261, 0.7672116) # high variance

# EXNEX
exnex_prior_parameters <- bhmbasket::setPriorParametersExNex(
  mu_mean = design_exnex$mu_mean,
  mu_sd = design_exnex$mu_sd,
  tau_scale = tau_scale[s],
  mu_j = rep(design_exnex$basket_mean, design_exnex$k),
  tau_j = rep(design_exnex$basket_sd, design_exnex$k),
  w_j = w[s]
)

set.seed(20250212)
debug(bhmbasket:::getPosteriors)

bhmbasket::performAnalyses(
  scenario_list = trial_data,
  method_names = "exnex",
  prior_parameters_list = exnex_prior_parameters,
  target_rates = rep(h1, K)
)

# Execute in debug mode after jags_fit has been created:
colMeans(jags_fit$BUGSoutput$sims.list$p > 0.15)

# stop debug mode
res_exnex$Posterior_Probabilities <- c(0.9919754, 0.1903405, 0.1797660, 0.5542223, 0.9810259, 0.7947353) # linear
# res_exnex$Posterior_Probabilities <- c(0.9919754, 0.1903405, 0.1797660, 0.5542223, 0.9810259, 0.7947353) # grouped
# res_exnex$Posterior_Probabilities <- c(0.9913754, 0.1371681, 0.1858407, 0.5777711, 0.9814009, 0.8036598) # high variance



# creating results table
estimates <- data.frame(baskets, "r/n" = paste0(r, "/", n), p = r/n, CPP = res_cpp$Estimates, LCPP = res_cpplim$Estimates,
                        APP = res_app$Estimates, Fujikawa = res_fujikawa$Estimates, BMA = res_bma$Estimates, BHM = res_bhm$Estimates,
                        EXNEX = res_exnex$Estimates)

post_probs <- data.frame(baskets, "r/n" = paste0(r, "/", n), p = r/n, CPP = res_cpp$Posterior_Probabilities, LCPP = res_cpplim$Posterior_Probabilities,
                         APP = res_app$Posterior_Probabilities, Fujikawa = res_fujikawa$Posterior_Probabilities, BMA = res_bma$Posterior_Probabilities,
                         BHM = res_bhm$Posterior_Probabilities, EXNEX = res_exnex$Posterior_Probabilities)


# ----- save results -----
# Rdata
save(estimates, file = paste0("/Users/sabrinaschmitt/Documents/R/results_hyman/responseRates_", scenario[s], ".Rdata"))
save(post_probs, file = paste0("/Users/sabrinaschmitt/Documents/R/results_hyman/posteriorProbabilities_", scenario[s], ".Rdata"))

# Excel
write_xlsx(as.data.frame(estimates), paste0("/Users/sabrinaschmitt/Documents/R/results_hyman/responseRates_", scenario[s], ".xlsx"))
write_xlsx(as.data.frame(post_probs), paste0("/Users/sabrinaschmitt/Documents/R/results_hyman/posteriorProbabilities_", scenario[s], ".xlsx"))

# Latex
print(xtable(estimates, type = "latex", digits = 3), file = paste0("/Users/sabrinaschmitt/Documents/R/results_hyman/responseRates_", scenario[s], ".tex"), include.rownames = TRUE)
print(xtable(post_probs, type = "latex", digits = 3), file = paste0("/Users/sabrinaschmitt/Documents/R/results_hyman/posteriorProbabilities_", scenario[s], ".tex"), include.rownames = TRUE)


