####################################################################
# Visualization of the JSD weights for different values of epsilon #
# for three different scenarios:                                   #
#   1. few variance:      n1 = 25, n2 = 30, r1 = 18                #
#   2. high variance:     n1 = 10, n2 = 30, r1 = 8                 #
#   3. equal sample size: n1 = 30, n2 = 30, r1 = 15                #
####################################################################

# required packages
library(basksim)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)

# Setup Fujikawa Design Object
design <- setup_fujikawa(k = 2, p0 = 0.15)


# color selection
purple01 <- c("darkblue", "royalblue1", "skyblue", "#6D2077", "darkorchid1", "palevioletred1")




###################
## few variance  ##
###################

n <- c(25,30)
r_cur <- 18
r_comp <- 0:30



vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()
vec6 <- c()

# epsilon = 0.5
epsilon = 0.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# epsilon = 1
epsilon = 1

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# epsilon = 1.5
epsilon = 1.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 2
epsilon = 2

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 2.5
epsilon = 2.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 3
epsilon = 3

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec6[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df1 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5, vec6), r2 = rep(0:30,6),
                  Epsilon = as.factor(c(rep(0.5,31),rep(1,31),rep(1.5,31),rep(2,31),rep(2.5,31),rep(3,31))), n = rep(1,186))




####################
## high variance  ##
####################

n <- c(10,30)
r_cur <- 8
r_comp <- 0:30



vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()
vec6 <- c()

# epsilon = 0.5
epsilon = 0.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# epsilon = 1
epsilon = 1

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# epsilon = 1.5
epsilon = 1.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 2
epsilon = 2

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 2.5
epsilon = 2.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 3
epsilon = 3

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec6[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df2 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5, vec6), r2 = rep(0:30,6),
                  Epsilon = as.factor(c(rep(0.5,31),rep(1,31),rep(1.5,31),rep(2,31),rep(2.5,31),rep(3,31))), n = rep(2,186))




########################
## equal sample size  ##
########################

n <- c(30,30)
r_cur <- 15
r_comp <- 0:30



vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()
vec6 <- c()

# epsilon = 0.5
epsilon = 0.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# epsilon = 1
epsilon = 1

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# epsilon = 1.5
epsilon = 1.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 2
epsilon = 2

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 2.5
epsilon = 2.5

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# epsilon = 3
epsilon = 3

weight_mat <- get_weights_jsd(design, n = n, epsilon = epsilon, tau = 0, logbase = 2)

for(i in 1:length(r_comp)){
  vec6[i] <- get_weight_mat_jsd(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df3 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5, vec6), r2 = rep(0:30,6),
                  Epsilon = as.factor(c(rep(0.5,31),rep(1,31),rep(1.5,31),rep(2,31),rep(2.5,31),rep(3,31))), n = rep(3,186))



# ----- combined plot with all three scenarios -----

global_labeller <- labeller(
  n = c('1' = "n1 = 25, n2 = 30; r1 = 18", '2' = "n1 = 10, n2 = 30; r1 = 8", '3' = "n1 = 30, n2 = 30; r1 = 15")
)

# combined data frame
df_comb <- rbind(df1, df2, df3)




# plot
ggplot(data = df_comb, mapping = aes(x = r2, y = Weights, group = Epsilon, color = Epsilon)) +
  geom_line() + scale_color_manual(values = purple01) + theme_bw() +
  facet_wrap(. ~ n, labeller = global_labeller, scales = 'free', ncol = 1, strip.position = c("right")) +
  theme(text=element_text(size=18))
