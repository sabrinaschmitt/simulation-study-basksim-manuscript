####################################################################
# Visualization of the CPP weights for different values of a and b #
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
library(ggh4x)

# setup CPP Design object
design <- setup_cpp(k = 2, p0 = 0.15)

# color selection
purple <- c("darkblue", "royalblue1", "skyblue", "#6D2077", "darkorchid1")


###################
## few variance  ##
###################

n <- c(25,30)
r_cur <- 18
r_comp <- 0:30


#----- b = 1 -----
b <- 1

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df1_1 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(1, 155))





#----- b = 2 -----
b <- 2

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df2_1 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(2, 155))




#----- b = 3 -----
b <- 3

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df3_1 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(3, 155))




#----- b = 4 -----
b <- 4

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df4_1 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(4, 155))





####################
## high variance  ##
####################

n <- c(10,30)
r_cur <- 8
r_comp <- c(0:30)


#----- b = 1 -----
b <- 1

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df1_2 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(1, 155))



#----- b = 2 -----
b <- 2

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df2_2 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(2, 155))




#----- b = 3 -----
b <- 3

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df3_2 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(3, 155))




#----- b = 4 -----
b <- 4

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df4_2 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(4, 155))





########################
## equal sample size  ##
########################

n <- c(30,30)
r_cur <- 15
r_comp <- c(0:30)


#----- b = 1 -----
b <- 1

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df1_3 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(1, 155))



#----- b = 2 -----
b <- 2

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df2_3 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(2, 155))




#----- b = 3 -----
b <- 3

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df3_3 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(3, 155))




#----- b = 4 -----
b <- 4

vec1<- c()
vec2<- c()
vec3 <- c()
vec4 <- c()
vec5 <- c()

# a = 1
a <- 1
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec1[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 2
a <- 2
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec2[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 3
a <- 3
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec3[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}

# a = 4
a <- 4
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec4[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


# a = 5
a <- 5
weight_mat <- get_weights_cpp(n = n, tune_a = a, tune_b = b)

for(i in 1:length(r_comp)){
  vec5[i] <- get_weight_mat_cpp(design, n = n, r = c(r_cur,r_comp[i]), weight_mat)[1,2]
}


df4_3 <- data.frame(Weights = c(vec1, vec2, vec3, vec4, vec5), r2 = rep(0:30,5),
                    a = as.factor(c(rep(1,31),rep(2,31),rep(3,31),rep(4,31),rep(5,31))),
                    b = rep(4, 155))





# ----- combined plot with all three scenarios -----

global_labeller <- labeller(
  b = c('1' = "b=1",'2' = "b=2",'3' = "b=3",'4' = "b=4"),
  n = c('1' = "n1 = 25, n2 = 30; r1 = 18", '2' = "n1 = 10, n2 = 30; r1 = 8", '3' = "n1 = 30, n2 = 30; r1 = 15")
)

# combining the small data frames
df_var_1 <- rbind(df1_1, df2_1, df3_1, df4_1)
df_var_2 <- rbind(df1_2, df2_2, df3_2, df4_2)
df_var_3 <- rbind(df1_3, df2_3, df3_3, df4_3)


var_df <- rbind(df_var_1 %>% mutate(n = rep(1,620)), df_var_2 %>% mutate(n = rep(2, 620)), df_var_3 %>% mutate(n = rep(3, 620)))


# plot
ggplot(data = var_df, mapping = aes(x = r2, y = Weights, group = a, color = a)) +
  geom_line() + scale_color_manual(values = purple) + theme_bw() +
  facet_grid2(n ~ b, labeller = global_labeller, axes = "x") + theme(text=element_text(size=18))


