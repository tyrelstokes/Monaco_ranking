######################
## Load the data list

race_list <- readRDS("race_data_list.Rds")

stan_dt <- race_list[[1]]

x <- stan_dt$x
x <- apply(x,2,as.integer)

stan_dt$x <- x

stan_dt$n_types <- 3

stan_dt$type <- plyr::mapvalues(stan_dt$type,from = c(1:3),to = c(3:1))

rank_data <- race_list[[2]]

athletes <- race_list[[3]]


#########################

library(cmdstanr)

mod <- cmdstan_model("plackett_luce.stan")


fit <- mod$sample(data = stan_dt,iter_warmup = 1200, iter_sampling = 1200,parallel_chains = 4)


fit$cmdstan_summary()
