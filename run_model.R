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

fints <- stan_dt$finals

#########################

library(cmdstanr)

mod <- cmdstan_model("plackett_luce.stan")


fit <- mod$sample(data = stan_dt,iter_warmup = 2000, iter_sampling = 2000,parallel_chains = 4)


#############################
################################

### Collect information and make some plots


cv <- fit$summary()


chc2 <- fit$summary("posterior_latent_ranks")

post_data <- data.frame(Name = athletes,rank = chc2$mean, Rl = chc2$q5,
                        Ru = chc2$q95)

fdata <- post_data[fints,] ## Rank information for just those in the finals


##########################################
#########################################

### This gets you the finals data

finals_draws <- fit$draws("replay_ranking", format = "df")

# Reformat this to work well with ggplot2
library(foreach)

fdata <- foreach(i = 1:8, .combine = rbind)%do%{
  
 out <-  data.frame(Name = athletes[fints[i]], Rank = as.vector(finals_draws[,i]))
  
  names(out)[2] <- "Rank"
  out
}


library(ggplot2)
library(ggthemes)

ggplot(fdata,aes(x= Name,y= Rank))+ geom_boxplot(aes(color = Name)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Estimated Ranks", 
       subtitle="Replaying the Olympic 2012 Finals",
       x="Finalist Name",
       y="Rank") + coord_flip() +theme_fivethirtyeight()

#############################################
################################################

## This gets you the ranking plot amoung all participants
overall_rank_draws <- fit$draws("posterior_latent_ranks", format = "df")


odata <- foreach(i = 1:8, .combine = rbind)%do%{
  
  out <-  data.frame(Name = athletes[fints[i]], Rank = as.vector(overall_rank_draws[,fints[i]]))
  
  names(out)[2] <- "Rank"
  out
}


ggplot(odata,aes(x= Name,y= Rank))+ geom_boxplot(aes(color = Name)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Estimated Ranks", 
       subtitle=" Amoung 87 Participants in WC2011 + 2012 Olympics",
       x="Finalist Name",
       y="Rank") + coord_flip() +theme_fivethirtyeight()+scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90))


#######################################################
#########################################################
## Finals heat map rank
library(janitor)
dd <- tabyl(fdata,Name, Rank)

rtab <- dd
rtab[,2:9] <- rtab[,2:9]/(nrow(fdata)/8)

rtab2 <- data.frame(Real_Result = c(8,6,3,7,5,4,1,2),rtab[,1:9])
names(rtab2)[3:ncol(rtab2)] <- c(1:8)

rtab2$Medal <- apply(rtab2[,3:5],1,sum)

names(rtab2)[1] <- "Actual Result"

library(gt)
library(scales)
library(readr)
library(dplyr)

rtab2 %>% arrange(`Actual Result`) %>% mutate_if(is.numeric,~round(.,2))%>% gt() %>% 
  tab_spanner(label = "Predicted Ranks",
              columns = c(3:10) )%>%
  data_color(columns = 3:10,
             colors = col_numeric(palette = c("white","firebrick"),
                                  domain = c(0,1))) %>%
  data_color(columns = 11,
             colors = col_numeric(palette = c("white","gold"),
                                  domain = c(0,1)))












