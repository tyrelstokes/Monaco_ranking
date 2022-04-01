
### Some example data
x <- c(1,1,1,2,3,4,5,6,7,7,7,7,8,9,10,10,10,11,12,13,14,14,14) ## ranking

y <- c(1:length(x)) # the integers repressenting the items ids

permute_cap_vector <- c(5,3,2,1,1,1) ## how many max samples for the ith tie


#########################
#######################
## Function
library(foreach)
library(dplyr)

tie_permute_function <- function(x,y,permute_cap_vector){
  n_ties <- length(permute_cap_vector)
 x_unique <- unique(x) 
  j <- 1
  
 permuted_df <- foreach(i = 1:length(x_unique))%do%{
   
   
  x_i <- x[x == x_unique[i]]
  y_i <- y[x == x_unique[i]]
  
  if(length(x_i)==1){
    
    as.data.frame(rep(y_i,prod(permute_cap_vector))) ### Outputs a column no permutation
    
  }else{
  
    ### This means there was a tie
  perm_mat<- do.call(rbind,lapply(c(1:permute_cap_vector[j]),function(z){
    
    perm_sample <- sample(y_i,size = length(y_i)) ## Take a random permutation
  matrix(rep(perm_sample,prod(permute_cap_vector[-j])),
         ncol = length(y_i), byrow = TRUE)  ## This line multiplies it the right number of times
 
  
   

 }))
  
 
  j <- j +1 ## Each time there is a tie, we move one down for the max number of permutations
  
  perm_mat
  
  }
  
 }
out <- list(as.data.frame(permuted_df), weights = rep(1/nrow(permuted_df), nrow(permuted_df)))  
out ### Gives us the data.frame and the weights.
}




dd <- tie_permute_function(x,y,permute_cap_vector)[[1]]
