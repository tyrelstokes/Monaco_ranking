functions{
  real luce_lpmf(int[] x,vector beta, real sigma,int R){
 real out = 0.0;
 real denom = 0.0;
 
 for(i in 1:(R-1)){
   denom = 0.0;
   for(j in i:R){
     denom += exp(beta[x[j]]/sigma);
     
   }
   out += log(exp(beta[x[i]]/sigma)/denom);
 }
  return out;
  
}
}
data {
  int<lower=1> N; //the number of observations
  int<lower=1> P;// number of participants
  int x[N,8];// player id matrix. first column is first, second got second etc
  int real_ranks[P];
  real P1;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[P] beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  int R;
  R = 8;
 
 for(i in 1:N){
//y[i,:] ~ luce_lmpf(beta);

target += luce_lpmf(x[i,:]|beta,sigma,R);
}
  
 
 beta ~ normal(0,3);
 sigma ~ normal(0, 0.5);
 
}

