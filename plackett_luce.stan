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
  int nps[N];
  int n_types;// number of race types -e.g heats, semis, finals
  int type[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[P] beta;
  positive_ordered[(n_types-1)] sig_0;
}transformed parameters{
  
 vector[n_types] sigma;
 
 sigma[1] =1.0;
 
for(j in 2:n_types){
  sigma[j] = 1.0 + sig_0[(j-1)];
}
  
}
model {
  

 
 for(i in 1:N){
//y[i,:] ~ luce_lmpf(beta);

target += luce_lpmf(x[i,:]|beta,sigma[type[i]],nps[i]);
}
  
 
 beta ~ normal(0,3);
 sig_0 ~ normal(0, 0.5);
 
}

