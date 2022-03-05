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

  real luce2_lpmf(int[] x,vector beta, real sigma,int R){
 real out = 0.0;
 vector[R] svec;
 //vector[R] pvec;
 

 
 svec = beta[x[1:R]]/sigma;
 
  //out += categorical_logit_lpmf(1|svec);

 for(i in 1:(R-1)){

   // pvec[i-1] = 0.0;
    //pvec[i:R] = softmax(svec[i:R]);
    
    out += categorical_logit_lpmf(1|svec[i:R]);
 }
  return out;
  
}




 int[] ranker(real[] beta,int N){
   int out[N];
   int med = 0;
   real b1;
   real b2;
   for(i in 1:N){
     med = 1;
     b1 = beta[i];
     if(i!=1){
     for(j in 1:(i-1)){
       b2 = beta[j];
       if(b1<b2){
         med += 1;
       }
       
     }
     }
     if(i != N){
     for(k in (i+1):N){
       b2 = beta[k];
       if(b1<b2){
         med += 1;
       }
     }
     
     }
     out[i] = med;
     
     
   }
   return out;
   
 }
 
 vector betas_remain(int R,vector betas, int[] excluded_ints, int n_rank, real sigma){
   
   vector[R] out;
   real denom;
   denom = 0.0;
   out = exp(betas/sigma);
   
   for(i in 1:(n_rank-1)){
     out[excluded_ints[i]] = 0.0;
   }
   

   out = out/sum(out);
   
   return out;
   
 }
 
 int[] luce_rng(int R, vector betas, real sigma){
   
   vector[R] probs;
   int win_integer[R];
   int out[R];
   
     for(i in 1:R){
    if(i!=1){
    probs = betas_remain(8,betas, win_integer[1:(i-1)],i,sigma);
    }else{
      probs = softmax(betas/sigma);
    }
   win_integer[i] =  categorical_rng(probs);
   out[win_integer[i]] = i;
  }
  
  
  return out; 
   
   
   
 }


}

data {
  int<lower=1> N; //the number of observations
  int<lower=1> P;// number of participants
  int x[N,8];// player id matrix. first column is first, second got second etc
  int nps[N];// number of participants in the race (7-8 in this data set)
  int n_types;// number of race types -e.g heats, semis, finals )
  int type[N]; // (ordered from most important to least - i.e finals =1, semis =2, heats =2)
  int finals[8]; // The integers of the athletes in the finals (you could supply any 8 athletes to simulate a race with those participants if desired)
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[(P-1)] beta;
  positive_ordered[(n_types-1)] sig_0;
}transformed parameters{
  
 vector[n_types] sigma;
 vector[P] beta2 = append_row(beta, 0);
 
 sigma[1] =1.0;
 
for(j in 2:n_types){
  sigma[j] = 1.0 + sig_0[(j-1)];
}
  
}
model{
  

 
for(i in 1:N){ 
target += luce2_lpmf(x[i,:]|beta2, sigma[type[i]], nps[i]);
}


  
 beta ~ normal(0,3);
 sig_0 ~ normal(0, 0.5);
 
}generated quantities{
  
 int posterior_latent_ranks[P];

 int replay_ranking[8];

 
 posterior_latent_ranks = ranker(to_array_1d(beta2),P);
 
 replay_ranking = luce_rng(8,beta2[finals],1);
  

  
}
