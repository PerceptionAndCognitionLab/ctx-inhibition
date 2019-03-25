makeStanWish=function(){
  stanWishC <- "
    data {		
      int<lower=1> n;
      int<lower=1> I;
      int<lower=1> J; 
      vector[n] y;
      int<lower=0,upper=I> sub[n];
      int<lower=1,upper=2> cond[n];
      int<lower=1,upper=J> task[n];
      real<lower=0> scle;
    }

    parameters {
      matrix[I,J] alpha;
      matrix[I,J] theta;
      vector[J] muTheta;
      cov_matrix[J] BSigma;
      real<lower=0> sigma2;
    }

    transformed parameters {
      real<lower=0> sigma;
      vector[n] obsMean;
      for (k in 1:n){
        obsMean[k]=alpha[sub[k],task[k]]+(cond[k]-1)*theta[sub[k],task[k]];}
      sigma=sqrt(sigma2);
    }

    model {
      matrix[J,J] identity;
      identity = diag_matrix(rep_vector(1,J)); 
      muTheta ~ normal(.06,.05);
      BSigma ~ inv_wishart(J,identity*scle*scle);
      to_vector(alpha) ~ normal(.8,.3);
      for (i in 1:I){
        theta[i,] ~ multi_normal(muTheta,BSigma);}
      sigma2 ~ inv_gamma(.1,.1);
      y ~ normal(obsMean,sigma);
    }"
  stanWishM <- stan_model(model_code = stanWishC)
  return(stanWishM)
}

runStanWish=function(stanWishM,dat,iter=400,chains=1,warmup=200){
  stanData <- list(y=dat$rt, 
                 n=length(dat$sub), I=typical$I,J=2, 
                 sub=dat$sub, cond=dat$cond, task=dat$task,
                 scle=0.05)
  samples <- sampling(stanWishM, data=stanData,
                      iter=iter,chains=chains,warmup=warmup)
  return(samples)
}