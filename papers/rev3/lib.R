sim1=function(vals){
  I=vals$I #ppl
  L=vals$L # reps
  J=1
  t.alpha=rnorm(I,.8,sqrt(vals$alpha.var))
  t.theta=rnorm(I,.06,sqrt(vals$theta.var))
  t.s2=vals$s2
  K=2
  N=I*J*K*L
  sub=rep(1:I,each=J*K*L)
  task=rep(rep(1:J,each=K*L),I)
  cond=rep(rep(1:K,each=L),I*J)
  subtask=cbind(sub,task)
  t.cell=t.alpha[subtask]+(cond-1)*t.theta[subtask]
  rt=rnorm(N,t.cell,sqrt(t.s2))
  dat=data.frame(sub,task,cond,rt)
  out=list(dat=dat,t.theta=t.theta)
  return(out)
}

sim2=function(vals,trueCor){
  I=vals$I #ppl
  L=vals$L # reps
  J=2
  alpha.var=diag(rep(vals$alpha.var,J))
  t.alpha=rmvnorm(I,rep(.8,J),alpha.var)
  t.theta=mvrnorm(I,
                  rep(.06,J),
                  matrix(ncol=2,
                         rep(vals$theta.var,4)*c(1,trueCor,trueCor,1)))
  t.s2=vals$s2
  K=2
  N=I*J*K*L
  sub=rep(1:I,each=J*K*L)
  task=rep(rep(1:J,each=K*L),I)
  cond=rep(rep(1:K,each=L),I*J)
  subtask=cbind(sub,task)
  t.cell=t.alpha[subtask]+(cond-1)*t.theta[subtask]
  rt=rnorm(N,t.cell,sqrt(t.s2))
  dat=data.frame(sub,task,cond,rt)
  out=list(dat=dat,t.theta=t.theta)
  return(out)
}

sim6=function(vals,weights){
  I=vals$I #ppl
  L=vals$L # reps
  J=6
  alpha.var=diag(rep(vals$alpha.var,J))
  t.alpha=rmvnorm(I,rep(.8,J),alpha.var)
  t.score=rnorm(I)
  t.theta.mu=rep(c(.05,.1),J/2)
  t.theta=t(outer(t.w,t.score)+rnorm(I*J,0,.01)+t.theta.mu)
  t.s2=vals$s2
  K=2
  N=I*J*K*L
  sub=rep(1:I,each=J*K*L)
  task=rep(rep(1:J,each=K*L),I)
  cond=rep(rep(1:K,each=L),I*J)
  subtask=cbind(sub,task)
  t.cell=t.alpha[subtask]+(cond-1)*t.theta[subtask]
  rt=rnorm(N,t.cell,sqrt(t.s2))
  dat=data.frame(sub,task,cond,rt)
  out=list(dat=dat,t.theta=t.theta)
  return(out)
}

spearman=function(dat){
  mrt=tapply(dat$rt,list(dat$sub,dat$task,dat$cond),mean)
  sample.effect=mrt[,,2]-mrt[,,1]
  se <- function(x) sd(x)/sqrt(length(x))
  sert <- tapply(dat$rt, list(dat$sub,dat$task,dat$cond), se)
  # (squared) standard error of difference in means:
  se2.diff <- colMeans(sert[,,2]^2 + sert[,,1]^2)
  cov.diff <- cov(sample.effect) - diag(se2.diff)
  return(cov2cor(cov.diff))
}



reliability=function(dat){
  if (mean(dat$cond %in% 1:2)<1) stop("Conditions must be 1 and 2")
  sub=as.integer(as.factor(dat$sub))
  I=max(sub)
  N=dim(dat)[1]
  mrt=tapply(dat$rt,list(dat$sub,dat$cond),mean)
  sample.effect=mrt[,2]-mrt[,1]
  se <- function(x) sd(x)/sqrt(length(x))
  sert <- tapply(dat$rt, list(dat$sub,dat$cond), se)
  se2.diff <- mean(sert[,2]^2 + sert[,1]^2)
  # (descriptive) variance of effect size estimates:
  var.diff <- var(sample.effect)
  # reliability of estimates:
  # = ( VAR(estimates) - SE(estimates)^2 ) / VAR(estimates)
  reliability <- (var.diff - se2.diff) / var.diff
  names(reliability)="rel"
  return(reliability)
}

split.rel=function(dat)
{
  if (mean(dat$cond %in% 1:2)<1) stop("Conditions must be 1 and 2")
  sub=as.integer(as.factor(dat$sub))
  I=max(sub)
  N=dim(dat)[1]
  set=rep(1:2,N/2)
  if (N%%2==1) set=c(set,1)
  mrt=tapply(dat$rt,list(dat$sub,set,dat$cond),mean)
  sample.effect=mrt[,,2]-mrt[,,1] 
  rho=cor(sample.effect)[1,2]
  out=2*rho/(rho+1)
  names(out)="split+pred"
  return(out)
}

genModWish2=function(dat,M=2000,b0=.03){
  if (mean(dat$cond %in% 1:2)<1) stop("Conditions must be 1 and 2")
  dat$sub=as.integer(as.factor(dat$sub))
  I=max(dat$sub)
  J=max(dat$task)
  N=dim(dat)[1]
  K=table(dat$sub,dat$task,dat$cond)
  mn=tapply(dat$rt,list(dat$sub,dat$task,dat$cond),mean)
  sd=tapply(dat$rt,list(dat$sub,dat$task,dat$cond),sd)
  x=tapply(dat$cond,list(dat$sub,dat$task,dat$cond),mean)-1
  
  theta=alpha=array(dim=c(M,I,J),0)
  s2=1:M
  muTheta=matrix(nrow=M,ncol=J)
  s2[1]=.25^2
  s2Alpha=1
  muAlpha=.8
  muTheta[1,]=rep(0,J)
  
  meanMuTheta=rep(.05,J)
  precMuTheta=diag(J)/.1^2
  B=array(dim=c(M,J,J))
  B[1,,]=diag(J)/.025^2
  XtX=K[,,2]
  
  a0=J
  for (m in 2:M){
    for (i in 1:I) {
      #alpha
      c=apply(K[i,,]*(mn[i,,]-x[i,,]*theta[m-1,i,]),1,sum)/s2[m-1]+muAlpha/s2Alpha
      v=1/(apply(K[i,,],1,sum)/s2[m-1]+1/s2Alpha)
      alpha[m,i,]=rnorm(J,c*v,sqrt(v))
      #theta
      Xty=K[i,,2]*(mn[i,,2]-alpha[m,i,])
      c=Xty/s2[m-1]+B[m-1,,]%*%muTheta[m-1,]
      v=solve(diag(XtX[i,])/s2[m-1]+B[m-1,,])
      theta[m,i,]=rmvnorm(1,v%*%c,v)
    }
    #s2
    scale=sum((K-1)*sd^2+K*((mn-outer(alpha[m,,],c(1,1))-x*outer(theta[m,,],c(1,1)))^2))/2+.5
    s2[m]=rinvgamma(1,shape=(N+1)/2,scale=scale)
    #muTheta
    v=solve(I*B[m-1,,]+precMuTheta)
    c=I*B[m-1,,]%*%apply(theta[m,,],2,mean)+precMuTheta%*%meanMuTheta
    muTheta[m,]=rmvnorm(1,v%*%c,v)
    #B
    err=t(t(theta[m,,])-muTheta[m,])
    SSE=crossprod(err)
    scale=solve(diag(rep(b0^2,J))  + SSE)
    B[m,,]=rWishart(1,a0+I,scale)
  }
  return(list(alpha=alpha,theta=theta,B=B))
}


genModOneTask=function(dat,M=2000,priors=priorOne){
  if (mean(dat$cond %in% 1:2)<1) stop("Conditions must be 1 and 2")
  sub=as.integer(as.factor(dat$sub))
  I=max(sub)
  N=dim(dat)[1]
  
  K=table(dat$sub,dat$cond)
  Kall=rowSums(K)
  mn=tapply(dat$rt,list(dat$sub,dat$cond),mean)
  sd=tapply(dat$rt,list(dat$sub,dat$cond),sd)
  
  theta=alpha=matrix(nrow=M,ncol=I,0)
  s2=1:M
  muTheta=s2Theta=1:M
  theta[1,]=rep(0,I)
  s2[1]=.3^2
  s2Alpha=1^2
  muAlpha=.8
  muTheta[1]=0
  muTheta.m=priors$mu.theta.m
  muTheta.v=priors$mu.theta.v
  s2Theta[1]=.2^2
  a=priors$a
  b=priors$b
  a0=priors$a0
  b0=priors$b0
  
  x=matrix(nrow=I,ncol=2)
  x[,1]=rep(0,I)
  x[,2]=rep(1,I)
  
  for (m in 2:M){
    #alpha
    c=apply(K*(mn-x*theta[m-1,]),1,sum)/s2[m-1]+muAlpha/s2Alpha
    v=1/(Kall/s2[m-1]+1/s2Alpha)
    alpha[m,]=rnorm(I,c*v,sqrt(v))
    #theta
    c=K[,2]*(mn[,2]-alpha[m,])/s2[m-1]+muTheta[m-1]/s2Theta[m-1]
    v=1/(K[,2]/s2[m-1]+1/s2Theta[m-1])
    theta[m,]=rnorm(I,c*v,sqrt(v))
    #s2
    scale=sum((K-1)*sd^2+K*(((mn-alpha[m,])-x*theta[m,])^2))/2+b  #note, scale not squared
    s2[m]=rinvgamma(1,shape=N/2+a,scale=scale)
    #muTheta
    v=1/(I/s2Theta[m-1]+1/muTheta.v)
    c=sum(theta[m,])/s2Theta[m-1]
    muTheta[m]=rnorm(1,v*c,sqrt(v))
    #s2Theta
    scale=sum((theta[m,]-muTheta[m])^2)/2+b0^2 #note, scale squared, b0 in seconds.
    s2Theta[m]=rinvgamma(1,shape=I/2+a0,scale=scale)
  }
  return(list(I=I,N=N,alpha=alpha,theta=theta,s2=s2,s2Theta=s2Theta))}

stanLkjC <- "
data {		
		int<lower=1> n;
    int<lower=1> I;
    int<lower=1> J; 
		vector[n] y;
    int<lower=0,upper=I> sub[n];
    int<lower=1,upper=2> cond[n];
    int<lower=1,upper=J> task[n];
	}
	
parameters {
		matrix[I,J] alpha;
    matrix[I,J] theta;
    vector[J] muTheta;
    cholesky_factor_corr[J] Lcorr;  
    vector<lower=0>[J] SigmaTheta;
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
    y ~ normal(obsMean,sigma);
    muTheta ~ normal(.06,.05);
		to_vector(alpha) ~ normal(.8,.3);
    for (i in 1:I){
      theta[i,]~multi_normal_cholesky(muTheta,diag_pre_multiply(SigmaTheta,Lcorr));}
  SigmaTheta ~ inv_gamma(2,.02^2);
  Lcorr ~ lkj_corr_cholesky(1);}

generated quantities {
  matrix[J,J] Omega;
  matrix[J,J] Sigma;
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(Omega, SigmaTheta); 
}"

