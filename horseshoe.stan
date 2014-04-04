data {
  int<lower=0> n;
  real y[n];
}

parameters{
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[n] theta;
  real<lower=0> lambda[n];
}

model {
  for (i in 1:n) {
    y[i] ~ normal(theta[i], sigma);
    theta[i] ~ normal(0, lambda[i]);
    lambda[i] ~ cauchy(0,tau)T[0,]; 
  }

  sigma ~ uniform(0,100);
  tau   ~ cauchy(0,sigma)T[0,];
}

generated quantities {
  real<lower=0,upper=1> kappa[n];
  for (i in 1:n)
    kappa[i] <- 1/(1+pow(lambda[i],2));
}

