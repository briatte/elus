#==============================================================================
#
# 07_stage1_model.r -- first stage of Bayesian Spatial Following model
#
# Fits the first stage of the model presented in Pablo Barberá's paper "Birds of
# the Same Feather Tweet Together: Bayesian Ideal Point Estimation Using Twitter
# Data", Political Analysis (2015) 23:76–91.
#
# Author: Pablo Barbera
# Source: https://github.com/pablobarbera/twitter_ideology/tree/master/code
#
#==============================================================================

library(rstan)

options(scipen = 20)

# load likes matrix
load("model/matrix_selected.rda")

# parameters for Stan model
n.iter = 250
n.warmup = 100
thin = 1

# show sample
cat(date(), ": fitting", nrow(y), "x", ncol(y), "matrix...\n")
timing = Sys.time()

J = dim(y)[1]
K = dim(y)[2]
N = J * K
jj = rep(1:J, times = K)
kk = rep(1:K, each = J)

stan.code = '
data {
  int<lower=1> J; // number of twitter users
  int<lower=1> K; // number of elite twitter accounts
  int<lower=1> N; // N = J x K
  int<lower=1,upper=J> jj[N]; // twitter user for observation n
  int<lower=1,upper=K> kk[N]; // elite account for observation n
  int<lower=0,upper=1> y[N]; // dummy if user i follows elite j
}
parameters {
  vector[K] alpha;
  vector[K] phi;
  vector[J] theta;
  vector[J] beta;
  real mu_beta;
  real<lower=0.1> sigma_beta;
  real mu_phi;
  real<lower=0.1> sigma_phi;
  real gamma;
}
model {
  alpha ~ normal(0, 1);
  beta ~ normal(mu_beta, sigma_beta);
  phi ~ normal(mu_phi, sigma_phi);
  theta ~ normal(0, 1);
  for (n in 1:N)
    y[n] ~ bernoulli_logit( alpha[kk[n]] + beta[jj[n]] -
      gamma * square( theta[jj[n]] - phi[kk[n]] ) );
}
'

stan.data = list(J = J, K = K, N = N, jj = jj, kk = kk, y = c(y))
colK = colSums(y)
rowJ = rowSums(y)

normalize = function(x){

  (x-mean(x))/sd(x)

}

## RUNNING MODEL

inits = list(list(alpha = normalize(log(colK + 0.0001)),
                   beta = normalize(log(rowJ + 0.0001)),
                   theta = rep(0, J), phi = start.phi,
                   mu_beta = 0,
                   sigma_beta = 1,
                   gamma = 2,
                   mu_phi = 0,
                   sigma_phi = 1))

stan.fit = stan(model_code = stan.code,
                 data = stan.data,
                 iter = n.iter,
                 warmup = n.warmup,
                 chains = 1,
                 thin = 1,
                 init = inits,
                 seed = 83398)

# save results with initial matrix
save(y, stan.fit, file = "model/stanfit.rda")

cat(date(), ": finished.\n")
print(Sys.time() - timing)

rm(list = ls())
gc()
