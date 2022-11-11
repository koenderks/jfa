#include /include/license.stan

data {
  int<lower=1> S;
  int<lower=0> n;
  int<lower=1> s[n];
  real<lower=0> t[n];
  real<lower=0> priorx;
  real<lower=0> priorn;
  int betaprior;
}
parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> sigma;
  vector[S] alpha_s;
  real nu;
}
transformed parameters {
  real mu;
  vector<lower=0, upper=1>[S] phi;
  mu = logit(theta);
  phi = inv_logit(mu + sigma * alpha_s);
}
model {
  if (betaprior) {
    theta ~ beta(1 + priorx, priorn - priorx);
  } else {
    theta ~ gamma(1 + priorx, priorn);
  }
  sigma ~ normal(0, 1);
  alpha_s ~ normal(0, 1);
  nu ~ pareto(1, 1.5);
  for (i in 1:n) {
    t[i] ~ beta(phi[s[i]] * nu, (1 - phi[s[i]]) * nu);
  }
}
generated quantities {
  vector<lower=0, upper=1>[S] theta_s;
  for (i in 1:S) {
    theta_s[i] = inv_logit(mu + sigma * alpha_s[i]);
  }
}
