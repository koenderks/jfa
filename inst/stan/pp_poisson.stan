#include /include/license.stan

data {
  int<lower=1> S;
  int<lower=1> n[S];
  int<lower=0> k[S];
  real<lower=0> alpha;
  real<lower=0> beta;
  int beta_prior;
  int gamma_prior;
  int normal_prior;
  int uniform_prior;
  int cauchy_prior;
  int t_prior;
  int chisq_prior;
  int use_likelihood;
}
parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> sigma;
  vector[S] alpha_s;
}
transformed parameters {
  real mu;
  mu = logit(theta);
}
model {
  if (beta_prior) {
    theta ~ beta(alpha, beta);
  } else if (gamma_prior) {
    theta ~ gamma(alpha, beta);
  } else if (normal_prior) {
    theta ~ normal(alpha, beta);
  } else if (uniform_prior) {
    theta ~ uniform(alpha, beta);
  } else if (cauchy_prior) {
    theta ~ cauchy(alpha, beta);
  } else if (t_prior) {
    theta ~ student_t(alpha, 0, 1);
  } else if (chisq_prior) {
    theta ~ chi_square(alpha);
  }
  sigma ~ normal(0, 1);
  alpha_s ~ normal(0, 1);
  if (use_likelihood) {
    for (i in 1:S){
      k[i] ~ poisson(inv_logit(mu + sigma * alpha_s[i]) * n[i]);
    }
  }
}
generated quantities {
  vector<lower=0, upper=1>[S] theta_s;
  for (i in 1:S) {
    theta_s[i] = inv_logit(mu + sigma * alpha_s[i]);
  }
}
