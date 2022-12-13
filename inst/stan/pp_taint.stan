#include /include/license.stan

data {
  int<lower=1> S;
  int<lower=0> n;
  int<lower=1> s[n];
  real<lower=0, upper=1> t[n];
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
  real<lower=0> mu;
  real<lower=0> rho;
  vector<lower=0>[S] xi_s;
}
transformed parameters {
  vector<lower=0, upper=1>[S] theta_s;
  vector<lower=0>[S] kappa_s;
  theta_s = inv_logit(logit(theta) + sigma * alpha_s);
  kappa_s = mu + rho * xi_s;
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
  mu ~ normal(0, 100);
  rho ~ normal(0, 1);
  xi_s ~ normal(0, 1);
  if (use_likelihood) {
    for (i in 1:n) {
      t[i] ~ beta_proportion(theta_s[s[i]], kappa_s[s[i]]);
    }
  }
}
