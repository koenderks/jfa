#include /include/license.stan

data {
  int<lower=1> S;                      // number of strata
  int<lower=0> n;                      // total sample size
  array[n] int<lower=1> s;             // item stratum indicator
  array[n] real<lower=0, upper=1> t;   // item taints
  real<lower=0> alpha;                 // prior parameter alpha
  real<lower=0> beta;                  // prior parameter beta
  int beta_prior;                      // beta prior {0 = no, 1 = yes}
  int gamma_prior;                     // gamma prior {0 = no, 1 = yes}
  int normal_prior;                    // normal prior {0 = no, 1 = yes}
  int uniform_prior;                   // uniform prior {0 = no, 1 = yes}
  int cauchy_prior;                    // Cauchy prior {0 = no, 1 = yes}
  int t_prior;                         // Student-t prior {0 = no, 1 = yes} 
  int chisq_prior;                     // Chi-squared prior {0 = no, 1 = yes}
  int exponential_prior;               // exponential prior {0 = no, 1 = yes}
  int use_likelihood;                  // apply likelihood {0 = no, 1 = yes}
}
parameters {
  real<lower=0, upper=1> phi;          // average population taint
  real<lower=1> nu;                    // population taint concentration
  real<lower=1> mu;                    // average stratum taint concentration
  real<lower=0> sigma;                 // st.dev stratum taint concentration
  vector<lower=0, upper=1>[S] theta_s; // average stratum taint
  vector<lower=1>[S] kappa_s;          // stratum taint concentration
}
model {
  // Hyperpriors
  if (beta_prior) {
    phi ~ beta(alpha, beta);
  } else if (gamma_prior) {
    phi ~ gamma(alpha, beta);
  } else if (normal_prior) {
    phi ~ normal(alpha, beta);
  } else if (uniform_prior) {
    phi ~ uniform(alpha, beta);
  } else if (cauchy_prior) {
    phi ~ cauchy(alpha, beta);
  } else if (t_prior) {
    phi ~ student_t(alpha, 0, 1);
  } else if (chisq_prior) {
    phi ~ chi_square(alpha);
  } else if (exponential_prior) {
    phi ~ exponential(alpha);
  }
  nu ~ pareto(1, 1.5);
  mu ~ normal(1, 100);
  sigma ~ normal(0, 10);
  // Priors
  theta_s ~ beta(phi * nu, (1 - phi) * nu);
  kappa_s ~ normal(mu, sigma);
  // Likelihood
  if (use_likelihood) {
    t ~ beta_proportion(theta_s[s], kappa_s[s]);
  }
}
