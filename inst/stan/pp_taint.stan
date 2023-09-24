#include /include/license.stan

data {
  int<lower=1> S;                           // number of strata
  int<lower=0> n;                           // total sample size
  array[n] int<lower=1> s;                  // item stratum indicator
  array[n] real<lower=0, upper=1> t;        // item taints
  real<lower=0> alpha;                      // prior parameter alpha
  real<lower=0> beta;                       // prior parameter beta
  int beta_prior;                           // beta prior {0 = no, 1 = yes}
  int gamma_prior;                          // gamma prior {0 = no, 1 = yes}
  int normal_prior;                         // normal prior {0 = no, 1 = yes}
  int uniform_prior;                        // uniform prior {0 = no, 1 = yes}
  int cauchy_prior;                         // Cauchy prior {0 = no, 1 = yes}
  int t_prior;                              // Student-t prior {0 = no, 1 = yes} 
  int chisq_prior;                          // Chi-squared prior {0 = no, 1 = yes}
  int exponential_prior;                    // exponential prior {0 = no, 1 = yes}
  int use_likelihood;                       // apply likelihood {0 = no, 1 = yes}
}
parameters {
  real<lower=0, upper=1> phi;               // population mean taint
  real<lower=1> nu;                         // population taint concentration
  real<lower=1> mu;                         // mean stratum taint concentration
  real<lower=0> sigma;                      // stratum taint concentration st.dev
  vector<lower=0, upper=1>[S] theta_s;      // stratum taint
  vector<lower=1>[S] kappa_s;               // stratum taint concentration
}
model {
  if (beta_prior) {
    phi ~ beta(alpha, beta);                // hyperprior
  } else if (gamma_prior) {
    phi ~ gamma(alpha, beta);               // hyperprior
  } else if (normal_prior) {
    phi ~ normal(alpha, beta);              // hyperprior
  } else if (uniform_prior) {
    phi ~ uniform(alpha, beta);             // hyperprior
  } else if (cauchy_prior) {
    phi ~ cauchy(alpha, beta);              // hyperprior
  } else if (t_prior) {
    phi ~ student_t(alpha, 0, 1);           // hyperprior
  } else if (chisq_prior) {
    phi ~ chi_square(alpha);                // hyperprior
  } else if (exponential_prior) {
    phi ~ exponential(alpha);               // hyperprior
  }
  nu ~ pareto(1, 1.5);                      // hyperprior
  mu ~ normal(1, 100);                      // hyperprior
  sigma ~ normal(0, 10);                    // hyperprior
  theta_s ~ beta(phi * nu, (1 - phi) * nu); // prior
  kappa_s ~ normal(mu, sigma);              // prior
  if (use_likelihood) {
    for (i in 1:n) {
      t[i] ~ beta_proportion(theta_s[s[i]], kappa_s[s[i]]); // likelihood
    }
  }
}
