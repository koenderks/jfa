#include /include/license.stan

functions {
  int num_non_discrete(vector y) {
    int j = 0;
    for (i in 1:size(y)) {
      if (y[i] != 0) {
        j += 1;
      }
    }
    return j; 
  } 
}
data {
  int<lower=0> n;                // Number of observations in sample
  vector<lower=0, upper=1>[n] y; // Sample taints
  int<lower=0> N;                // Number of items in population
  vector<lower=0>[N] E;          // Book values of items in population
  real<lower=0> alpha;           // prior parameter alpha
  real<lower=0> beta;            // prior parameter beta
  int beta_prior;                // beta prior {0 = no, 1 = yes}
  int gamma_prior;               // gamma prior {0 = no, 1 = yes}
  int normal_prior;              // normal prior {0 = no, 1 = yes}
  int uniform_prior;             // uniform prior {0 = no, 1 = yes}
  int cauchy_prior;              // Cauchy prior {0 = no, 1 = yes}
  int t_prior;                   // Student-t prior {0 = no, 1 = yes} 
  int chisq_prior;               // Chi-squared prior {0 = no, 1 = yes}
  int exponential_prior;         // exponential prior {0 = no, 1 = yes}
  int use_likelihood;            // Apply the likelihood
}
transformed data {
  int N_zeroes = 0;
  vector[num_non_discrete(y)] y_non_discrete;
  int j = 1; 
  for (i in 1:n) {
    if (y[i] == 0) {
      N_zeroes += 1; 
    } else {
      y_non_discrete[j] = y[i];
      j += 1; 
    }
  }
}
parameters {
  real<lower=[(uniform_prior ? alpha : 0)][1], upper=[(uniform_prior ? beta : 1)][1]> p_error; // Probability of a misstatement occurring
  real<lower=0, upper=1> phi; // Average of non-zero-one taint
  real<lower=1> nu; // Concentration parameter
}
transformed parameters {
  real p_discrete = 1 - p_error; // Probability of y being 0 or 1
  // Probability of y being 0, 1, and (0, 1)
  simplex[2] prob = [p_discrete, 1 - p_discrete]';
}
model {
  // Prior distributions
  if (beta_prior) {
    p_error ~ beta(alpha, beta);
  } else if (gamma_prior) {
    p_error ~ gamma(alpha, beta);
  } else if (normal_prior) {
    p_error ~ normal(alpha, beta);
  } else if (uniform_prior) {
    p_error ~ uniform(alpha, beta);
  } else if (cauchy_prior) {
    p_error ~ cauchy(alpha, beta);
  } else if (t_prior) {
    p_error ~ student_t(alpha, 0, 1);
  } else if (chisq_prior) {
    p_error ~ chi_square(alpha);
  } else if (exponential_prior) {
    p_error ~ exponential(alpha);
  }
  phi ~ beta(1, 1);
  nu ~ pareto(1, 1.5);
  // Likelihood
  if (use_likelihood) { 
    target += N_zeroes * log(prob[1]);
    target += size(y_non_discrete) * log(prob[2]) + beta_proportion_lpdf(y_non_discrete | phi, nu);
  }
}
generated quantities {
  // Model predicted misstatement
  real<lower=0> E_rep = 0;
  for (i in 1:N) {
    if (bernoulli_rng(p_error)) {
      E_rep += beta_proportion_rng(phi, nu) * E[i];
    }
  }
  real<lower=0, upper=1> theta = E_rep / sum(E);
}
