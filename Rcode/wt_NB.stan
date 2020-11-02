/* Logistic regression with exponentiated alphas */

functions{

/**
  * Return the log probability of a sampling-weighted bernoulli distribution
  * for an n-vector of parameter values, y
  *
  * @param y Vector containing the parameters under a bernoulli
  * @param mu vector containing linear predictor (logit scale)
  * @param alphas Vector of observation-specific marginal alphas
  * @param n number of observations (length of y, mu, alphas)
  */
real wt_NB_lpmf(int[] y, vector mu, real phi, vector alphas, int n){
    real check_term;
    check_term  = 0.0;
    for( i in 1:n )
    {
	check_term    += alphas[i] * neg_binomial_2_log_lpmf(y[i] | mu[i], phi);
	/*try to keep slope at zero equal to 1*/
    }
    return check_term;
  }

real wt_NBi_lpmf(int y_i, real mu_i, real phi, real alphas_i){
    real check_term;
	check_term    = alphas_i * neg_binomial_2_log_lpmf(y_i | mu_i, phi);
    return check_term;
  }
} /* end function{} block */


data {
    int<lower=1> n; // number of observations
	  int<lower=1> K; // number of linear predictors
    int<lower=0> y[n]; // Response variable
    vector<lower=0>[n] alphas; // observation-indexed (sampling) alphas
    matrix[n, K] X; // coefficient matrix
}

transformed data{
  vector<lower=0>[K] zeros_beta;
  zeros_beta  = rep_vector(0,K);
} /* end transformed data block */

parameters{
  vector[K] beta; /* regression coefficients from linear predictor */
  vector<lower=0>[K] sigma_beta; 
  cholesky_factor_corr[K] L_beta; /* cholesky of correlation matrix for Sigma_beta */
  real<lower=0> reciprocal_phi; 
}

transformed parameters{
  vector[n] mu;
  real<lower=0> phi;
  mu   = X * beta;
  phi = 1. / reciprocal_phi;
} /* end transformed parameters block */

model{
  /*caucy prior for 1/phi*/
  reciprocal_phi ~ cauchy(0., 5);
  /*improper prior on theta in (-inf,inf)*/
  L_beta          ~ lkj_corr_cholesky(6);
  sigma_beta      ~ student_t(3,0,1);
  //beta            ~ normal(0,sigma_beta);
  /* Implement a beta ~ N_{T}(0,Q^{-1})  */
  beta            ~ multi_normal_cholesky( zeros_beta, diag_pre_multiply(sigma_beta,L_beta) ); /* K x 1, vector */
  /* directly update the log-probability for sampling */
  target          += wt_NB_lpmf(y | mu, phi, alphas, n);
} /* end model{} block */

generated quantities{
  vector[n] log_lik;
  for (i in 1:n) {
	log_lik[i] = wt_NBi_lpmf(y[i]| mu[i], phi, alphas[i]);
	}

}
