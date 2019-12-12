// Modified by KLO from https://github.com/nwfsc-timeseries/atsar/blob/master/exec/marss.stan
// Downloaded 3/21/19.

data {
  int<lower=0> N; // number of time steps in a time series
  int<lower=0> M; // number of observed time series
  int<lower=0> cohorts[M]; // vector assigning time series to cohorts
  int<lower=0> S; // number of states (cohorts)
  int<lower=0> n_area; // number of areas
  int<lower=0> area[M];
  int<lower=0> n_pos; // number of non-NA values
  int<lower=0> col_indx_pos[n_pos];
  int<lower=0> row_indx_pos[n_pos];
  vector[n_pos] y; // data
  real area_centers[n_area];
  int<lower=0, upper=1> calc_ppd;
}

transformed data {
  vector[N] seasons;
  vector[n_area] zeros;
  matrix[n_area, n_area] dist;

  for(t in 1:N) {
    seasons[t] = sin(pi()*fmod(t+1,12)/6);
  }
  for(i in 1:n_area) {
    zeros[i] = 0;
  }
  
  for(i in 1:n_area) {
    for(j in 1:n_area){
      dist[i,j] = fabs(area_centers[i] - area_centers[j]);
    }
  }
}
parameters {
  real x0_mean;
  real<lower=0> sigma_x0;
  vector[S] x0; // initial states
  vector[n_area] pro_dev[N-1, S];
  vector[n_area] area_offset;
  real<lower=0> sigma_area;
  real U;
  real U_season;
  real<lower=0, upper=1> B;
  real<lower=0> sigma_process;
  real<lower=0> sigma_obs;
  
  real<lower=0> gp_theta;
  real<lower=0> gp_sigma;
}
transformed parameters {
  vector[M] x[N]; // elements accessed [N,M]
  matrix[n_area, n_area] L_K;
  matrix[n_area, n_area] covmat;
  real gp_sigma_sq = square(gp_sigma);

  for(i in 1:n_area) {
    for(j in 1:n_area) {
      covmat[i,j] = gp_sigma_sq * exp(-dist[i,j] / gp_theta);
    }
  }
  L_K = cholesky_decompose(covmat);
 
  // AR(1) process in states
  for(m in 1:M) {
    x[1,m] = x0[cohorts[m]] + area_offset[area[m]]; // initial state, vague prior below
    for(t in 2:N) {
      x[t,m] = B*x[t-1, m] + U_season *  seasons[t-1] + U + pro_dev[t-1, cohorts[m], area[m]];
    }
  }
}
model {
  x0_mean ~ normal(-5.0, 1.0);
  sigma_x0 ~ normal(0.0, 2.0);
  x0 ~ normal(x0_mean, sigma_x0);
  
  sigma_area ~ normal(0.0, 2.0);
  area_offset ~ normal(0, sigma_area);
  
  sigma_obs ~ normal(0.0, 2.0);

  sigma_process ~ normal(0.0, 1.0);
  gp_sigma ~ normal(0,1);
  gp_theta ~ normal(0,1);

  U ~ normal(0.0,1.0);
  U_season ~ normal(0.0,1.0);
  B ~ beta(2,2);
  for(i in 1:(N-1)){
    for(j in 1:S){
      pro_dev[i,j] ~ multi_normal_cholesky(zeros, L_K);
    }
  }

  // likelihood
  for(i in 1:n_pos) {
    y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]], sigma_obs);
    // this order is correct even if unintuitive
  }
}
generated quantities {
  vector[n_pos] pred_vec;
  vector[n_pos] y_pp;
  
  for(i in 1:n_pos) {
    pred_vec[i] = x[col_indx_pos[i], row_indx_pos[i]];
  }

  // posterior predictive checks. 
  if(calc_ppd == 1) {
    for(i in 1:n_pos) {
      y_pp[i]  = normal_rng(pred_vec[i], sigma_obs);
    }
  }

// vector[n_pos] log_lik;
  // // regresssion example in loo() package
  // for (n in 1:n_pos) {
  //   log_lik[n] = normal_lpdf(y[n] | pred[col_indx_pos[n], row_indx_pos[n]],
  //                            sigma_obs[obsVariances[row_indx_pos[n]]]/sqrt(samp_size[n]));
  // }
}
