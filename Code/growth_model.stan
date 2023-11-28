// Modified by KLO from https://github.com/nwfsc-timeseries/atsar/blob/master/exec/marss.stan
// Downloaded 3/21/19.

data {
  int<lower=0> N; // number of time steps in a time series
  int<lower=0> M; // number of observed time series
  int<lower=0> cohorts[M]; // vector assigning time series to cohorts
  int<lower=0> S; // number of states (cohorts)
  int<lower=0> n_area; // number of areas
  int<lower=0> area[M];
  int<lower=0> n_big_area;
  int<lower=0> big_area[M];
  int<lower=0> n_pos; // number of non-NA values
  int<lower=0> col_indx_pos[n_pos];
  int<lower=0> row_indx_pos[n_pos];
  vector[n_pos] y; // data
  int<lower=0> n_covar;
  matrix[S, n_covar] covar_dat;
  int<lower=0, upper=1> est_pro_dev;
  int<lower=0, upper=1> est_area_offset;
  int<lower=0, upper=1> calc_ppd;
}

transformed data {
  vector[N] seasons;
  for(t in 1:N) {
    seasons[t] = sin(pi()*fmod(t+1,12)/6);
    // fmod = remainder function
  }
}
parameters {
  real x0_mean;
  real<lower=0> sigma_x0;
  vector[S] x0[n_big_area]; // initial states
  // This is a conditional. x ? y : z means if x then y else z.
  vector[est_pro_dev ? S : 0] pro_dev[est_pro_dev ? N-1 : 0];
  vector[est_area_offset ? n_area : 0] area_offset;
  real<lower=0> sigma_area;
  real U;
  real U_season;
  vector[n_covar] covar_par; 
  real<lower=0, upper=1> B;
  real<lower=0> sigma_process;
  real<lower=0> sigma_obs;
}
transformed parameters {
  vector[M] x[N]; // elements accessed [N,S]
  // AR(1) process in states
  for(m in 1:M) {
    x[1,m] = x0_mean + x0[big_area[m], cohorts[m]]; // initial state, vague prior below
    if(est_area_offset == 1) {
      x[1,m] += area_offset[area[m]]; 
    }
    if(n_covar > 0) {
      x[1,m] += covar_dat[cohorts[m]] * covar_par; // row vector * column vector = scalar
    }
    for(t in 2:N) {
      x[t,m] = B * x[t-1,m] + U + U_season * seasons[t-1];
      if(est_pro_dev == 1) {
        x[t,m] += pro_dev[t-1, cohorts[m]];
      }
    }
  }
}
model {
  // priors
  x0_mean ~ normal(-5.0, 1.0);
  sigma_x0 ~ normal(0.0, 2.0);
  for(i in 1:n_big_area) {
    x0[i] ~ normal(0, sigma_x0);
  }
  if(est_area_offset == 1) {
    sigma_area ~ normal(0.0, 2.0);
    area_offset ~ normal(0, sigma_area);
  }
  sigma_obs ~ normal(0.0, 2.0);
  // sigma_samp ~ normal(0.0, 2.0);
  
  U ~ normal(0.0,1.0);
  U_season ~ normal(0.0,1.0);
  //U_sex ~ normal(1.0, 1.0); // more females should magnify seasonal fluctuations
  B ~ beta(2,2);
  
  if(est_pro_dev == 1) {
    sigma_process ~ normal(0.0, 1.0);
    for(i in 1:(N-1)){
      pro_dev[i] ~ normal(0.0, sigma_process);
    }
  }
  // likelihood
  for(i in 1:n_pos) {
    y[i] ~ normal(x[col_indx_pos[i], row_indx_pos[i]], sigma_obs);// + sigma_samp/sqrt(samp_size[i]));
// this order is correct even if unintuitive
  }
}

generated quantities {
  vector[n_pos] pred_vec;
  vector[n_pos] y_pp;
  vector[n_pos] log_lik;
  
  for(i in 1:n_pos) {
    pred_vec[i] = x[col_indx_pos[i], row_indx_pos[i]];
    log_lik[i] = normal_lpdf(y[i] | pred_vec[i], sigma_obs);
  }
  
  // posterior predictive checks. 
  if(calc_ppd == 1) {
    for(i in 1:n_pos) {
      y_pp[i]  = normal_rng(pred_vec[i], sigma_obs);// + sigma_samp/sqrt(samp_size[i]));
    }
  }
  
}
