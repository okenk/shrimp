// Modified by KLO from https://github.com/nwfsc-timeseries/atsar/blob/master/exec/marss.stan
// Downloaded 3/21/19.

data {
  int<lower=0> N; // number of time steps in a time series
  int<lower=0> M; // number of observed time series
  int<lower=0> cohorts[M]; // vector assigning time series to cohorts
  int<lower=0> S; // number of states (cohorts)
  int<lower=0> n_area; // number of areas
  int<lower=0> area[M];
  // int<lower=0> obsVariances[M];
  // int<lower=0> n_obsvar;
  // int<lower=0> proVariances[S+1];
  // int<lower=0> n_provar;
  // int<lower=0> trends[S+1];
  // int<lower=0> n_trends;
  int<lower=0> n_pos; // number of non-NA values
  vector<lower=0, upper=1>[N-1] winter_ind; // which time steps are during the winter?
  int<lower=0> col_indx_pos[n_pos];
  int<lower=0> row_indx_pos[n_pos];
  vector[n_pos] y; // data
  vector[M] bio[N];
  vector[M] ssh[N];
  // int<lower=0> area[n_pos];
  int<lower=0, upper=1> calc_ppd;
  // int family; // 1 = normal, 2 = binomial, 3 = poisson, 4 = gamma, 5 = lognormal
}

transformed data {
  vector[N] seasons;
  for(t in 1:N) {
    seasons[t] = sin(pi()*fmod(t+1,12)/6);
  }
}
parameters {
  real x0_mean;
  real<lower=0> sigma_x0;
  vector[S] x0; // initial states
  vector[S] pro_dev[N-1];
  vector[n_area] area_offset;
  real<lower=0> sigma_area;
  real U;
  real U_season;
  real U_bio;
  real U_ssh;
  real<lower=0, upper=1> B;
  real<lower=0> sigma_process;//[S];
  real<lower=0> sigma_obs;//[n_obsvar];
}
transformed parameters {
  // vector[M] pred[N];
  vector[M] x[N]; // elements accessed [N,S]
  // AR(1) process in states
  for(m in 1:M) {
    x[1,m] = x0[cohorts[m]] + area_offset[area[m]]; // initial state, vague prior below
    for(t in 2:N) {
      x[t,m] = B*x[t-1,m] + U_season*seasons[t-1] + U + pro_dev[t-1,cohorts[m]];
    }
  }
  

  // map predicted states to time series
  // for(m in 1:M) {
  //     pred[,m] = x[,cohorts[m]];
  // }
}
model {
  x0_mean ~ normal(0.0, 10.0);
  sigma_x0 ~ cauchy(0.0, 5.0);
  x0 ~ normal(x0_mean, sigma_x0);
  
  sigma_area ~ cauchy(0.0, 5.0);
  area_offset ~ normal(0, sigma_area);
  
  sigma_obs ~ cauchy(0.0, 5.0);
  sigma_process ~ cauchy(0.0, 5.0);
  
  U ~ normal(0.0,1.0);
  U_season ~ normal(0.0,1.0);
  U_bio ~ normal(0.0,1.0);
  U_ssh ~ normal(0.0,1.0);
  B ~ beta(2,2);
  // B ~ normal(0.75, 1.0);
  for(i in 1:(N-1)){
    pro_dev[i] ~ normal(0.0, sigma_process);
  }
  // for(s in 1:S) {
  //   pro_dev[s] ~ normal(0, sigma_process[proVariances[s]]); // process deviations
  // }

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
      y_pp[i]  = normal_rng(pred_vec[i],// + area_offset[area[i]], 
                            sigma_obs);///sqrt(samp_size[i])); 
    }
  }

// vector[n_pos] log_lik;
  // // regresssion example in loo() package
  // for (n in 1:n_pos) {
  //   log_lik[n] = normal_lpdf(y[n] | pred[col_indx_pos[n], row_indx_pos[n]],
  //                            sigma_obs[obsVariances[row_indx_pos[n]]]/sqrt(samp_size[n]));
  // }
}
