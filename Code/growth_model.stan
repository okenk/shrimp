// Modified by KLO from https://github.com/nwfsc-timeseries/atsar/blob/master/exec/marss.stan
// Downloaded 3/21/19.

data {
  int<lower=0> N; // number of time steps in a time series
  int<lower=0> M; // number of observed time series
  int<lower=0> states[M]; // vector assigning time series to states
  int<lower=0> S; // number of states (cohorts)
  int<lower=0> n_area; // number of areas
  // int<lower=0> obsVariances[M];
  // int<lower=0> n_obsvar;
  // int<lower=0> proVariances[S+1];
  // int<lower=0> n_provar;
  // int<lower=0> trends[S+1];
  // int<lower=0> n_trends;
  int<lower=0> n_pos; // number of non-NA values
  int<lower=0> col_indx_pos[n_pos];
  int<lower=0> row_indx_pos[n_pos];
  vector[n_pos] y; // data
  vector[n_pos] samp_size; // number of measurements going into each mean length
  int area[n_pos];
  int<lower=0, upper=1> return_preds;
  int<lower=0, upper=1> calc_ppd;
  // int family; // 1 = normal, 2 = binomial, 3 = poisson, 4 = gamma, 5 = lognormal
}
parameters {
  real x0_mean;
  real<lower=0> sigma_x0;
  vector[S] x0; // initial states
  vector[S] pro_dev[N-1];
  vector[n_area] area_offset;
  real<lower=0> sigma_area;
  real U;
  real B;
  // vector[n_trends] U;
  real<lower=0> sigma_process;//[S];
  real<lower=0> sigma_obs;//[n_obsvar];
}
transformed parameters {
  vector[M] pred[N];
  vector[S] x[N]; // elements accessed [N,S]
  // AR(1) process in states
   x[1,] = x0; // initial state, vague prior below
   for(t in 2:N) {
    x[t,] = B*x[t-1,] + U + pro_dev[t-1,];
   }

  // map predicted states to time series
  for(m in 1:M) {
      pred[,m] = x[,states[m]];
  }
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
  B ~ normal(0.0,1.0);
  // B ~ normal(0.75, 1.0);
  for(i in 1:(N-1)){
    pro_dev[i] ~ normal(0.0, sigma_process);
  }
  // for(s in 1:S) {
  //   pro_dev[s] ~ normal(0, sigma_process[proVariances[s]]); // process deviations
  // }

  // likelihood
  for(i in 1:n_pos) {
    y[i] ~ normal(pred[col_indx_pos[i], row_indx_pos[i]] + area_offset[area[i]], sigma_obs);///sqrt(samp_size[i])); 
    // this order is correct even if unintuitive
  }
}
generated quantities {
  vector[n_pos] pred_vec;
  vector[M] pred_sim[N];
  vector[S] x_sim[N]; // elements accessed [N,S]
  vector[n_pos] y_pp;
  
  if(return_preds == 1) {
    for(i in 1:n_pos) {
      pred_vec[i] = pred[col_indx_pos[i], row_indx_pos[i]];
    }
  }
  
  // posterior predictive checks
  if(calc_ppd == 1) {
    for(s in 1:S) {
      x_sim[1,s] = normal_rng(x0_mean, sigma_x0); 
    }
    for(s in 1:S) {
      for(t in 2:N) {
        x_sim[t,s] = normal_rng(B*x_sim[t-1,s] + U, sigma_process);
      }
    }
    for(n in 1:N) {
      for(m in 1:M) {
        pred_sim[n,m] = x_sim[n,states[m]];
      }
    }
    for(i in 1:n_pos) {
      y_pp[i]  = normal_rng(pred_sim[col_indx_pos[i], row_indx_pos[i]] + area_offset[area[i]], 
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
