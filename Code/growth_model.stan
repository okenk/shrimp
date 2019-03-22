// Modified by KLO from https://github.com/nwfsc-timeseries/atsar/blob/master/exec/marss.stan
// Downloaded 3/21/19.

data {
  int<lower=0> N;
  int<lower=0> M;
  int<lower=0> states[M]; // vector assigning time series to states
  int<lower=0> S; // number of states
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
  int y_int[n_pos];
  // int family; // 1 = normal, 2 = binomial, 3 = poisson, 4 = gamma, 5 = lognormal
}
parameters {
  real x0_mean;
  real x0_sd;
  vector[S] x0; // initial states
  vector[S] pro_dev[N-1];
  real U;
  real B;
  // vector[n_trends] U;
  real<lower=0> sigma_process;//[S];
  real<lower=0> sigma_obs;//[n_obsvar];
}
transformed parameters {
  vector[M] pred[N];
  vector[S] x[N]; // elements accessed [N,K]
  // random walk in states
   x[1,] = x0; // initial state, vague prior below
   for(t in 2:N) {
    x[t,] = x[t-1,] + U + pro_dev[t-1,];
    // x[t,s] = x[t-1,s] + U[trends[s]] + pro_dev[t-1,s];
   }

  // map predicted states to time series
  for(m in 1:M) {
      pred[,m] = x[,states[m]];
  }
}
model {
  x0_mean ~ normal(0.0, 10.0);
  x0_sd ~ cauchy(0.0, 5.0);
  x0 ~ normal(x0_mean, x0_sd);
  sigma_obs ~ gamma(0.982^2/.025, 0.982/.025); // mean = .982 (weighted avg of popn SD), sd = .5
  sigma_process ~ cauchy(0.0, 5.0);
  U ~ normal(0.0,1.0);
  // B ~ normal(0.75, 1.0);
  for(i in 1:(N-1)){
    pro_dev[i] ~ normal(0.0, sigma_process);
  }
  // for(s in 1:S) {
  //   pro_dev[s] ~ normal(0, sigma_process[proVariances[s]]); // process deviations
  // }

  // likelihood
  for(i in 1:n_pos) {
    y[i] ~ normal(pred[col_indx_pos[i], row_indx_pos[i]], sigma_obs/sqrt(samp_size[i]));
                  // sigma_obs[obsVariances[row_indx_pos[i]]])/sqrt(samp_size[i]));
  }
}
// generated quantities {
//   vector[n_pos] log_lik;
//   // regresssion example in loo() package
//   for (n in 1:n_pos) {
//     log_lik[n] = normal_lpdf(y[n] | pred[col_indx_pos[n], row_indx_pos[n]],
//                              sigma_obs[obsVariances[row_indx_pos[n]]]/sqrt(samp_size[n]));
//   }
// }
