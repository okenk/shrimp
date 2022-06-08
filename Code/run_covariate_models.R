library(furrr)
library(loo)

fit_growth_mod <- function(input_data, covar, covar_name, mcmc_control = list()) {
  input_data$covar_dat <- as.matrix(covar, ncol = 1)
  input_data$n_covar <- 1
  mod <- stan('Code/growth_model.stan', 
              data = input_data, 
              pars = c('x0_mean', 'sigma_x0', 'x0', 
                       list('area_offset', NULL, 'area_offset')[[ii]],
                       'sigma_area', 
                       'U', 'B', 'U_season', 
                       'sigma_process', 
                       'sigma_obs', 
                       'pred_vec', 
                       'pro_dev', 'covar_par',
                       'log_lik'
                       #                       'y_pp'
              ), 
              iter = 4000, chains = 4, cores = 4,
              seed = 2309853209,
              # algorithm = 'Fixed_param',
              #iter=100, chains=1,
              #thin = mcmc_list$n_thin, warmup = mcmc_list$n_burn,
              control = list(adapt_delta = 0.9, max_treedepth = 15))
  save(mod, file = here(glue::glue('Code/covars/model_fit_{covar_name}.RData')))
  
  log_lik <- extract_log_lik(mod, merge_chains = FALSE)
  r_eff <- relative_eff(exp(log_lik), cores = 4) 
  loo_est <- loo(log_lik, r_eff = r_eff, cores = 4)
  return(loo_est)
}

ii <- 1
input_data <- list(N = N, M = M, y = y.vec - mean(y.vec),  
     cohorts = cohorts, 
     S = max(cohorts), 
     n_area = max(areas), area = areas,
     n_big_area = c(1, rep(max(big.areas), 2))[ii], 
     big_area = cbind(1, big.areas, big.areas)[,ii],
     n_pos = n_pos, col_indx_pos = complete.data[,2], 
     row_indx_pos = complete.data[,1], 
     est_pro_dev = 1,
     est_area_offset = c(1,0,1)[ii],
     calc_ppd = 0, samp_size = n.vec)

library(furrr)

plan(list(tweak(multisession, workers = 6), tweak(multisession, workers = 4)))
xx <- covar.all %>%
  future_imap(~ fit_growth_mod(input_data = input_data, covar = .x, covar_name = .y))

loo_compare(xx)

library(furrr)
library(loo)
plan(list(tweak(multisession, workers = 6), tweak(multisession, workers = 4)))
xx <- future_map(names(covar.all), function(.x) {
  load(here(glue::glue('Code/covars/model_fit_{.x}.RData')))
  log_lik <- extract_log_lik(mod, merge_chains = FALSE)
  r_eff <- relative_eff(exp(log_lik), cores = 4) 
  loo_est <- loo(log_lik, r_eff = r_eff, cores = 4)
  return(loo_est)
})

load(here('Code/model_fit_base.RData'))
log_lik <- extract_log_lik(mod, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 4) 
xx$base <- loo(log_lik, r_eff = r_eff, cores = 4)

names(xx)[1:6] <- names(covar.all)
