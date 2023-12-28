library(furrr)
library(rstan)
library(loo)
library(here)


rstan_options(auto_write = TRUE)

fit_growth_mod <- function(input_data, covar, covar_name, mcmc_control = list()) {
  input_data$covar_dat <- as.matrix(covar)
  input_data$n_covar <- length(covar)
  mod <- stan(here('Code/growth_model.stan'),
              data = input_data,
              pars = c('x0_mean', 'sigma_x0', 'x0',
                       'area_offset',
                       'sigma_area',
                       'U', 'B', 'U_season',
                       'sigma_process',
                       'sigma_obs',
                       'pred_vec',
                       'pro_dev', 'covar_par',
                       'log_lik', 'x'
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
     n_big_area = 1, 
     big_area = rep(1, M),
     n_pos = n_pos, col_indx_pos = complete.data[,2], 
     row_indx_pos = complete.data[,1], 
     est_pro_dev = 1,
     est_area_offset = 1,
     calc_ppd = 0, samp_size = n.vec
)

## This runs the MCMC and calculates LOOIC
vars_to_use <- c(as.list(names(covar.all)[1:3]),
                 list(c('cuti_all_lat', 'cuti_all_lat_sq')),
                 list(c('beuti_all_lat', 'beuti_all_lat_sq')))
# vars_to_use <- vars_to_use[[1]]
plan(list(tweak(multisession, workers = 2), tweak(multisession, workers = 4)))
xx <- vars_to_use %>%
  future_map(\(.x) fit_growth_mod(input_data = input_data,
                                  covar = select(covar.all, tidyr::all_of(.x)),
                                  covar_name = tail(.x, 1)))

# Run base model (no covariates)
base_input <- input_data
base_input$calc_ppd <- 1
base_input$n_covar <- 0
base_input$covar_dat <- matrix(0, nrow = nrow(covar.all), ncol=0)
mod <- stan(here('Code/growth_model.stan'), 
                data = base_input,
                pars = c('x0_mean', 'sigma_x0', 'x0', 
                         'area_offset',
                         'sigma_area', 
                         'U', 'B', 'U_season', 
                         'sigma_process', 
                         'sigma_obs', 
                         'pred_vec', 
                         'pro_dev', 'covar_par',
                         'log_lik', 'y_pp'
                ), 
                iter = 4000, chains = 4, cores = 4,
                #iter=100, chains=1,
                #thin = mcmc_list$n_thin, warmup = mcmc_list$n_burn,
                control = list(adapt_delta = 0.9, max_treedepth = 15))
save(mod, file = here('Code/covars/base.Rdata'))
log_lik <- extract_log_lik(mod, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik), cores = 4)
loo_est <- loo(log_lik, r_eff = r_eff, cores = 4)
xx[[6]] <- loo_est

## This calculates LOOIC based on pre-run MCMC chains.
plan(list(tweak(multisession, workers = 2), tweak(multisession, workers = 4)))
loo_est <- future_map(list.files(here('Code/covars')), \(.x) {
  load(here('Code', 'covars', .x))
  log_lik <- extract_log_lik(mod, merge_chains = FALSE)
  r_eff <- relative_eff(exp(log_lik), cores = 4) 
  loo_est <- loo(log_lik, r_eff = r_eff, cores = 4)
  return(loo_est)
})
names(loo_est) <- list.files(here('Code/covars'))
loo_compare(loo_est)
save(loo_est, file = here('Code/loo_est.Rdata'))
beepr::beep()


### Recruitment
library(brms)
library(future)
### want to use raw recruitment and then log transform rather than standardized form.
### This is a task for next week...
### Then: check model fit. potential for AR term?
#plan(list(tweak(multisession, workers = 6), tweak(multisession, workers = 4)))

log_transform <- brm(formula = log(recruits) ~ meanBLT_precond, family = gaussian, data = covar.all)
untransformed <- brm(formula = brmsformula(recruits ~ meanBLT_precond),
                     family = gaussian(link = 'log'), data = covar.all, )
pp_check(log_transform, type = 'intervals') # Very overdispersed. Is there a better error distribution to use?


plan(multisession)

xx <- select(covar.all, -Std_R) %>%
  pivot_longer(cols = -recruits, names_to = 'driver', values_to = 'driver_value') %>%
  group_by(driver) %>%
  nest() %>%
  mutate(model = purrr::map(data, function(df)
    brm(formula = log(recruits) ~ driver_value, family = gaussian, data = df, 
        chains = 4, iter = 1000, cores = 4, seed = 23598709))) %>%
  mutate(looic = purrr::map(model, loo))

# There is probably a better way to do this:
looic.list <- xx$looic
names(looic.list) <- xx$driver
loo_compare(looic.list)

xx %>%
  mutate(tidy_model = purrr::map(model, broom.mixed::tidy)) %>%
  select(-data, -model) %>%
  unnest()

test <- brm(formula = )
