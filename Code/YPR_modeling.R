load(here('Code/model_fit_base.RData'))
list_of_draws <- rstan::extract(mod)


get_ypr <- function(FF, opening, metric, setup.list) {
  if(metric != 'revenue' & metric != 'yield') 
    stop('Metric must be "revenue" or "yield"')

  list2env(x = setup.list, envir = sys.frame(sys.nframe()))
  pop <- rep(0, n_month)
  Catch <- rep(0, n_month-1)
  revenue <- matrix(0, nrow = length(list_of_draws$B), ncol = n_month-1)
  pop[1] <- 1
  
  for(mo in 1:(n_month-1)) {
    if(opening/2 >= mo %% 12) { # biweekly dynamics first two months to test opening date
      pop.temp <- pop[mo] * exp(0.5 * (-0.09 - FF * (opening/2 < mo %% 12)))
      Catch[mo] <- (opening/2 < mo %% 12) * FF/(FF+0.09) * 
        (1-exp(0.5 * (-FF-0.09))) * pop[mo] 
      pop[mo+1] <- pop.temp * exp(0.5 * (-0.09 - FF * (opening/2 <= mo %% 12)))
      Catch[mo] <- Catch[mo] + (opening/2 <= mo %% 12) * FF/(FF+0.09) * 
        (1-exp(0.5 * (-FF-0.09))) * pop.temp 
    } else {
      pop[mo+1] <- pop[mo] * exp(-0.09*(season[mo]=='S') - 0.06*(season[mo]=='W') -
                                   FF*(season[mo]=='S'))
      Catch[mo] <- FF/(FF+0.09) * (1-exp(-FF-0.09)) * pop[mo] * (season[mo]=='S')
    }
  }
  # Plus group???
  evp <- ifelse((453.592/pred_wt) < max_cpp | is.na(max_cpp), 
                1.4309 - 0.00387 * (453.592/pred_wt), # 453.592/pred_wt (in g) = cpp
                0)
  # from Dan email on 8/5/22
  
  yield <- t(t(pred_wt) * Catch) # this transposing makes the multiplication correct
  revenue <- yield * evp
  
  ypr <- apply(yield, 1, sum)
  rpr <- apply(revenue, 1, sum)
  as_tibble((metric == 'revenue') * rpr + (metric == 'yield') * ypr) %>%
    mutate(FF = FF, opening = opening, mcmc_rep = 1:length(rpr)) %>%
    return()
}

weight.dat <- readxl::read_excel(here('Data/Kiva.xlsx'), sheet = 'weights')
# What are the units of "weight"??? It looks like grams based on 
# inverting cpp and then multiplying by lb -> gram conversion factor,
# comparing to measured weight.


weight.length.mod <- lm(log(Weight) ~ log(CL), data = weight.dat)

# pred_len <- matrix(0, nrow = length(list_of_draws$x0_mean), ncol = 33)
# pred_len[,1] <- list_of_draws$x0_mean
pred_len <- array(0, dim = c(nrow(list_of_draws$area_offset), # MCMC iteration
                             ncol(list_of_draws$area_offset), # state area
                             33),
                  dimnames = list(mcmc_iter=NULL, area=NULL, month=NULL)) # time step
pred_len[,,1] <- apply(list_of_draws$area_offset, 2, function(x)
  x + list_of_draws$x0_mean)
for(mo in 1:32) {
  pred_len[,,mo+1] <- apply(pred_len[,,mo], 2, function(x)
    list_of_draws$B * x + list_of_draws$U)
}

pred_len_base <- matrix(0, nrow = length(list_of_draws$x0_mean), # MCMC iteration
                        ncol = 33, dimnames = list(mcmc_iter=NULL, month=NULL)) # time step
pred_len_base[,1] <- list_of_draws$x0_mean
for(mo in 1:32) {
  pred_len_base[,mo+1] <- pred_len_base[,mo] * list_of_draws$B + list_of_draws$U
}

pred_wt <- exp(coef(weight.length.mod)[1]) * (pred_len + mean(y.vec)) ^
  coef(weight.length.mod)[2]

pred_wt_base <- exp(coef(weight.length.mod)[1]) * (pred_len_base + mean(y.vec)) ^
  coef(weight.length.mod)[2]

# (Could use brms to do the calculation to propagate error???)

n_month <- 34
season <- rep('S', n_month)
season[c(8:12, 20:24)] <- 'W'

ypr.list <- list(n_month = n_month, season = season, max_cpp = NA)

n_FF <- 50
FF_seq <- seq(0, 0.3, length.out = n_FF) 

wt_ls <- purrr::array_tree(pred_wt, margin = 2)

rpr_by_area_long <- expand.grid(FF_seq = FF_seq, opening = 1:5, area = 1:12) %>%
  as_tibble() %>%
  purrr::pmap_dfr(function(FF_seq, opening, area)
    get_ypr(FF = FF_seq, opening = opening, metric = 'revenue', 
            setup.list = c(ypr.list, pred_wt = wt_ls[area])) %>%
      mutate(area = area)) %>%
  rename(rpr = value) %>%
  group_by(FF, opening, area)

rpr_by_area_summary <- rpr_by_area_long %>%
  mutate(opening_chr = paste(month.abb[ceiling(opening/2) + 3], 
                             c(15,1)[opening %% 2 + 1])) %>%
  group_by(area, opening_chr, FF) %>%
  summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
            high = quantile(rpr, 0.975), opening = first(opening))

# to do:
# panels = fishing mortality
# x axis = size at recruitment
# y axis = RPR
# lines = opening date