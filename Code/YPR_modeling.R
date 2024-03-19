source(here('code/length_data_manip.R'))
mod <- readRDS(here('Code/covars/base.Rds'))
list_of_draws <- rstan::extract(mod)
season_num <- sin(pi*(1:N+1)%%12/6) # consistent w/stan model implementation

get_ypr <- function(FF, opening, metric, setup.list) {
  if(metric != 'revenue' & metric != 'yield') 
    stop('Metric must be "revenue" or "yield"')

  list2env(x = setup.list, envir = sys.frame(sys.nframe()))
  pop <- rep(0, n_month)
  Catch <- rep(0, n_month)
  revenue <- matrix(0, nrow = length(list_of_draws$B), ncol = n_month-1)
  pop[1] <- 1
  
  for(mo in 1:(n_month-1)) {
    if(opening/2 >= mo %% 12) { # biweekly dynamics first two months to test opening date
      pop.temp <- pop[mo] * exp(0.5 * (-Ms - FF * (opening/2 < mo %% 12)))
      Catch[mo] <- (opening/2 < mo %% 12) * FF/(FF+Ms) * 
        (1-exp(0.5 * (-FF-Ms))) * pop[mo] 
      pop[mo+1] <- pop.temp * exp(0.5 * (-Ms - FF * (opening/2 <= mo %% 12)))
      Catch[mo] <- Catch[mo] + (opening/2 <= mo %% 12) * FF/(FF+Ms) * 
        (1-exp(0.5 * (-FF-Ms))) * pop.temp 
    } else {
      pop[mo+1] <- pop[mo] * exp(-Ms*(season[mo]=='S') - Mw*(season[mo]=='W') -
                                   FF*(season[mo]=='S'))
      Catch[mo] <- FF/(FF+Ms) * (1-exp(-FF-Ms)) * pop[mo] * (season[mo]=='S')
    }
  }
  Catch[n_month] <- FF/(FF+Ms) * (1-exp(-FF-Ms)) * pop[n_month] * (season[n_month]=='S')
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

# fit length-weight model
weight.dat <- readxl::read_excel(here('Data/Kiva.xlsx'), sheet = 'weights')
# What are the units of "weight"??? It looks like grams based on 
# inverting cpp and then multiplying by lb -> gram conversion factor,
# comparing to measured weight.


weight.length.mod <- lm(log(Weight) ~ log(CL), data = weight.dat)

# calculate weight-age accounting for spatial variation
pred_len <- array(0, dim = c(nrow(list_of_draws$area_offset), # MCMC iteration
                             ncol(list_of_draws$area_offset), # state area
                             N), # time step
                  dimnames = list(mcmc_iter=NULL, area=NULL, month=NULL)) 
pred_len[,,1] <- apply(list_of_draws$area_offset, 2, function(x)
  x + list_of_draws$x0_mean)
for(mo in 1:(N-1)) {
  pred_len[,,mo+1] <- apply(pred_len[,,mo], 2, function(x)
    list_of_draws$B * x + list_of_draws$U + list_of_draws$U_season * season_num[mo])
}

pred_wt <- exp(coef(weight.length.mod)[1] + 0.5 * sigma(weight.length.mod)^2) * 
  (pred_len + mean(y.vec)) ^ coef(weight.length.mod)[2]

# calculate weight-age accounting for temporal variation
pred_len_yr <- array(0, dim = c(dim(list_of_draws$x0)[1], # MCMC iteration
                             dim(list_of_draws$x0)[3], # cohort
                             N), # time step
                  dimnames = list(mcmc_iter=NULL, cohort=NULL, month=NULL)) 
pred_len_yr[,,1] <- apply(list_of_draws$x0[,1,], 2, function(x)
  x + list_of_draws$x0_mean)
for(mo in 1:(N-1)) {
  pred_len_yr[,,mo+1] <- apply(pred_len_yr[,,mo], 2, function(x)
    list_of_draws$B * x + list_of_draws$U + list_of_draws$U_season * season_num[mo])
}

pred_wt_yr <- exp(coef(weight.length.mod)[1] + 0.5 * sigma(weight.length.mod)^2) * 
  (pred_len_yr + mean(y.vec)) ^ coef(weight.length.mod)[2]

# calculate mean weight-age relationship
pred_len_base <- matrix(0, nrow = length(list_of_draws$x0_mean), # MCMC iteration
                        ncol = N, dimnames = list(mcmc_iter=NULL, month=NULL)) # time step
pred_len_base[,1] <- list_of_draws$x0_mean
for(mo in 1:(N-1)) {
  pred_len_base[,mo+1] <- pred_len_base[,mo] * list_of_draws$B + list_of_draws$U + 
    list_of_draws$U_season * season_num[mo]
}

pred_wt_base <- exp(coef(weight.length.mod)[1] + 0.5 * sigma(weight.length.mod)^2) * 
  (pred_len_base + mean(y.vec)) ^ coef(weight.length.mod)[2]

# calculations for space AND time (extreme cases only)

area_ind <- c(which.min(apply(list_of_draws$area_offset, 2, mean)),
              which.max(apply(list_of_draws$area_offset, 2, mean)))
year_ind <- c(which.min(apply(list_of_draws$x0, 3, mean)),
              which.max(apply(list_of_draws$x0, 3, mean)))

x0_ls <- purrr::array_tree(list_of_draws$x0[,,year_ind], margin = 2) |>
  `names<-`(year_ind)
area_ls <- purrr::array_tree(list_of_draws$area_offset[,area_ind], margin = 2) |>
  `names<-`(area_ind)

x0_area_cross <- tidyr::expand_grid(x0_ls, area_ls) |>
  mutate(year_i = names(x0_ls), area_j = names(area_ls),
    init = purrr::map2(x0_ls, area_ls, \(x,y) x + y + list_of_draws$x0_mean))

pred_len_st <- array(0, dim = c(dim(list_of_draws$x0)[1], # MCMC iteration
                                2, # cohort
                                2, # area
                                N), # time step
                     dimnames = list(mcmc_iter=NULL, cohort=year_ind, area = area_ind, month=NULL)) 

for(i in 1:2) {
  for(j in 1:2) {
    pred_len_st[,i,j,1] <- x0_area_cross |> 
      filter(year_i == year_ind[i], area_j == area_ind[j]) |>
      select(init) |> 
      unlist()
  }
}

for(mo in 1:(N-1)) {
  pred_len_st[,,,mo+1] <- apply(pred_len_st[,,,mo], c(2,3), function(x)
    list_of_draws$B * x + list_of_draws$U + list_of_draws$U_season * season_num[mo])
}

pred_wt_st <- exp(coef(weight.length.mod)[1] + 0.5 * sigma(weight.length.mod)^2) * 
  (pred_len_st + mean(y.vec)) ^ coef(weight.length.mod)[2]


# Apply RPR model to different weight-age relationships -------------------

n_month <- N
season <- rep('S', n_month)
season[c(8:12, 20:24)] <- 'W'

ypr.list <- list(n_month = n_month, 
                 season = season, 
                 max_cpp = NA, 
                 # max_cpp = 160,
                 Ms = 0.09, 
                 Mw = 0.06)

n_FF <- 50
FF_seq <- seq(0, 0.3, length.out = n_FF) 

# Accounting for spatial variation
wt_ls <- purrr::array_tree(pred_wt, margin = 2)

rpr_by_area_long <- expand.grid(FF_seq = FF_seq, opening = 1:5, 
                                area = 1:length(unique(y.df$Area))) %>%
  as_tibble() %>% 
  purrr::pmap_dfr(function(FF_seq, opening, area)
    get_ypr(FF = FF_seq, opening = opening, metric = 'revenue', 
            setup.list = c(ypr.list, pred_wt = wt_ls[area])) %>%
      mutate(area = unique(y.df$Area)[area])) %>%
  rename(rpr = value) %>%
  group_by(FF, opening, area)


rpr_by_area_summary <- rpr_by_area_long %>%
  mutate(opening_chr = paste(month.abb[ceiling(opening/2) + 3], 
                             c(15,1)[opening %% 2 + 1]),
         opening_fct = factor(opening_chr, levels = c('Apr 1', 'Apr 15', 'May 1',
                                                      'May 15', 'Jun 1'))) %>%
  group_by(area, opening_fct, FF) %>%
  summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
            high = quantile(rpr, 0.975), opening = first(opening))

# Accounting for temporal variation
wt_ls_yr <- purrr::array_tree(pred_wt_yr, margin = 2)

rpr_by_yr_long <- expand.grid(FF_seq = FF_seq, opening = 1:5, cohort = 1:dim(pred_wt_yr)[2]) %>%
  as_tibble() %>%
  purrr::pmap_dfr(function(FF_seq, opening, cohort)
    get_ypr(FF = FF_seq, opening = opening, metric = 'revenue', 
            setup.list = c(ypr.list, pred_wt = wt_ls_yr[cohort])) %>%
      mutate(cohort = unique(sort(y.df$Year_Class))[cohort])) %>%
  rename(rpr = value) %>%
  group_by(FF, opening, cohort)

rpr_by_yr_summary <- rpr_by_yr_long %>%
  mutate(opening_chr = paste(month.abb[ceiling(opening/2) + 3], 
                             c(15,1)[opening %% 2 + 1]),
         opening_fct = factor(opening_chr, levels = c('Apr 1', 'Apr 15', 'May 1',
                                                      'May 15', 'Jun 1'))) %>%
  group_by(cohort, opening_fct, FF) %>%
  summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
            high = quantile(rpr, 0.975), opening = first(opening))

# RPR based on mean, sensitivity to M
base.setup.list <- within(ypr.list, rm(Ms, Mw))
rpr_base_long <- expand.grid(FF_seq = FF_seq, opening = 1:5, M.multiplier = c(0.75, 1, 1.25)) %>%
  as_tibble() %>%
  purrr::pmap_dfr(function(FF_seq, opening, M.multiplier)
    get_ypr(FF = FF_seq, opening = opening, metric = 'revenue', 
            setup.list = c(base.setup.list, Ms = M.multiplier * ypr.list$Ms, 
                           Mw = M.multiplier * ypr.list$Mw, 
                           pred_wt = list(pred_wt_base))) %>%
    mutate(M.multiplier = M.multiplier)) %>%
  rename(rpr = value) %>%
  group_by(FF, opening, M.multiplier) 

rpr_base_summary <- rpr_base_long %>%
  mutate(opening_chr = paste(month.abb[ceiling(opening/2) + 3], 
                             c(15,1)[opening %% 2 + 1]),
         opening_fct = factor(opening_chr, levels = c('Apr 1', 'Apr 15', 'May 1',
                                                      'May 15', 'Jun 1'))) %>%
  group_by(M.multiplier, opening_fct, FF) %>%
  summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
            high = quantile(rpr, 0.975), opening = first(opening))

# Accounting for spatial AND temporal variation
rpr_st_long <- expand.grid(FF_seq = FF_seq, opening = 1:5, cohort = 1:2, area = 1:2) %>%
  as_tibble() %>%
  purrr::pmap_dfr(function(FF_seq, opening, cohort, area)
    get_ypr(FF = FF_seq, opening = opening, metric = 'revenue', 
            setup.list = c(ypr.list, pred_wt = list(pred_wt_st[,cohort, area,]))) %>%
      mutate(cohort = unique(sort(y.df$Year_Class))[year_ind[cohort]],
             area = unique(y.df$Area)[area_ind[area]])) %>%
  rename(rpr = value) %>%
  group_by(FF, opening, cohort)

rpr_st_summary <- rpr_st_long %>%
  mutate(opening_chr = paste(month.abb[ceiling(opening/2) + 3], 
                             c(15,1)[opening %% 2 + 1]),
         opening_fct = factor(opening_chr, levels = c('Apr 1', 'Apr 15', 'May 1',
                                                      'May 15', 'Jun 1'))) %>%
  group_by(cohort, area, opening_fct, FF) %>%
  summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
            high = quantile(rpr, 0.975), opening = first(opening))

# Save results for later use
save(rpr_base_summary, rpr_by_area_summary, rpr_by_yr_summary, rpr_st_summary, file = here('code/rpr.Rdata'))