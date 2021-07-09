# run model ---------------------------------------------------------------

mod <- stan('Code/growth_model.stan', 
            data = list(N = N, M = M, y = y.vec - mean(y.vec), cohorts = cohorts, 
                        S = max(cohorts), 
                        # obsVariances = obsVariances, 
                        # n_obsvar = max(obsVariances), proVariances = proVariances, 
                        # n_provar = max(proVariances), trends = trends, 
                        # n_trends = max(trends), 
                        n_area = max(areas), area = areas, #area.vec,
                        n_pos = n_pos, col_indx_pos = complete.data[,2], 
                        row_indx_pos = complete.data[,1],
                        calc_ppd = 1, samp_size = n.vec), 
            pars = c('x0_mean', 'sigma_x0', 'x0', 'area_offset', 'sigma_area', 'U', 'U_season', 'B', 'sigma_process', 'sigma_obs', 
                     'pred_vec', 'pro_dev', 'y_pp'), 
            chains = 3, iter = mcmc_list$n_mcmc, cores = 3,
            #thin = mcmc_list$n_thin, warmup = mcmc_list$n_burn,
            control = list(adapt_delta = 0.9, max_treedepth = 10))
save(mod, file = here('Code/model_fit.RData'))

pairs(mod, pars = c('x0_mean', 'sigma_x0', 'sigma_area', 'U', 'U_season', 'B', 'sigma_process', 'sigma_obs', 'lp__'))
xx <- as.shinystan(mod)
launch_shinystan(drop_parameters(xx, pars = c('pro_dev', 'y_pp', 'pred_vec')))
xx <- summary(mod, pars = c('x0_mean', 'sigma_x0', 'sigma_area', 'U', 'U_season', 'B', 'sigma_process', 'sigma_obs'))$summary

summary(mod.vague.priors, pars = c('x0_mean', 'sigma_x0', 'sigma_area', 'U', 'U_season', 'B', 'sigma_process', 'sigma_obs', 'lp__'))$summary

mod.spatiotemporal <- stan('Code/growth_model_spatiotemporal.stan', 
                           data = list(N = N, M = M, y = y.vec - mean(y.vec), cohorts = cohorts, 
                                       S = max(cohorts), 
                                       # obsVariances = obsVariances, 
                                       # n_obsvar = max(obsVariances), proVariances = proVariances, 
                                       # n_provar = max(proVariances), trends = trends, 
                                       # n_trends = max(trends), 
                                       n_area = max(areas), area = areas, #area.vec,
                                       n_pos = n_pos, col_indx_pos = complete.data[,2], 
                                       row_indx_pos = complete.data[,1], 
                                       return_preds = 1, calc_ppd = 0,
                                       area_centers = area.locations$Centroid[1:max(areas)]), # There is an NA at the end of the data frame because the centroids are diff(latitude)
                           pars = c('x0_mean', 'sigma_x0', 'x0', 'area_offset', 'sigma_area', 'U', 'U_season', 'B', 'sigma_process', 'sigma_obs', 'gp_theta', 'gp_sigma', 
                                    'pred_vec', 'pro_dev'), 
                           chains = 3, iter = mcmc_list$n_mcmc, cores = 3,
                           thin = mcmc_list$n_thin, warmup = mcmc_list$n_burn,
                           control = list(adapt_delta = 0.9, max_treedepth = 15))
pairs(mod.spatiotemporal, pars = c('x0_mean', 'sigma_x0', 'sigma_area', 'rho', 'alpha', 'B', 'sigma_process', 'sigma_obs', 'lp__'))
launch_shinystan(mod.spatiotemporal, )


# post-processing ---------------------------------------------------------

## process deviations

list_of_draws <- rstan::extract(mod)

pro.dev <- list_of_draws$pro_dev %>%
  apply(2:3, median) %>% 
  as_tibble() %>%
  rename_with(~ 1986:2017) %>%
  mutate(Age_Month = names(y.df)[-c(1:2,21)]) %>%
  pivot_longer(cols = -Age_Month, names_to = 'Year_Class', values_to = 'pro.dev') %>%
  mutate(Year_Class = as.numeric(Year_Class))

## initial size deviations

x0.dev <- list_of_draws$x0 %>%
  apply(2, median) %>% 
  tibble() %>%
  rename_with(~ 'x0') %>%
  mutate(x0 = x0 + mean(y.mat, na.rm = TRUE)) %>%
  bind_cols(Year_Class = sort(unique(y.df$Year_Class))) %>%
  rename_with(~ 'x0', .cols = -last_col()) %>%
  mutate(Year = Year_Class + 1) %>% #ack is this right!?!
  left_join(ssh) %>%
  left_join(biomass)

 ## observation errors

med.pred <- list_of_draws$pred %>%
  apply(2, median)
pred.mat <- matrix(nrow = nrow(y.mat), ncol = ncol(y.mat), 
                   dimnames = list(rownames(y.mat), colnames(y.mat)))
for(ii in 1:nrow(complete.data)) {
  pred.mat[complete.data[ii,1], complete.data[ii,2]] <- med.pred[ii] 
}

length.fits <- as_tibble(pred.mat + mean(y.mat, na.rm=TRUE)) %>%
  bind_cols(select(y.df, Area, Year_Class)) %>%
  gather(key = 'Age_Month', value = 'MedPredLength', -Area, -Year_Class) %>%
  mutate(Age_Month = as.numeric(Age_Month),
         Age = floor(Age_Month),
         Month_Num = round(12*(Age_Month - Age))) %>% 
  select(-Age_Month) %>%
  right_join(lengths)

save(length.fits, pro.dev, x0.dev, file = here('Code/model_fit_tables.RData'))


# Plotting ----------------------------------------------------------------

# process error boxplot by age
pro.dev %>%
  filter(Age_Month %% 1 > 0.3 & Age_Month %% 1 < 0.9) %>% # filter out months with no data
  mutate(Age_Month = round(Age_Month, 2)) %>%
  ggplot() +
  geom_boxplot(aes(x = factor(Age_Month), y = pro.dev)) +
  geom_hline(aes(yintercept = 0), col = 'red') +
  xlab('Age (yrs)') +
  ylab('Median residual') +
  theme_bw(base_size = 18)

## plots of x0 vs ssh and recruitment

ssh.plot <- ggplot(x0.dev) +
  geom_point(aes(x = std_MSL, y = x0), cex=2.5) +
  labs(x = 'Standardized Sea Level Height\n(Upwelling)', y = 'Recruitment size (mm)') +
  theme_classic(base_size = 18)

bio.plot <- ggplot(x0.dev) +
  #geom_point(aes(x = Std_R, y = x0, pch = as.character(Year)), cex=2.5) +
  labs(x = 'Standardized Recruitment\n', y = '') +
  theme_classic(base_size = 18) +
  geom_text(aes(x = Std_R, y = x0, label = Year))

png('Figures/covariates.png', width = 10, height = 5, units = 'in', res=500)
gridExtra::grid.arrange(ssh.plot, bio.plot, nrow = 1)
dev.off()



pro.dev %>%
  mutate(Year = Year_Class + floor(Age_Month)) %>%
  left_join(biomass) %>%
  left_join(ssh) %>%
  gather(key = 'covariate', value = 'value', std_Log_Biomass, std_MSL) %>%
  ggplot() +
  geom_point(aes(x = value, y = pro.dev)) +
  facet_wrap(~covariate, scales = 'free_x')

ppd <- list_of_draws$y_pp %>%
  plyr::alply(1, function(mcmc.draw) {
    to.return <- matrix(nrow = nrow(y.mat), ncol = ncol(y.mat),
                        dimnames = list(rownames(y.mat), colnames(y.mat)))
    for(ii in 1:nrow(complete.data)) {
      to.return[complete.data[ii,1], complete.data[ii,2]] <- mcmc.draw[ii]
    }
    as_tibble(to.return) %>%
      bind_cols(select(y.df, Area, Year_Class))
  }) %>%
  bind_rows(.id = 'mcmc_draw') %>%
  gather(key = 'Age_Month', value = 'PP_Length', -Area, -Year_Class, -mcmc_draw) %>%
  na.omit() %>%
  mutate(Age_Month = round(as.numeric(Age_Month), 3))

png('Figures/month_resids_bounded.png', width = 14, height = 6, units = 'in', res = 500)
pro.dev %>%
  #  filter(Age_Month %% 1 > 0.3 & Age_Month %% 1 < 0.9) %>% # filter out months with no data
  ggplot() +
  geom_boxplot(aes(x = factor(round(Age_Month, 2)), y = pro.dev)) +
  geom_hline(yintercept = 0, col = 'red') +
  ylab('Median residual') +
  xlab('Month') +
  theme_bw(base_size = 18) +
  NULL
dev.off()

test <- mutate(lengths, Age_Month = round(Age_Month, 3),
               Demean_Length = Avg_Len - mean(Avg_Len)) %>%
  right_join(ppd)

filter(test, mcmc_draw %in% sample(3000, 100, replace = FALSE)) %>%
  ggplot(aes(x = Age_Month)) +
  geom_line(aes(y = PP_Length, group = paste(Year_Class, mcmc_draw)),
            lwd = 0.1, alpha = 0.25) +
  geom_line(aes(y = Demean_Length, group = Year_Class, col = Year_Class))+
  # geom_path(aes(x = Age_Month, y = Avg_Len, group = paste(Year_Class, Area)), data = lengths) +
  facet_wrap(~Area)

test %>%
  group_by(Year_Class, Age_Month, Area) %>%
  summarize(PPD_max = max(PP_Length),
            PPD_min = )
labs(x = 'Age (years)', y = 'Mean Length (mm)', color = 'Cohort') +
  NULL
# this can get fancier...

qplot(N, Avg_Len - MedPredLength, data = length.fits, alpha = .5) + 
  stat_smooth() +
  geom_hline(yintercept = 0, col = 'red')
# will a lognormal dist'n fix this skew?

ggplot(length.fits) +
  geom_boxplot(aes(x = factor(Year), y = Avg_Len - MedPredLength))
# need to fix this pattern. Estimate a mean for the observation process for each area?

tibble(med.pred, MeanLength = ordered.y)[order(complete.data.mat[,1]),] %>%
  bind_cols(tibble(N = n.vec)) %>%
  right_join(lengths) %>%
  group_by(MeanLength, N) %>%
  summarize(n = n()) %>% 
  arrange(desc(n))

filter(lengths, AgeClass > 0)  %>%
  ggplot(aes(x = MonthNum, y = MeanLength, group = paste(Fyear, Area), col = factor(Area))) +
  geom_path() +
  geom_point() +
  facet_wrap(~AgeClass, scales = 'free_y') +
  NULL
# Areas 30 and 32 have small shrimp
# Age 3 is very variable, probably small sample sizes.

filter(lengths, Fmonth=='April' | Fmonth=='October', !is.na(MeanLength)) %>%
  mutate(newyear = ifelse(Fmonth=='April', Fyear-1, Fyear),
         newage = ifelse(Fmonth=='April', AgeClass-1, AgeClass)) %>% 
  ggplot(aes(x = -MonthNum+14, y = MeanLength, group = paste(newage, newyear), col = factor(newyear))) +
  geom_path() +
  # geom_point() +
  facet_wrap(~Area) +
  NULL

filter(lengths, AgeClass > 0, !is.na(MeanLength)) %>%
  ggplot(aes(x = MonthNum, y = MeanLength, group = paste(AgeClass, Fyear), col = factor(Fyear))) +
  geom_path() +
  # geom_point() +
  facet_wrap(~Area) +
  NULL

summary(mod, pars = c('x0_mean', 'x0_sd', 'U', 'sigma_process', 'sigma_obs'))$summary

ggplot(length.fits, aes(x = Age_Month, y = Avg_Len, group = paste(Year_Class, Age), col = Year_Class)) +
  geom_path(lwd = .25, alpha = .5) +
  # geom_point(cex=.5) +
  facet_wrap(~Area) +
  labs(x = 'Month', y = 'Mean Length (mm)', color = 'Year Class', lty = 'Age') +
  NULL

ggplot(length.fits, aes(x = Age_Month, y = MedPredLength, group = Year_Class, col = Year_Class)) +
  geom_path(alpha = 0.5) +
  # geom_point(alpha = .5) +
  # labs(x = 'Month', y = 'Mean Length (mm)', color = 'Year Class', lty = 'Age') +
  # geom_hline(yintercept = 0, col = 'red') +
  # stat_smooth() +
  # facet_grid(Month_Num~Age) +
  NULL


# covariate analysis ------------------------------------------------------

complete.data <- which(!is.na(y.mat) & !is.na(bio.mat) & !is.na(ssh.mat), arr.ind = TRUE)
n_pos <- nrow(complete.data)
y.vec <- y.mat[which(!is.na(y.mat))]
# area.vec <- area.mat[which(!is.na(y.mat))]

cohorts <- as.numeric(as.factor(y.df$Year_Class))
areas <- as.numeric(as.factor(y.df$Area))

mcmc_list <- list(n_mcmc = 3000, n_burn = 900, n_thin = 1)

mod <- stan('Code/growth_model_predictors.stan', 
            data = list(N = N, M = M, y = y.vec - mean(y.vec), 
                        bio = bio.mat, ssh = ssh.mat,
                        cohorts = cohorts, 
                        S = max(cohorts), 
                        # obsVariances = obsVariances, 
                        # n_obsvar = max(obsVariances), proVariances = proVariances, 
                        # n_provar = max(proVariances), trends = trends, 
                        # n_trends = max(trends), 
                        n_area = max(area.vec), area = areas, #area.vec,
                        n_pos = n_pos, col_indx_pos = complete.data[,2], 
                        row_indx_pos = complete.data[,1], 
                        return_preds = 1, calc_ppd = 1, winter_ind = winter_ind), 
            pars = c('x0_mean', 'sigma_x0', 'area_offset', 'sigma_area', 'U', 'U_season', 'U_bio', 'U_ssh', 'B', 'sigma_process', 'sigma_obs', 
                     'pred_vec', 'pro_dev'), 
            chains = 3, iter = mcmc_list$n_mcmc, cores = 3,
            thin = mcmc_list$n_thin,
            control = list(adapt_delta = 0.9, max_treedepth = 15))

