library(readxl)
library(tidyverse)
library(rstan)
library(shinystan)


# preparing data ----------------------------------------------------------

age_comp <- read_excel('Data/shrimp age comp and count.xlsx') %>%
  gather(key = 'Age', value = 'Pct_Comp', `Age 0`:`Age 3`) %>%
  select(-ID) %>%
  rename(Area = `State Area`,
         Year = Fyear,
         Month = Fmonth) %>%
  mutate(Age = as.numeric(gsub('Age ', '', Age)),
         Month = str_sub(str_to_upper(Month), end = 3),
         Area = as.character(Area),
         N2 = ceiling(N*Pct_Comp))

ssh <- read_csv('Data/CC_SSH.csv') %>%
  filter(Month == 4, Year >= 1986) %>% # April SSH, length data starts in 1989 
  mutate(std_MSL = (Monthly_MSL - mean(Monthly_MSL, na.rm = TRUE))/sd(Monthly_MSL, na.rm = TRUE)) %>%
  select(Year, std_MSL) 

standardize_vec <- function(vec) {
  (vec - mean(vec, na.rm = TRUE)) / sd(vec, na.rm = TRUE)
}

biomass <- read_excel('Data/OR-VPE.XLSX') %>%
  mutate('Year' = `Spawn Year`, # this naming scheme makes no sense to me, but I *think* spawn year is the year the cohort is age 1.
         Std_Log_Bio = standardize_vec(`OR log VPE`),
         Std_Bio = standardize_vec(`OR VPE calculated`),
         Std_R = standardize_vec(`OR Age 1`)) %>%
  select(Year, Std_Log_Bio, Std_Bio, Std_R, `North Log VPE`, `South Log VPE`) %>%
  filter(!is.na(Std_Log_Bio))

lengths <- read_csv('Data/Compiled_Lengths.csv', 
                    col_types = 'cdcddddd') %>%
  mutate(Year_Class = Year - Age, # Year class is year at age 0, i.e., the year where they start settling in fall
       Age_Month = Age + Month_Num/12) %>%
  left_join(age_comp[,c('Year', 'Area', 'Month', 'N2', 'Age')]) %>%
  mutate(N = ifelse(is.na(N), N2, N)) %>%
  filter(N > 0, Age > 0, !is.na(N)) %>% 
  # I am concerned about the N filtering-- There is an error somewhere and this is a workaround to get code to run,
  # but is not solving the error. Currently excluding 8 rows due to filtering, but others might be wrong.
  # The issue is that there are a few area-year-month-ages that have an average length, but the
  # corresponding area-year-month does not have age comp data. Because I am no longer using N for the obs error,
  # I stopped filtering the NAs in sample size (N) out.
  select(-N2) %>%
  left_join(biomass) %>%
  mutate(Log_Bio_Regional = ifelse(Area <= 23, `South Log VPE`, `North Log VPE`),
         Std_Log_Bio_Regional = (Log_Bio_Regional - mean(Log_Bio_Regional, na.rm = TRUE))/sd(Log_Bio_Regional, na.rm = TRUE)) %>%
  # the VPEs are for OR only, length data include WA and CA! Use N. index for WA and S. index for CA
  select(-`North Log VPE`, -`South Log VPE`, -Log_Bio_Regional) %>%
  left_join(ssh)

# Setting up for Stan model
y.df <- lengths %>%
  select(-(Std_Log_Bio:std_MSL), -N) %>%
  bind_rows(tibble(Age_Month = c(1+11/12, 2 + c(0, 1/12, 2/12, 3/12, 11/12), 3+c(0, 1/12, 2/12, 3/12)),
                   Year_Class = 2015, Area = as.character(12))) %>% # This adds columns for wintertime months
  select(-Month, -Age, -Month_Num, -Year, -sd) %>%
  spread(key = Age_Month, value = Avg_Len) 

y.mat <- y.df %>%
  select(-(Area:Year_Class)) %>%
  as.matrix()

n.mat <- lengths %>%
  bind_rows(tibble(Age_Month = c(1+11/12, 2 + c(0, 1/12, 2/12, 3/12, 11/12), 3+c(0, 1/12, 2/12, 3/12)),
                   Year_Class = 2015, Area = as.character(12))) %>% # This adds columns for wintertime months
  select(-(Age:Month_Num), -sd, -(Std_Log_Bio:std_MSL)) %>%
  spread(key = Age_Month, value = N) %>%
  select(-(Area:Year_Class)) %>%
  as.matrix()

N <- ncol(y.mat)
M <- nrow(y.mat)

area.mat <- as.numeric(as.factor(y.df$Area)) %>%
                         rep(N) %>%
                         matrix(nrow = M, ncol = N)

to.add <- names(y.df)[-(1:2)] %>%
  as.numeric() %>%
  floor()

year.df <- y.df
year.df[,-(1:2)] <- sapply(y.df$Year_Class, function(x) x + to.add) %>% t()

ssh.mat <- bio.mat <- year.df %>%
  mutate_at(vars(-(Area:Year_Class)), function(.x) as.numeric(NA)) %>%
  as.matrix 
class(ssh.mat) <- class(bio.mat) <- 'numeric'

# I don't think these are right. Need to reinvestigate to actually run model.
for(yr in unique(ssh$Year)) {
  ssh.mat[which(year.df == yr)] <- ssh$std_MSL[ssh$Year == yr]
  if(length(biomass$Std_Log_Bio[biomass$Year == yr]) == 0) {
    bio.mat[which(year.df == yr)] <- NA
    print(yr)
  } else {
    bio.mat[which(year.df == yr)] <- biomass$Std_Log_Bio[biomass$Year == yr]
  }
}

ssh.mat <- ssh.mat[,-(1:2)]
bio.mat <- bio.mat[,-(1:2)]

complete.data <- which(!is.na(y.mat), arr.ind = TRUE)
n_pos <- nrow(complete.data)
y.vec <- y.mat[which(!is.na(y.mat))]
n.vec <- n.mat[which(!is.na(y.mat))]
# area.vec <- area.mat[which(!is.na(y.mat))]

cohorts <- as.numeric(as.factor(y.df$Year_Class))
areas <- as.numeric(as.factor(y.df$Area))

area.locations <- read_csv('Data/area_locations.csv') %>%
  arrange(Area)

sex_ratios <-  read_csv('Data/sex_ratios.csv') %>%
  select(year:`age 2 males`) %>%
  filter(year >= 1986, year <= 2017) %>%
  group_by(year) %>%
  summarize(n_age1 = sum(`age 1 all`),
            n_age1f = sum(`age1 fems+trans`),
            n_age2 = sum(`age 2 all`),
            n_age2f = n_age2 - sum(`age 2 males`)) %>%
  mutate(prop_1f = n_age1f/n_age1,
         prop_2f = n_age2f/n_age2)

sex_ratio_mat <- matrix(0, nrow = max(cohorts), ncol = N, dimnames = list(cohort = NULL, month = NULL))
sex_ratio_mat[,1:12] <- sex_ratios$prop_1f
sex_ratio_mat[-nrow(sex_ratio_mat), 13:24] <- sex_ratios$prop_2f[-1]
# Don't fill the last row of matrix, no length data for later ages of that cohort
# Don't include the first row of df, first cohort is 1986 (sampled in 1989), no lengths for age 2+ in 1986
sex_ratio_mat[-nrow(sex_ratio_mat)+0:1, 25:ncol(sex_ratio_mat)] <- sex_ratios$prop_2f[-(1:2)]
# Also checked that numbers get repeated row-wise and are unique column-wise, which is correct.

mcmc_list <- list(n_mcmc = 3000)
mcmc_list <- list(n_mcmc =300, n_burn = 200, n_thin = 1)


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
            control = list(adapt_delta = 0.9, max_treedepth = 10), )
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



# explore output ----------------------------------------------------------


pro.dev <- as.matrix(mod)[,grep('pro_dev', colnames(as.matrix(mod)))] %>%
  apply(2, median) %>% 
  # array(dim = c(N-1, max(cohorts)), dimnames = list(Age_Month = names(y.df)[-c(1:2,21)], Year_Class = 1986:2017)) %>%
  matrix(nrow = N-1, dimnames = list(Age_Month = names(y.df)[-c(1:2,21)], Year_Class = 1986:2017), byrow=FALSE) %>%
  reshape2::melt(value.name = 'pro.dev') %>%
  as_tibble()
  
pro.dev.st <- as.matrix(mod.spatiotemporal)[,grep('pro_dev', colnames(as.matrix(mod.spatiotemporal)))] %>%
  apply(2, median) %>% 
  array(dim = c(N-1, max(cohorts),  max(areas)), dimnames = list(Age_Month = names(y.df)[-c(1:2,21)], Year_Class = 1986:2017), Area = 1:max(areas)) %>%
  # matrix(nrow = N-1, dimnames = list(Age_Month = names(y.df)[-c(1:2,21)], Year_Class = 1986:2017)) %>%
  reshape2::melt(value.name = 'pro.dev') %>%
  as_tibble()

pro.dev %>%
  filter(Age_Month %% 1 > 0.3 & Age_Month %% 1 < 0.9) %>% # filter out months with no data
  mutate(Age_Month = round(Age_Month, 2)) %>%
  ggplot() +
  geom_boxplot(aes(x = factor(Age_Month), y = pro.dev)) +
  geom_hline(aes(yintercept = 0), col = 'red') +
  xlab('Age (yrs)') +
  ylab('Median residual') +
  theme_bw(base_size = 18)

x0.dev <- as.matrix(mod)[,grep('x0\\[', colnames(as.matrix(mod)))] %>%
  apply(2, median) + mean(y.mat, na.rm = TRUE) 

x0.dev <- bind_cols(x0 = x0.dev, Year_Class = sort(unique(y.df$Year_Class))) %>%
  as_tibble %>%
  mutate(Year = Year_Class + 1) %>%
  left_join(ssh) %>%
  left_join(biomass)

ssh.plot <- ggplot(x0.dev) +
  geom_point(aes(x = std_MSL, y = x0), cex=2.5) +
  labs(x = 'Standardized Sea Level Height\n(Upwelling)', y = 'Recruitment size (mm)') +
  theme_classic(base_size = 18)

bio.plot <- ggplot(x0.dev) +
  geom_point(aes(x = Std_R, y = x0), cex=2.5) +
  labs(x = 'Standardized Recruitment\n', y = '') +
  theme_classic(base_size = 18)

ggplot(x0.dev) +
  geom_point(aes(x = Std_R, y = x0), cex=2.5) +
  labs(x = 'Standardized Recruitment\n', y = '') +
  theme_classic(base_size = 18)


lm(x0 ~ Std_Log_Bio, data = x0.dev) %>% summary

png('Figures/covariates.png', width = 10, height = 5, units = 'in', res=500)
gridExtra::grid.arrange(ssh.plot, bio.plot, nrow = 1)
dev.off()

med.pred <- as.matrix(mod)[,grep('pred', colnames(as.matrix(mod)))] %>%
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

pro.dev %>%
  mutate(Year = Year_Class + floor(Age_Month)) %>%
  left_join(biomass) %>%
  left_join(ssh) %>%
  gather(key = 'covariate', value = 'value', std_Log_Biomass, std_MSL) %>%
  ggplot() +
  geom_point(aes(x = value, y = pro.dev)) +
  facet_wrap(~covariate, scales = 'free_x')

ppd <- as.matrix(mod)[,grep('y_pp', colnames(as.matrix(mod)))] %>%
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

save('med.pred', 'mod', file = 'Code/model_output.RData')
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

save(mod, file = 'Code/mcmc_run.RData')
