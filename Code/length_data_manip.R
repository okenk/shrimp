library(readxl)
library(tidyverse)
library(rstan)
library(shinystan)

# Deprecated
# age.comp <- read_excel('Data/AGETAB2000.xls', range = 'A4:I81', na = '--') %>%
#   filter(!is.na(Age)) %>% # remove blank rows
#   rename(State_Area = `State Area`) %>%
#   mutate(State_Area = rep(as.vector(na.omit(State_Area)), each = 6)) %>%
#   gather(key = 'Month', value = 'value', -State_Area, -Age) %>%
#   spread(key = Age, value = value) %>%
#   gather(key = 'Age', value = 'Pct_comp', `0`:`3`, na.rm=TRUE)

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

# Decprecated
# lengths <- read_excel('Data/shrimp mean carapace length (mm).xlsx') %>%
#   gather(key = 'AgeClass', value = 'MeanLength', `Age 0`:`Age 3`) %>%
#   select(-ID) %>%
#   mutate(AgeClass = as.numeric(gsub('Age ', '', AgeClass))) %>%
#   right_join(age_comp, by = c('Fyear', 'Area', 'Fmonth', 'AgeClass')) %>%
#   mutate(MonthNum = sapply(Fmonth, function(x) switch(x, April = 4,
#                            May = 5,
#                            June = 6,
#                            July = 7,
#                            August = 8,
#                            September = 9,
#                            October = 10))) %>%
#   filter(AgeClass > 0, !is.na(MeanLength))  %>%
#   bind_rows(indiv_data) %>%
#   mutate(YearClass = Fyear - AgeClass,
#          AgeMonth = AgeClass + MonthNum/12)

lengths <- read_csv('Data/Compiled_Lengths.csv', 
                    col_types = 'cdcddddd') %>%
  mutate(Year_Class = Year - Age,
       Age_Month = Age + Month_Num/12) %>%
  left_join(age_comp[,c('Year', 'Area', 'Month', 'N2', 'Age')]) %>%
  mutate(N = ifelse(is.na(N), N2, N)) %>%
  filter(!is.na(N), N > 0, Age > 0) %>% 
  # I am concerned about the N filtering-- There is an error somewhere and this is a workaround to get code to run,
  # but is not solving the error. Currently excluding 8 rows due to filtering, but others might be wrong.
  select(-N2)

# Setting up for Stan model
y.df <- lengths %>%
  bind_rows(tibble(Age_Month = c(1+11/12, 2 + c(0, 1/12, 2/12, 3/12, 11/12), 3+c(0, 1/12, 2/12, 3/12)),
                   Year_Class = 2015, Area = as.character(12))) %>% # This adds columns for wintertime months
  select(-Month, -Age, -Month_Num, -Year, -N, -sd) %>%
  spread(key = Age_Month, value = Avg_Len)

y.mat <- y.df %>%
  select(-(Area:Year_Class)) %>%
  as.matrix()

n.mat <- lengths %>%
  bind_rows(tibble(Age_Month = c(1+11/12, 2 + c(0, 1/12, 2/12, 3/12, 11/12), 3+c(0, 1/12, 2/12, 3/12)),
                   Year_Class = 2015, Area = as.character(12))) %>% # This adds columns for wintertime months
  select(-Month, -Age, -Month_Num, -Year, -Avg_Len, -sd) %>%
  spread(key = Age_Month, value = N) %>%
  select(-(Area:Year_Class)) %>%
  as.matrix()

N <- ncol(y.mat)
M <- nrow(y.mat)

area.mat <- as.numeric(as.factor(y.df$Area)) %>%
                         rep(N) %>%
                         matrix(nrow = M, ncol = N)

complete.data <- which(!is.na(y.mat), arr.ind = TRUE)
n_pos <- nrow(complete.data)
y.vec <- y.mat[which(!is.na(y.mat))]
n.vec <- n.mat[which(!is.na(y.mat))]
area.vec <- area.mat[which(!is.na(y.mat))]
  
states <- as.numeric(as.factor(y.df$Year_Class))

mcmc_list <- list(n_mcmc = 2000, n_burn = 1000, n_thin = 1)

mod <- stan('Code/growth_model.stan', 
            data = list(N = N, M = M, y = y.vec - mean(y.vec), states = states, 
                        S = max(states), 
                        # obsVariances = obsVariances, 
                        # n_obsvar = max(obsVariances), proVariances = proVariances, 
                        # n_provar = max(proVariances), trends = trends, 
                        # n_trends = max(trends), 
                        n_area = max(area.vec), area = area.vec,
                        n_pos = n_pos, col_indx_pos = complete.data[,2], 
                        row_indx_pos = complete.data[,1], samp_size = n.vec,
                        return_preds = 1, calc_ppd = 1), 
            pars = c('x0_mean', 'sigma_x0', 'area_offset', 'sigma_area', 'U', 'B', 'sigma_process', 'sigma_obs', 
                     'pred_vec', 'y_pp'), 
            chains = 3, iter = mcmc_list$n_mcmc, cores = 3,
            thin = mcmc_list$n_thin,
            control = list(adapt_delta = 0.9, max_treedepth = 10))

med.pred <- as.matrix(mod)[,grep('pred', colnames(as.matrix(mod)))] %>%
  apply(2, median)
area.offset <- as.matrix(mod)[,grep('area_', colnames(as.matrix(mod)))] %>%
  apply(2, median)
pred.mat <- matrix(nrow = nrow(y.mat), ncol = ncol(y.mat), 
                   dimnames = list(rownames(y.mat), colnames(y.mat)))

# pro.dev <- as.matrix(mod)[,grep('pro_dev', colnames(as.matrix(mod)))] %>%
#   apply(2, median) %>%
#   matrix(nrow = N-1, dimnames = list(Age_Month = names(y.df)[-c(1:2,21)], Year_Class = 1986:2017)) %>%
#   reshape2::melt(value.name = 'pro.dev') %>%
#   as_tibble()

for(ii in 1:nrow(complete.data)) {
  pred.mat[complete.data[ii,1], complete.data[ii,2]] <- med.pred[ii] + area.offset[area.vec[ii]]
}

length.fits <- as_tibble(pred.mat) %>%
  bind_cols(select(y.df, Area, Year_Class)) %>%
  gather(key = 'Age_Month', value = 'MedPredLength', -Area, -Year_Class) %>%
  mutate(Age_Month = as.numeric(Age_Month),
         Age = floor(Age_Month),
         Month_Num = round(12*(Age_Month - Age))) %>% 
  select(-Age_Month) %>%
  right_join(lengths)

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

qplot(MedPredLength, Avg_Len - MedPredLength, data = length.fits, alpha = .5) + 
  stat_smooth() +
  geom_hline(yintercept = 0, col = 'red')
# will a lognormal dist'n fix this skew?

ggplot(length.fits) +
  geom_boxplot(aes(x = Area, y = Avg_Len - MedPredLength))
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

