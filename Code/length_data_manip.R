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
  select(-Month, -Age, -Month_Num, -Year, -N, -sd) %>%
  spread(key = Age_Month, value = Avg_Len)

y.mat <- y.df %>%
  select(-(Area:Year_Class)) %>%
  as.matrix()

n.mat <- lengths %>%
  select(-Month, -Age, -Month_Num, -Year, -Avg_Len, -sd) %>%
  spread(key = Age_Month, value = N) %>%
  select(-(Area:Year_Class)) %>%
  as.matrix()

N <- ncol(y.mat)
M <- nrow(y.mat)

complete.data <- which(!is.na(y.mat), arr.ind = TRUE)
n_pos <- nrow(complete.data)
y.vec <- y.mat[which(!is.na(y.mat))]
n.vec <- n.mat[which(!is.na(y.mat))]

states <- as.numeric(as.factor(y.df$Year_Class))

mcmc_list <- list(n_mcmc = 1000, n_burn = 500, n_thin = 1)

mod <- stan('Code/growth_model.stan', 
            data = list(N = N, M = M, y = y.vec, states = states, 
                        S = max(states), 
                        # obsVariances = obsVariances, 
                        # n_obsvar = max(obsVariances), proVariances = proVariances, 
                        # n_provar = max(proVariances), trends = trends, 
                        # n_trends = max(trends), 
                        n_pos = n_pos, col_indx_pos = complete.data[,2], 
                        row_indx_pos = complete.data[,1], y_int = round(y.vec), samp_size = n.vec), 
            pars = c('x0_mean', 'x0_sd', 'U', 'sigma_process', 'sigma_obs', 'pred_vec'), 
            chains = 3, iter = mcmc_list$n_mcmc, cores = 3,
            thin = mcmc_list$n_thin,
            control = list(adapt_delta = 0.9, max_treedepth = 10))

pred <- as.matrix(mod)[,grep('pred', colnames(as.matrix(mod)))]
med.pred <- apply(pred, 2, median)
pred.mat <- matrix(nrow = nrow(y.mat), ncol = ncol(y.mat), 
                   dimnames = list(rownames(y.mat), colnames(y.mat)))

for(ii in 1:nrow(complete.data)) {
  pred.mat[complete.data[ii,1], complete.data[ii,2]] <- med.pred[ii]
}

length.fits <- as_tibble(pred.mat) %>%
  bind_cols(select(y.df, Area, Year_Class)) %>%
  gather(key = 'Age_Month', value = 'MedPredLength', -Area, -Year_Class) %>%
  mutate(Age_Month = as.numeric(Age_Month),
         Age = floor(Age_Month),
         Month_Num = round(12*(Age_Month - Age))) %>% 
  select(-Age_Month) %>%
  right_join(lengths)

# this can get fancier...

qplot(MedPredLength, Avg_Len - MedPredLength, data = length.fits, alpha = .5) + stat_smooth()
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


