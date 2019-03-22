library(readxl)
library(tidyverse)

# I don't have the heart to delete this code yet.
age.comp <- read_excel('Data/AGETAB2000.xls', range = 'A4:I81', na = '--') %>%
  filter(!is.na(Age)) %>% # remove blank rows
  rename(State_Area = `State Area`) %>%
  mutate(State_Area = rep(as.vector(na.omit(State_Area)), each = 6)) %>%
  gather(key = 'Month', value = 'value', -State_Area, -Age) %>%
  spread(key = Age, value = value) %>%
  gather(key = 'Age', value = 'Pct_comp', `0`:`3`, na.rm=TRUE)

length <- read_excel('Data/2000mltable.xls', range = 'A5:H71', na = '--') %>%
  filter(!is.na(X__1)) %>% # remove blank rows
  mutate(State_Area = rep(gsub('MEAN LENGTHS - area ', '', grep('MEAN', X__1, value = TRUE)), each = 5)) %>%
  slice(-grep('MEAN', X__1)) %>%  # remove more blank rows
  mutate(Age = gsub('\\+', '', gsub("'S", '', X__1))) %>%
  select(-X__1) %>% # this info has been put into Age column
  gather(key = 'Month', value = 'Avg_len', -State_Area, -Age)

# Actually run this code
age_comp <- read_excel('Data/shrimp age comp and count.xlsx') %>%
  gather(key = 'AgeClass', value = 'PctComp', `Age 0`:`Age 3`) %>%
  select(-ID) %>%
  rename(Area = `State Area`) %>%
  mutate(AgeClass = as.numeric(gsub('Age ', '', AgeClass)))

indiv_data <- read_excel('Data/2014-2018 OR pink shrimp.xlsx') %>%
  group_by(Age, Area, Year, Month) %>%
  summarize(MeanLength = mean(CL),
            N = n(),
            sd = sd(CL)) %>%
  rename(Fyear = Year, MonthNum = Month, AgeClass = Age) %>%
  filter(AgeClass > 0)
            

lengths <- read_excel('Data/shrimp mean carapace length (mm).xlsx') %>%
  gather(key = 'AgeClass', value = 'MeanLength', `Age 0`:`Age 3`) %>%
  select(-ID) %>%
  mutate(AgeClass = as.numeric(gsub('Age ', '', AgeClass))) %>%
  right_join(age_comp, by = c('Fyear', 'Area', 'Fmonth', 'AgeClass')) %>%
  mutate(MonthNum = sapply(Fmonth, function(x) switch(x, April = 4,
                           May = 5,
                           June = 6,
                           July = 7,
                           August = 8,
                           September = 9,
                           October = 10))) %>%
  filter(AgeClass > 0, !is.na(MeanLength))  %>%
  bind_rows(indiv_data) %>%  
  mutate(YearClass = Fyear - AgeClass,
         AgeMonth = AgeClass + MonthNum/12) 


# Setting up for Stan model
y.df <- lengths %>%
  select(-Fmonth, -AgeClass, -MonthNum, -PctComp, -Ct, -Fyear, -N) %>%
  spread(key = AgeMonth, value = MeanLength)

y.mat <- y.df %>%
  select(-(Area:YearClass)) %>%
  as.matrix()

n.mat <- lengths %>%
  filter(!is.na(MeanLength)) %>%
  select(-Fmonth, -AgeClass, -MonthNum, -PctComp, -Ct, -Fyear, -MeanLength) %>%
  spread(key = AgeMonth, value = N) %>%
  select(-(Area:YearClass)) %>%
  as.matrix()

N <- ncol(y.mat)
M <- nrow(y.mat)

complete.data <- which(!is.na(y.mat), arr.ind = TRUE)
n_pos <- nrow(complete.data)
y.vec <- y.mat[which(!is.na(y.mat))]
n.vec <- n.mat[which(!is.na(y.mat))]

states <- as.numeric(as.factor(y.df$YearClass))

mcmc_list <- list(n_mcmc = 1000, n_burn = 500, 
                 n_chain = 3, n_thin = 1, adapt)

mod <- stan('Code/growth_model.stan', 
            data = list(N = N, M = M, y = y.vec, states = states, 
                        S = max(states), 
                        # obsVariances = obsVariances, 
                        # n_obsvar = max(obsVariances), proVariances = proVariances, 
                        # n_provar = max(proVariances), trends = trends, 
                        # n_trends = max(trends), 
                        n_pos = n_pos, col_indx_pos = complete.data[,2], 
                        row_indx_pos = complete.data[,1], y_int = round(y.vec), samp_size = n.vec), 
            pars = c('x0_mean', 'U', 'sigma_process', 'sigma_obs', 'pred'), 
            chains = 3, iter = mcmc_list$n_mcmc, cores = 3,
            thin = mcmc_list$n_thin,
            control = list(adapt_delta = 0.9, max_treedepth = 10))

complete.data.vec <- which(t(!is.na(y.mat)))
complete.data.mat <- which(t(!is.na(y.mat)), arr.ind = TRUE)
head(cbind(complete.data.mat, complete.data.vec), 100)

pred <- as.matrix(mod)[,complete.data.vec + 4] # add 4 because 1st 4 cols are other parameters
ordered.y <- na.omit(as.vector(t(y.mat)))
med.pred <- apply(pred, 2, median)
plot(med.pred, ordered.y - med.pred)

# will a lognormal dist'n fix this skew?

tibble(med.pred, MeanLength = ordered.y)

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


