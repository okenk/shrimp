library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(rstan)
library(shinystan)
library(here)

# preparing data ----------------------------------------------------------

age_comp <- here('Data/shrimp age comp and count.xlsx') %>%
  read_excel() %>%
  gather(key = 'Age', value = 'Pct_Comp', `Age 0`:`Age 3`) %>%
  select(-ID) %>%
  rename(Area = `State Area`,
         Year = Fyear,
         Month = Fmonth) %>%
  mutate(Age = as.numeric(gsub('Age ', '', Age)),
         Month = str_sub(str_to_upper(Month), end = 3),
         Area = as.character(Area),
         N2 = ceiling(N*Pct_Comp))

ssh <- here('Data/CC_SSH.csv') %>%
  readr::read_csv() %>%
  filter(Month == 4, Year >= 1986) %>% # April SSH, length data starts in 1989 
  mutate(std_MSL = (Monthly_MSL - mean(Monthly_MSL, na.rm = TRUE))/sd(Monthly_MSL, na.rm = TRUE)) %>%
  select(Year, std_MSL) 

standardize_vec <- function(vec) {
  (vec - mean(vec, na.rm = TRUE)) / sd(vec, na.rm = TRUE)
}

biomass <- here('Data/OR-VPE.XLSX') %>%
  read_excel() %>% 
  mutate('Year' = `Spawn Year`, # this naming scheme makes no sense to me, but I *think* spawn year is the year the cohort is age 1.
         Std_Log_Bio = standardize_vec(`OR log VPE`),
         Std_Bio = standardize_vec(`OR VPE calculated`),
         Std_R = standardize_vec(`OR Age 1`)) %>%
  select(Year, Std_Log_Bio, Std_Bio, Std_R, `North Log VPE`, `South Log VPE`, `OR Age 1`) %>%
  filter(!is.na(Std_R))

lengths <- here('Data/Compiled_Lengths.csv') %>%
  readr::read_csv(col_types = 'cdcddddd') %>%
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
  spread(key = Age_Month, value = Avg_Len) %>%
  mutate(Big_Area = case_when(Area <= 18 ~ 3,
                   Area <= 22 ~ 1,
                   Area <= 28 ~ 2,
                   TRUE ~ 4)) %>%
  relocate(Big_Area, .after = Area)

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

to.add <- select(y.df, !(Area:Year_Class)) %>% 
  names() %>%
  as.numeric() %>%
  floor()

year.df <- y.df
year.df[,-(1:3)] <- sapply(y.df$Year_Class, function(x) x + to.add) %>% t()

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
big.areas <- y.df$Big_Area

area.locations <- here('Data/area_locations.csv') %>%
  readr::read_csv() %>%
  arrange(Area)

sex_ratios <-  here('Data/sex_ratios.csv') %>%
  readr::read_csv() %>%
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

ocean_combo <- readr::read_csv(here('data', 'data_roms_glorys.csv')) %>%
  select(-1)

ocean_roms <- readr::read_csv(here('data', 'data_roms.csv')) %>%
  select(-1)
ocean_glorys <- readr::read_csv(here('data', 'data_glorys.csv')) %>%
  select(-1)

covar.all <- tibble(year = y.df$Year_Class, cohorts) %>%
  group_by(year) %>%
  summarize(cohort = first(cohorts)) %>%
  left_join(ocean_combo) %>%
  left_join(select(biomass, year=Year, Std_R, `OR Age 1`)) %>% 
  arrange(cohort) %>% 
  select(-year, -cohort, -maxBLT_brood) %>%
  mutate(across(-`OR Age 1`, ~ (.x - mean(.x))/sd(.x))) %>%
  rename(recruits = `OR Age 1`)

mcmc_list <- list(n_mcmc = 5000)
#mcmc_list <- list(n_mcmc =300, n_burn = 200, n_thin = 1)