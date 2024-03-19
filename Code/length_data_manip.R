library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(rstan)
library(shinystan)
library(here)

# preparing data ----------------------------------------------------------

standardize_vec <- function(vec) {
  (vec - mean(vec, na.rm = TRUE)) / sd(vec, na.rm = TRUE)
}

beuti <- readr::read_csv(here('Data/BEUTI_monthly.csv')) |> 
  select(year, month, `42N`:`47N`) |>
  filter(month >= 3, month <= 6) |> 
  mutate(across(`42N`:`47N`, standardize_vec)) |>
  rowwise() |>
  mutate(all_lat = mean(c_across(`42N`:`47N`))) |>
  group_by(year) |>
  summarise(beuti_45N = mean(`45N`), 
            beuti_all_lat = mean(all_lat)) |>
  ungroup() |>
  mutate(year = year + 1) # upwelling impacts growth the year before they recruit

cuti <- readr::read_csv(here('Data/CUTI_monthly.csv')) |> 
  select(year, month, `42N`:`47N`) |>
  filter(month >= 3, month <= 6) |> 
  mutate(across(`42N`:`47N`, standardize_vec)) |>
  rowwise() |>
  mutate(all_lat = mean(c_across(`42N`:`47N`))) |>
  group_by(year) |>
  summarise(cuti_45N = mean(`45N`), 
            cuti_all_lat = mean(all_lat)) |>
  ungroup() |>
  mutate(year = year + 1) # upwelling impacts growth the year before they recruit

sst <- readr::read_csv(here('data/nceiErsstv5_LonPM180_24fe_b874_defd.csv'), skip = 1) |>
  `names<-`(scan(here('data/nceiErsstv5_LonPM180_24fe_b874_defd.csv'), nlines = 1, sep = ',', what ='character')) |>
  mutate(month = lubridate::month(time),
         year = lubridate::year(time) + 1) |> # year is recruitment year, not larval year
  select(-depth, -time, -ssta) |>
  filter(month >= 3, month <= 8) |>
  group_by(year) |>
  summarise(sst = mean(sst))

biomass <- here('Data/VPE-update-02022024.xlsx') %>%
  read_excel() %>% 
  mutate(Log_R = log(`OR VPE`)) %>%
  select(Year = `Spawn Year`, Log_R) %>%
  filter(!is.na(Log_R))

lengths <- here('Data/Compiled_Lengths.csv') %>%
  readr::read_csv(col_types = 'cdcddddd') %>%
  mutate(Age_Month = Age + Month_Num/12,
         Year_Class = Year - Age + 1) %>% # Year class is year they recruit to fishery @Age 1
  filter(Year_Class >= min(cuti$year),
         Area != '12', # Area 12 was only ever sampled in Sep and Oct, 2018.
         Age != 0) # Not modeling age 0's.

# Setting up for Stan model
y.df <- lengths %>%
  # select(-(Std_Log_Bio:Std_Log_Bio_Regional), -N) %>%
  select(-N) %>%
  bind_rows(tibble(Age_Month = c(1+11/12, 2 + c(0, 1/12, 2/12, 3/12, 11/12), 3+c(0, 1/12, 2/12, 3/12)),
                   Year_Class = 2015, Area = as.character(18))) %>% # This adds columns for wintertime months
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
year.df[,-(1:3)] <- sapply(y.df$Year_Class, function(x) x + to.add - 1) %>% t()

complete.data <- which(!is.na(y.mat), arr.ind = TRUE)
n_pos <- nrow(complete.data)
y.vec <- y.mat[which(!is.na(y.mat))]
area.vec <- area.mat[which(!is.na(y.mat))]

cohorts <- as.numeric(as.factor(y.df$Year_Class))
areas <- as.numeric(as.factor(y.df$Area))
big.areas <- y.df$Big_Area # from failed hierarchical spatial model

covar.all <- tibble(year = y.df$Year_Class, cohorts) %>%
  group_by(year) %>%
  summarize(cohort = first(cohorts)) %>%
  left_join(cuti) %>% # need to rename columns to have index name
  left_join(beuti) %>%
  left_join(sst) %>%
  left_join(select(biomass, year=Year, Log_R)) %>% 
  arrange(cohort) %>% 
  select(-year, -cohort, -cuti_45N, -beuti_45N) %>%
  mutate(across(everything(), ~ standardize_vec(.x)),
         across(-Log_R, ~ .x^2, .names = '{.col}_sq'))
