library(tidyverse)
library(readxl)

lengths <- read_csv('Data/Compiled_Lengths.csv', col_types = 'cdcddddd') %>%
  filter(Age > 0)

age_comp <- read_excel('Data/shrimp age comp and count.xlsx') %>%
  gather(key = 'Age', value = 'Pct_Comp', `Age 0`:`Age 3`) %>%
  select(-ID) %>%
  rename(Area = `State Area`,
         Year = Fyear,
         Month = Fmonth) %>%
  mutate(Age = as.numeric(gsub('Age ', '', Age)),
         Month = str_sub(str_to_upper(Month), end = 3),
         Area = as.character(Area),
         N2 = ceiling(N*Pct_Comp)) %>% # N2 is number of shrimp of each age
         # Take ceiling because Scott said non-integer cases are usually underestimates
         filter(Age > 0)

select(age_comp, Year, Area, Month, Age, N2) %>%
  right_join(lengths) %>%
  filter(Year >= 2011) %>%
  mutate(N_final = ifelse(is.na(N), N2, N),
         North_South = ifelse(Area > 23, 'North', 'South')) %>%
  group_by(Year, Month, Age, North_South) %>%
  summarize(Aggregated_Length = sum(Avg_Len * N_final) / sum(N_final)) %>%
  write.csv('Data/Dan_Aggregated_Lengths.csv', row.names = FALSE)