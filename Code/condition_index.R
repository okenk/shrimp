library(dplyr)
library(ggplot2)

bci <- readxl::read_xlsx('Data/Alldata.xlsx', sheet = 'BCI data') %>%
  mutate(`Catch date` = as.POSIXct(`Catch date`, format = '%m/%d/%Y', tz = 'UTC'),
         Boat = stringr::str_to_upper(Boat))

individual.lengths <- readxl::read_xlsx('Data/allshrimp.xlsx', sheet = 'rework')

names(individual.lengths)

hist(bci$`bob-residual`)

temp <- individual.lengths %>%
  group_by(`Catch date`, Boat, Area, Depth) %>%
  summarize(projwt_sum = sum(projwt),
            avg_age = mean(Age, na.rm=TRUE),
            avg_length = mean(CL, na.rm=TRUE)) %>%
  mutate(Boat = stringr::str_to_upper(Boat)) %>%
  right_join(bci) %>%
  mutate(check = projwt_sum - `projected weight Bob`) %>%
 # filter(check > 1) %>% 
  select(`Catch date`, Boat, Area, Depth, projwt_sum, `projected weight Bob`, 
         `total weight`, check) %>%
  View

filter(temp, check < 1) %>% 
ggplot() +
  geom_point(aes(x = avg_length, y = BCI), alpha = .2)
  
ggplot(bci) +
  geom_point(aes(x = `Catch date`, y = BCI))
