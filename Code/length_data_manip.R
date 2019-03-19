library(readxl)
library(tidyverse)

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

year2000 <- left_join(age.comp, length, by = c('State_Area', 'Month', 'Age')) %>%
  mutate(pc_char = Pct_comp) %>%
  mutate_at(c('Ct', 'N', 'Age', 'Pct_comp'), as.numeric) %>%
  mutate(Year = 2000,
         n_age = Pct_comp * N)

year2000
View(year2000)
