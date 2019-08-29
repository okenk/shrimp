library(readxl)
library(tidyverse)

read_excel('Data/2014-2018 OR pink shrimp.xlsx') %>% 
  filter(Age >= 1) %>%
  group_by(Year, Month, Area) %>%
  summarize(Age1 = sum(Age==1)/n(),
            Age2 = sum(Age==2)/n(),
            Age3 = sum(Age==3)/n()) %>%
  write_csv('Data/age_comp_2014-18.csv')
