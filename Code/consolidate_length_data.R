library(readxl)
library(tidyverse)

# I changed the name of the 2006 file from 2006mltableexc.xls to 2006mltable.xls so this loop would work
length_by_yr <- list()
yrs <- 1989:2013

for(year in yrs) {
  filename <- paste0('Data/shrimp_length_files/', year, 'mltable.xls')
  length_by_yr[[which(yrs == year)]] <- read_excel(filename, range = 'A5:H71', na = c('-', '--'), 
                                                   col_types = c('text', rep('numeric', 7))) %>%
    filter(!is.na(X__1)) %>% # remove blank rows
    mutate(State_Area = rep(gsub('MEAN LENGTHS - area ', '', grep('MEAN', X__1, value = TRUE)), each = 5)) %>%
    slice(-grep('MEAN', X__1)) %>%  # remove more blank rows
    mutate(Age = as.numeric(gsub('\\+', '', gsub("'S", '', X__1)))) %>%
    select(-X__1) %>% # this info has been put into Age column
    gather(key = 'Month', value = 'Avg_Len', -State_Area, -Age) %>%
    mutate(Year = year,
           Month = str_sub(str_to_upper(Month), end = 3), # month formatting varied, this ensures consistency
           Month_Num = sapply(Month, function(x) switch(x, APR = 4,
                                                               MAY = 5,
                                                               JUN = 6,
                                                               JUL = 7,
                                                               AUG = 8,
                                                               SEP = 9,
                                                               OCT = 10))) %>%
    rename(Area = State_Area)
}

# The warnings are from all the cells with data (as opposed to --) for years 2006 and later and I can't figure out why! 
# But the data frames looked correct once I specified col_types.

indiv_data <- read_excel('Data/2014-2018 OR pink shrimp.xlsx') %>%
  mutate(Area = as.character(Area)) %>%
  group_by(Year, Month, Area, Age) %>%
  summarize(Avg_Len = mean(CL),
            N = n(),
            sd = sd(CL)) %>%
  rename(Month_Num = Month) %>%
  mutate(Month = str_sub(str_to_upper(month.name[Month_Num]), end = 3)) # text version of month


lengths <- do.call(bind_rows, length_by_yr) %>%
  bind_rows(indiv_data)

filter(lengths, !is.na(Avg_Len)) %>%
  write.csv(file = 'Data/Compiled_Lengths.csv', row.names = FALSE)

png('Figures/MeanLengths.png', width = 12, height = 10, units = 'in', res = 250)
filter(lengths, Age > 0, !is.na(Avg_Len)) %>%
  ggplot(aes(x = Month_Num, y = Avg_Len, group = paste(Age, Area), col = factor(Area), lty = factor(Age, levels = 3:1))) +
  geom_path(lwd = .25) +
  geom_point(cex=.5) +
  facet_wrap(~Year) +
  labs(x = 'Month', y = 'Mean Length (mm)', color = 'State Area', lty = 'Age') +
  NULL
dev.off()

