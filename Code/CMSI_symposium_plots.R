library(beyonce)
pal <- beyonce_palette(22, 6)

xx <- lengths %>%
  group_by(Age_Month, Year_Class) %>%
  summarize(Avg_Len = mean(Avg_Len)) %>%
  filter(Year_Class == 2000) %>%
  ggplot() +
  geom_line(aes(x=Age_Month-.33, y=Avg_Len, col = Year_Class, group = factor(Year_Class)), alpha = 0.5) +
  labs(x = 'Age', y = 'Average length (mm)', col = 'Recruitment\nyear') +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(limits = c(12, 26)) +
  scale_color_gradient2(limits = range(lengths$Year), midpoint = mean(range(lengths$Year)), low = pal[1], mid = pal[3], high = pal[6]) +
  NULL

png('Figures/CMSI_symposium/year1.png', width = 7, height = 5, units = 'in', res=500)
xx
dev.off()

png('Figures/CMSI_symposium/year2.png', width = 7, height = 5, units = 'in', res=500)
xx %+% 
  (lengths %>%
     group_by(Age_Month, Year_Class) %>%
     summarize(Avg_Len = mean(Avg_Len)))
dev.off()

pal <- beyonce::beyonce_palette(82, n = length(unique(lengths$Area)), type = 'continuous')
yy <- lengths %>%
  group_by(Age_Month, Area) %>%
  summarize(Avg_Len = mean(Avg_Len)) %>%
  filter(Area == 24) %>%
  mutate(Area = factor(Area, levels = unique(lengths$Area))) %>%
  ggplot() +
  geom_line(aes(x = Age_Month - .33, y = Avg_Len, col = Area)) +
  labs(x = 'Age', y = 'Average length (mm)', col ='State Area') +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(limits = c(12, 26)) +
  scale_color_manual(drop = FALSE, values = pal) +
  guides(linetype = guide_legend(override.aes = list(lwd = 20))) +
  NULL

png('Figures/CMSI_symposium/area1.png', width = 7, height = 5, units = 'in', res=500)
yy
dev.off()

png('Figures/CMSI_symposium/area2.png', width = 7, height = 5, units = 'in', res=500)
yy %+%
  (lengths %>%
     group_by(Age_Month, Area) %>%
     summarize(Avg_Len = mean(Avg_Len)))
dev.off()

png('Figures/anova.png', width = 7, height = 5, units = 'in', res=500)
summary(mod, pars = c('sigma_x0', 'sigma_area', 'sigma_process', 'sigma_obs'))$summary[,c(4,6,8)] %>% 
  as_tibble %>%
  mutate(param = c('Initial size\n(year)', 'Initial size\n(area)', 'Process\n(year only)', 'Observation\n(year x area)')) %>%
  ggplot() +
  geom_segment(aes(x = param, xend = param, y = `2.5%`, yend = `97.5%`), lwd=1.2) +
  geom_point(aes(x = param, y = `50%`, col = param), cex = 5) +
  labs(x = 'Variance component', y = 'Estimated standard deviation') +
  theme_classic(base_size = 18) +
  theme(legend.position = 'none') +
  guides(fill = FALSE) +
  ylim(0, 1.25) +
  scale_fill_manual(values = LaCroixColoR::lacroix_palette('PassionFruit', 4))
dev.off()
