library(beyonce)
theme_set(ggsidekick::theme_sleek(base_size = 18))

# Lengths by year ------------------------------------------------

pal <- beyonce_palette(22, 6)
#pal <- PNWColors::pnw_palette('Bay', 11)

xx <- lengths %>%
  group_by(Age_Month, Year_Class) %>%
  summarize(Avg_Len = mean(Avg_Len)) %>%
  filter(Year_Class == 2000) %>%
  ggplot() +
  geom_line(aes(x=Age_Month-.33, y=Avg_Len, col = Year_Class, group = factor(Year_Class)),
            alpha = 0.5) +
  labs(x = 'Age', y = 'Average length (mm)', col = 'Recruitment\nyear') +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(limits = c(12, 26)) +
  # nmfspalette::scale_color_nmfs(palette = 'waves', discrete = FALSE, 
  #                               limits = range(lengths$Year))
 scale_color_gradient2(limits = range(lengths$Year), midpoint = mean(range(lengths$Year)),
                       low = pal[1], mid = pal[3], high = pal[6]) +
  NULL

png(here('Figures/AFS/year1.png'), width = 7, height = 5, units = 'in', res=500)
xx
dev.off()

png(here('Figures/AFS/year2.png'), width = 7, height = 5, units = 'in', res=500)
xx %+% 
  (lengths %>%
     group_by(Age_Month, Year_Class) %>%
     summarize(Avg_Len = mean(Avg_Len)))
dev.off()


# Lengths by area ---------------------------------------------------------

#pal <- beyonce::beyonce_palette(82, n = length(unique(lengths$Area)), type = 'continuous')
pal <- PNWColors::pnw_palette('Bay', n = length(unique(lengths$Area)))
names(pal) <- sort(unique(lengths$Area))
yy <- lengths %>%
  group_by(Age_Month, Area) %>%
  summarize(Avg_Len = mean(Avg_Len)) %>%
  filter(Area == 24) %>%
  mutate(Area = factor(Area, levels = unique(lengths$Area))) %>%
  ggplot() +
  geom_line(aes(x = Age_Month - .33, y = Avg_Len, col = Area)) +
  labs(x = 'Age', y = 'Average length (mm)', col ='State Area') +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(limits = c(12, 26)) +
  scale_color_manual(drop = FALSE, values = pal) +
  guides(linetype = guide_legend(override.aes = list(lwd = 20))) +
  NULL

png(here('Figures/AFS/area1.png'), width = 7, height = 5, units = 'in', res=500)
yy
dev.off()

png(here('Figures/AFS/area2.png'), width = 7, height = 5, units = 'in', res=500)
yy %+%
  (lengths %>%
     group_by(Age_Month, Area) %>%
     summarize(Avg_Len = mean(Avg_Len)))
dev.off()


# Variance estimates ------------------------------------------------------

anova.tibble <- summary(mod, pars = c('sigma_x0', 'sigma_area', 'sigma_process', 'sigma_obs'))$summary[,c(4,6,8)] %>% 
  as_tibble %>%
  mutate(param = c('Initial size\n(year)', 'Initial size\n(area)', 'Process\n(year)', 'Observation\n(year x area)'))

anova.tibble %>%
  ggplot() +
  geom_segment(aes(x = param, xend = param, y = `2.5%`, yend = `97.5%`), lwd=1.2) +
  geom_point(aes(x = param, y = `50%`, col = param), cex = 5) +
  labs(x = 'Variance component', y = 'Estimated standard deviation') +
  theme(legend.position = 'none') +
  # guides(fill = FALSE) +
  ylim(0, 1.25) +
  scale_color_manual(values = inauguration::inauguration('inauguration_2021', n=4))
  ggsave(filename = here('Figures/AFS/anova.png'), device = png, width = 7, height = 5, units = 'in', dpi=500)
  

# RPR baseline ------------------------------------------------------------

n_FF <- 50
FF_seq <- seq(0, 0.3, length.out = n_FF) 
ypr.list$max_cpp <- NA
ypr.list$pred_wt <- pred_wt_base
rpr_base <- expand.grid(FF_seq = FF_seq, opening = 1:5) %>% 
  purrr::pmap_dfr(function(FF_seq, opening) 
    get_ypr(FF = FF_seq, opening = opening, metric = 'revenue', setup.list = ypr.list)) %>%
  rename(rpr = value) %>%
  mutate(opening_chr = paste(month.abb[ceiling(opening/2) + 3], 
                             c(15,1)[opening %% 2 + 1]),
         opening_fct = factor(opening_chr, levels = c('Apr 1', 'Apr 15', 'May 1',
                                                      'May 15', 'Jun 1'))) %>%
  select(-opening_chr) %>%
  group_by(opening_fct, FF)

pal <- beyonce_palette(18, n = 5)
names(pal) <- levels(rpr_base$opening_fct)
p <- rpr_base %>%
  summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
            high = quantile(rpr, 0.975), opening = first(opening)) %>% 
  filter(opening == 1) %>%
  ggplot() +
  geom_line(aes(x = FF, y = mid, col = opening_fct), lwd = 1) +
  labs(x = 'F (1/mo)', y = 'Revenue per recruit') +
  ylim(0, 1.4) +
  scale_color_manual(values = pal, drop = FALSE, name = 'Opening date') +
  NULL

p +
  geom_ribbon(aes(x = FF, ymin = low, ymax = high), alpha = 0.2) 
ggsave(here('Figures/AFS/rpr1.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)
p +
  geom_ribbon(aes(x = FF, ymin = low, ymax = high), alpha = 0.2) +
  geom_vline(xintercept = 0.03) +
  geom_vline(xintercept = 0.2) +
  NULL
ggsave(here('Figures/AFS/rpr2.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)

(p +
  geom_vline(xintercept = 0.03) + # add fishing mortality lines
  geom_vline(xintercept = 0.2)) %+%
  (rpr_base %>% #update to include all five opening dates
     summarize(low = quantile(rpr, 0.025), mid = median(rpr), 
               high = quantile(rpr, 0.975), opening = first(opening)))
ggsave(here('Figures/AFS/rpr3.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)



# Opening date by area ----------------------------------------------------
sort(unique(lengths$Area))

p <- rpr_by_area_summary %>%
  ggplot() +
  geom_line(aes(x = FF, y = mid, col = opening_chr, group = opening_chr)) +
  labs(x = 'F (1/mo)', y = 'Revenue per recruit') +
  geom_vline(xintercept = 0.03) +
  geom_vline(xintercept = 0.2) +
  facet_wrap(~area) +
  scale_color_manual(values = pal, drop = FALSE, name = 'Opening date') +
  theme(
    strip.text.x = element_text(color = 'white')
    ) +
  scale_x_continuous(breaks = c(0,.1,.2,.3), labels = c('0.0', '', '0.2', '')) +
  NULL
p
ggsave(here('Figures/AFS/rpr_area.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 1000)

(p +
    theme(legend.position = 'none')) %+%
    # theme(axis.ticks = element_blank(),
    #       axis.text = element_blank(),
    #       axis.title = element_blank(),
    #       strip.text = element_text(size = 0.01),
    #       legend.position = 'none') +
  #   ylim(0, 1.5)) %+%
  filter(rpr_by_area_summary, area == 3 | area == 12)
ggsave(here('Figures/AFS/rpr_area2.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 1000)


# model comparison calculations -------------------------------------------

load(here('Code/model_fit_base.RData'))
base_mod <- mod
load(here('Code/covars/model_fit_meanBLT_precond.RData'))
env_mod <- mod

draws_base <- rstan::extract(base_mod)
draws_env <- rstan::extract(env_mod)

covar_all_yr <- tibble(year = y.df$Year_Class, cohorts) %>%
  group_by(year) %>%
  summarize(cohort = first(cohorts)) %>%
  bind_cols(covar.all)

base_pred <- as.vector(draws_base$x0_mean)

env_pred <- purrr::map_dfc(covar_all_yr$meanBLT_precond,
                           ~ tibble(as.vector(draws_env$x0_mean) +
                                      as.vector(draws_env$covar_par) * .x)) %>%
  setNames(covar_all_yr$year) %>%
  mutate(constant = as.vector(draws_base$x0_mean),
         mcmc_iter = 1:n()) %>%
  pivot_longer(cols = -c(constant, mcmc_iter), names_to = 'year', values_to = 'length_env')

ggplot(env_pred) +
  geom_violin(aes(x = year, y = length_env - constant))


x0_comparison <- apply(draws_base$x0, c(1,3), identity) %>% 
  `colnames<-`(covar_all_yr$year) %>%
  as_tibble() %>% 
  mutate(across(everything(), ~ .x + as.vector(draws_base$x0_mean)),
         mcmc_iter = 1:n()) %>%
  pivot_longer(-mcmc_iter, names_to = 'year', values_to = 'length_obs') %>%
  full_join(env_pred) %>%
  mutate(const_error = constant - length_obs,
         env_error = length_env - length_obs,
         env_improvement = abs(const_error) - abs(env_error),
         improvement_pct = 100*(env_improvement/
                                  (mean(y.vec) + mean(x0_comparison$constant))))


# model comparison density plots ------------------------------------------


p <- x0_comparison %>%
  filter(year == 2003) %>%
  select(mcmc_iter:length_env) %>%
  pivot_longer(cols = -c(mcmc_iter, year), names_to = 'model',
               values_to = 'estimate') %>%
  mutate(model = as.factor(model)) %>%
  ggplot() +
  geom_density(aes(x = estimate + mean(y.vec), col = model), lwd = 1) +
  xlab('Length (mm)') +
#  xlim(min(data$estimate) + mean(y.vec), max(data$estimate) + mean(y.vec)) +
  scale_color_manual(values = inauguration::inauguration('inauguration_2021', n=3) ,
                     labels = c('Constant size', 'w/Bottom temp', '2003 Observed'),
                     name = '', drop = FALSE)

p %+%
  filter(p$data, model == 'constant') +
  xlim(min(p$data$estimate) + mean(y.vec), max(p$data$estimate) + mean(y.vec))
ggsave(here('Figures/AFS/density_ex1.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)

p %+%
  filter(p$data, model != 'length_obs') +
  xlim(min(p$data$estimate) + mean(y.vec), max(p$data$estimate) + mean(y.vec))
ggsave(here('Figures/AFS/density_ex2.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)

p 
ggsave(here('Figures/AFS/density_ex3.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)


# model comparison violin plots -------------------------------------------

p <- x0_comparison %>%  
  mutate(year = factor(year)) %>%
  ggplot() +
  geom_violin(aes(x = year, y = improvement_pct, fill = year!=2003),
              col = 'gray20') + 
  geom_hline(yintercept = 0, col = 'red', lwd = 1) +
  labs(y = 'Percent improvement') +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = 'none') +
  scale_x_discrete(drop = FALSE, name = 'Recruitment year') +
  ylim(min(x0_comparison$improvement_pct), max(x0_comparison$improvement_pct)) +
  scale_fill_manual(values = c(inauguration::inauguration('inauguration_2021', n=3)[3],
                               'gray80'))

p %+%
  filter(p$data, year==2003)
ggsave(here('Figures/AFS/violin1.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)

p 
ggsave(here('Figures/AFS/violin2.png'), width = 9, height = 5, device = png, 
       units = 'in', dpi = 500)
