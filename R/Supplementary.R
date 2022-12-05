

library(tidyverse)
library(metafor)

# Fitness data
dat_f <- readRDS('derived_data/effect_sizes.rds') %>%
  mutate(cite = paste(authors, yr)) %>%
  group_by(cite) %>%
  mutate(ID = sequence(n())) %>%
  arrange(cite, ID, yi_f) %>%
  rename('yi' = yi_f) %>%
  mutate(cite_ID = paste0(cite, ' (', ID, ')'),
         lower = yi - vi_f*1.96, upper = yi + vi_f*1.96) %>%
  select(cite, cite_ID, yi, lower, upper)

# Plot
ggplot(dat_f, aes(x = yi, y = cite_ID)) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point() +
  scale_y_discrete(labels = dat_f$cite) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12, colour = 'black', vjust = -4),
        axis.text.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 12, colour = 'black'),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        strip.text = element_text(colour = 'black', face = 'italic', size = 12),
        strip.background = element_rect(colour = 'white', fill = 'white'),
        plot.margin = unit(c(1,0.5,1,0.5), 'cm')) +
  xlab('Standardized mean difference (fitness)')

# Save plot
ggsave(last_plot(), filename  = 'figures/supp_forest_plot_fitness.tiff', 
       device = 'tiff', dpi = 300,
       width = 18, height = 45, units = 'cm')

# Gluc data
dat_gc <- readRDS('derived_data/effect_sizes.rds') %>%
  mutate(cite = paste(authors, yr)) %>%
  group_by(cite) %>%
  mutate(ID = sequence(n())) %>%
  arrange(cite, ID, yi_f) %>%
  rename('yi' = yi_gc) %>%
  mutate(cite_ID = paste0(cite, ' (', ID, ')'),
         lower = yi - vi_gc*1.96, upper = yi + vi_gc*1.96) %>%
  select(cite, cite_ID, yi, lower, upper)

# Plot
ggplot(dat_gc, aes(x = yi, y = cite_ID)) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point() +
  scale_y_discrete(labels = dat_f$cite) +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12, colour = 'black', vjust = -4),
        axis.text.y = element_text(size = 12, colour = 'black'),
        axis.text.x = element_text(size = 12, colour = 'black'),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        strip.text = element_text(colour = 'black', face = 'italic', size = 12),
        strip.background = element_rect(colour = 'white', fill = 'white'),
        plot.margin = unit(c(1,0.5,1,0.5), 'cm')) +
  xlab('Standardized mean difference (glucocorticoids)')

# Save plot
ggsave(last_plot(), filename  = 'figures/supp_forest_plot_gcs.tiff', 
       device = 'tiff', dpi = 300,
       width = 18, height = 45, units = 'cm')
