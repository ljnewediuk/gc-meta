
# Make effect size figure ====
# 
# Testing grand mean effect sizes of glucocorticoids and fitness
# Also testing how moderators (species longevity, individual life-history stage, 
# individual sex) influence strength of stress response and effect of stressors
# on fitness

library(tidyverse)
library(cowplot)

# Load data
dat_es <- readRDS('derived_data/effect_sizes.rds') 

# Load model outputs
f_cint <- readRDS('output/fit_meta_confint.rds')
# Define polygon edges for effect size
f_poly <- data.frame(x = c(f_cint$upper, f_cint$beta, f_cint$lower, f_cint$beta),
                     y = c(0, 0.1, 0, -0.1))
gc_cint <- readRDS('output/gc_meta_confint.rds')
# Define polygon edges for effect size
gc_poly <- data.frame(x = c(gc_cint$upper, gc_cint$beta, gc_cint$lower, gc_cint$beta),
                     y = c(0, 0.1, 0, -0.1))

# Get vector of species with multiple studies to use as examples
multi_studies <- dat_es %>%
  group_by(study_id, species_name) %>%
  summarize(N =n()) %>%
  group_by(species_name) %>%
  filter(n() >= 2) %>%
  pull(species_name) %>%
  unique()

# Rename to save space
forest_dat <- dat_es %>%
  filter(species_name %in% multi_studies) %>%
  mutate(species_name = factor(species_name,
                               labels = c('D. cuneata', 'L. americanus',
                                          'L. sylvaticus', 'M. oeconomus',
                                          'S. salar', 'T. guttata')),
         #  Make UIDs
         uid = factor(1:n()),
         # Get 95% confidence intervals of effect sizes
         lower_gc = yi_gc - 1.96*sqrt(vi_gc),
         upper_gc = yi_gc + 1.96*sqrt(vi_gc),
         lower_f = yi_f - 1.96*sqrt(vi_f),
         upper_f = yi_f + 1.96*sqrt(vi_f)) %>%
  # Make ID and qualify relationship between glucocorticoids and fitness for eaach
  group_by(species_name) %>%
  mutate(ID = factor(cumsum(!duplicated(study_id))),
         rel_gc = ifelse(lower_gc < 0 & upper_gc < 0, 'neg', 'neut'),
         rel_gc = ifelse(lower_gc > 0 & upper_gc > 0, 'pos', rel_gc),
         rel_f = ifelse(lower_f < 0 & upper_f < 0, 'neg', 'neut'),
         rel_f = ifelse(lower_f > 0 & upper_f > 0, 'pos', rel_f)) %>%
  select(uid, ID, species_name, fitness_type:gc_measurement, yi_f, yi_gc, 
         lower_gc:upper_f, rel_gc, rel_f) 

# Plot glucocorticoid effect sizes for each study
gc_plot <- ggplot() +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_colour_manual(values = c('#e32636', '#91a3b0', '#2636e3')) +
  scale_shape_manual(values = c(21, 22, 24)) +
  geom_linerange(data = forest_dat, size = 0.75,
                 aes(x = uid, ymin = lower_gc, ymax = upper_gc, col = rel_gc)) +
  geom_point(data = forest_dat, aes(x = uid, y = yi_gc, shape = ID), 
             size = 2, fill = 'white') +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        strip.text = element_text(colour = 'black', face = 'italic', size = 12),
        strip.background = element_rect(colour = 'white', fill = 'white'),
        plot.margin = unit(c(1,0,0,0), 'cm')) +
  facet_wrap(~ species_name, strip.position = 'left', scales = 'free_y', dir = 'v', ncol = 1)
# Plot fitness effect sizes for each study
f_plot <- ggplot() +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 'dashed') +
  scale_colour_manual(values = c('#e32636', '#91a3b0', '#2636e3')) +
  scale_shape_manual(values = c(21, 22, 24)) +
  geom_linerange(data = forest_dat, size = 0.75,
                 aes(x = uid, ymin = lower_f, ymax = upper_f, col = rel_f)) +
  geom_point(data = forest_dat, aes(x = uid, y = yi_f, shape = ID), 
             size = 2, fill = 'white') +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank(),
        strip.text = element_text(colour = 'white'),
        strip.background = element_rect(colour = 'white', fill = 'white'),
        plot.margin = unit(c(1,0.2,0,0), 'cm')) +
  facet_wrap(~ species_name, strip.position = 'left', scales = 'free_y', dir = 'v', ncol = 1)
# Plot grand mean effect sizes for glucocorticoids
gc_es <- ggplot() +
  geom_polygon(data = gc_poly, aes(x = x, y = y), fill = '#2636e3') +
  annotate('text', x = 4, y = 0, label = paste0(
    '[', round(gc_cint$lower, 2), ', ', 
           round(gc_cint$upper, 2), ']'), size = 3.5) +
  xlim(-2.5, 8) + ylim(-1, 1) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(colour = 'white', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank())
# Plot grand mean effect sizes for fitness
f_es <- ggplot() +
  geom_polygon(data = f_poly, aes(x = x, y = y), fill = '#e32636') +
  annotate('text', x = -5.5, y = 0, 
           label = paste0('[', round(f_cint$lower, 2), ', ', 
                          round(f_cint$upper, 2), ']'), size = 3.5) +
  xlim(-10, 3.5) + ylim(-1, 1) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(colour = 'white', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid = element_blank())

# Plot in columns
plot_grid(gc_plot, f_plot, gc_es, f_es, ncol = 2, rel_heights = c(7, 0.75),
          labels = c('GCs', 'Fitness', '', ''), 
          label_x = c(0.1, 0))

# Save plot
ggsave(last_plot(), filename  = 'figures/forest_plot.tiff', 
       device = 'tiff', dpi = 300,
       width = 9, height = 21, units = 'cm')

