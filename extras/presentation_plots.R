
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

# Plot fitness effect sizes for each study
ggplot() +
  coord_flip() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#E8E8E8') +
  scale_colour_manual(values = c('#d62d20', '#E8E8E8', '#2acaea')) +
  geom_linerange(data = forest_dat, linewidth = 1,
                 aes(x = uid, ymin = lower_f, ymax = upper_f, col = rel_f)) +
  geom_point(data = forest_dat, aes(x = uid, y = yi_f), size = 2, colour = '#E8E8E8') +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18, colour = '#E8E8E8'),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = '#E8E8E8'),
        panel.background = element_rect(colour = '#E8E8E8', fill = '#32404A'),
        plot.background = element_rect(fill = '#32404A', colour = '#32404A'),
        panel.grid = element_blank(),
        strip.text = element_blank(),
        strip.background = element_rect(colour = '#32404A', fill = '#32404A'),
        plot.margin = unit(c(1,0.2,0,0), 'cm')) +
  facet_wrap(~ species_name, strip.position = 'left', scales = 'free_y', dir = 'v', ncol = 1) 


# Save plot
ggsave(last_plot(), filename  = 'extras/dark_forest_plot.tiff', 
       device = 'tiff', dpi = 300,
       width = 9, height = 12, units = 'cm')
