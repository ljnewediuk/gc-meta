
library(tidyverse)
library(metafor)

# Load Anage data for longevity
dat_anage <- read.delim('input/anage_data.txt') %>%
  mutate(species_name = paste0(tolower(Genus), '_', Species)) %>%
  select(species_name, `Maximum.longevity..yrs.`) %>%
  rename(., `Maximum.longevity..yrs.` == lifespan)

# Load data
dat <- readRDS('input/meta_dat.rds') %>%
  # Fill number of fitness samples with gc samples if same
  mutate(n_fitness_low = ifelse(is.na(n_fitness_low), n_gc_low, n_fitness_low),
         n_fitness_high = ifelse(is.na(n_fitness_high), n_gc_high, n_fitness_high)) %>%
  # Reverse fitness value if necessary
  mutate(mean_fitness_low = ifelse(fitness_reversed == 
                                     'yes', - mean_fitness_low, mean_fitness_low),
         mean_fitness_high = ifelse(fitness_reversed == 
                                     'yes', - mean_fitness_high, mean_fitness_high)) %>%
  # Add columns for sd for effect size calculation
  mutate(sd_gc_low = se_gc_low * sqrt(n_gc_low),
         sd_gc_high = se_gc_high * sqrt(n_gc_high),
         sd_fitness_low = se_fitness_low * sqrt(n_fitness_low),
         sd_fitness_high = se_fitness_high * sqrt(n_fitness_high))

# Calculate effect sizes
# Fitness
f_es <- escalc(data = dat, measure = 'SMD', 
              m1i = mean_fitness_high, m2i = mean_fitness_low,
              sd1i = sd_fitness_high, sd2i = sd_fitness_low,
              n1i = n_fitness_high, n2i = n_fitness_low,
              var.names = c('yi_f', 'vi_f'))
# GC
dat_es <- escalc(data = f_es, measure = 'SMD', 
              m1i = mean_gc_high, m2i = mean_gc_low,
              sd1i = sd_gc_high, sd2i = sd_gc_low,
              n1i = n_gc_high, n2i = n_gc_low,
              var.names = c('yi_gc', 'vi_gc')) %>%
  # Make remaining misc. changes to data
  #  Make all species name lowercase
  mutate(species_name = tolower(species_name)) %>%
  # Fix misspelled or old species names
  mutate(species_name = ifelse(species_name == 'dicologoglossa_cuneata', 'dicologlossa_cuneata', species_name)) %>%
  mutate(species_name = ifelse(species_name == 'brycon _nsignis', 'brycon_insignis', species_name)) %>%
  mutate(species_name = ifelse(species_name == 'scortum barcoo', 'scortum_barcoo', species_name)) %>%
  mutate(species_name = ifelse(species_name == 'scaphiopodidae', 'spea_bombifrons', species_name)) %>%
  mutate(species_name = ifelse(species_name == 'octodon_degu', 'octodon_decus', species_name)) %>%
  mutate(species_name = ifelse(species_name == 'rana_sylvatica', 'lithobates_sylvaticus', species_name)) %>%
  # Make LH column either adult or juvenile
  mutate(life_history_stage = ifelse(life_history_stage == 'juvenile', 'juvenile', 'adult')) %>%
  # Compare only known male & female
  mutate(sex = ifelse(sex == 'both', NA, sex)) %>%
  # Join longevity data
  left_join(dat_anage)


# Save data for models
saveRDS(dat_es, 'derived_data/effect_sizes.rds')

