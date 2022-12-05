
# Fit mixed-effect meta-analytic models ====
# 
# Testing grand mean effect sizes of glucocorticoids and fitness
# Also testing how moderators (species longevity, individual life-history stage, 
# individual sex) influence strength of stress response and effect of stressors
# on fitness

library(tidyverse)
library(metafor)

# Load effect size data
dat_es <- readRDS('derived_data/effect_sizes.rds') 

# Egger's test for publication bias
regtest(x = yi_f, vi = vi_f, data = dat_es)

# Test effects of stressor on fitness and GCs independently
# Grand mean effect sizes indicate that overall, verts produce more GCs in 
# response to stressors and fitness decreases in response to stressors.
# 
# Fitness model
f_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
                random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
# Get confidence intervals
f_cint <- data.frame(beta = f_mod$beta, 
                     lower = f_mod$ci.lb, upper = f_mod$ci.ub)
# Glucocorticoids model
gc_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
                 random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# Get confidence intervals
gc_cint <- data.frame(beta = gc_mod$beta, 
                      lower = gc_mod$ci.lb, upper = gc_mod$ci.ub)

# Key papers to cite:
#   Bonier et al. 2009
#   Patterson et al. 2014
#   Breuner & Berk 2019

# Sex differences in GC production and fitness
# 
# No sex differences in GC production
# (reference cat = female; significant intercept, but QM p-value = 0.36, 
# sex = male not different from intercept)
rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ factor(sex), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# No sex differences in effects of stressor on fitness
rma.mv(data = dat_es, yi = yi_f, V = vi_f,
       mods = ~ factor(sex), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))

# Differences in GC production with longevity and allocation to fitness types
# 
# No differences in GC production with longevity when exposed to stressors
rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ lifespan, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# Fitness of longer-lived species somewhat more negatively affected by stressors
rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
       mods = ~ lifespan, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))

# Differences in GC production and fitness effects in juveniles and adults
# 
# Juveniles produce more glucocorticoids in response to stressors
rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ factor(life_history_stage), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# Juveniles experience more negative fitness effects from stressors
rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
       mods = ~ factor(life_history_stage), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))

# Save outputs
saveRDS(f_cint, 'output/fit_meta_confint.rds')
saveRDS(gc_cint, 'output/gc_meta_confint.rds')

