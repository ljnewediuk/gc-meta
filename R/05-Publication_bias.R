library(metafor)

#### Rosenberg FSN ####
#Check for publication bias with Rosenber Fail-safe  Numner
#Remember, if FSN >5k+10 then publication bias unlikely
#Fitness, FSN= 2855, target: FSN>615

# Load data
dat_es <- readRDS("derived_data/effect_sizes.rds")

Fitnessfsnstudy<-fsn(yi_f, vi_f, data=dat_es, type="Rosenberg", digits=4)
Fitnessfsnstudy

#GC's, FSN = 5467, FSN>615
GCfsnstudy<-fsn(yi_gc, vi_gc, data=dat_es, type="Rosenberg", digits=4)
GCfsnstudy

#### Funnel plots for moderators ####

#Fitness funnel plot
fit_pb_test <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, mods = I(1/n_fitness_low),
                      random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
# tiff("Desktop/gcfigures/funnelplots/fitness funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(fit_pb_test, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

#GCs funnel plot
GC_pb_test <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, mods = I(1/n_gc_low),
                     random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
# tiff("Desktop/gcfigures/funnelplots/gc funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(GC_pb_test, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

# Sex moderator on GCs
sex_gc_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ factor(sex) + I(1/n_fitness_low), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# tiff("Desktop/gcfigures/funnelplots/gc and sex funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(sex_gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

# Sex moderator on fitness
sex_fit_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f,
       mods = ~ factor(sex) + I(1/n_fitness_low), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
# tiff("Desktop/gcfigures/funnelplots/fitness and sex funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(sex_fit_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

# longevity moderator on GCs
lon_gc_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ lifespan + I(1/n_fitness_low), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# tiff("Desktop/gcfigures/funnelplots/gc and long funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(lon_gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

# longevity moderator on fitness
lon_fit_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
       mods = ~ lifespan + I(1/n_fitness_low), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
# tiff("Desktop/gcfigures/funnelplots/fitness and long funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(lon_fit_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

# LHS moderator on GCs
LHS_fit_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ factor(life_history_stage) + I(1/n_fitness_low), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
# tiff("Desktop/gcfigures/funnelplots/gc and LHS funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(LHS_fit_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()

# LHS moderator on fitness
LHS_gc_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
       mods = ~ factor(life_history_stage) + I(1/n_fitness_low), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
# tiff("Desktop/gcfigures/funnelplots/fitness and LHS funnel plot.tiff", width = 7, height = 7, units = "in", res = 300,compression="lzw")
funnel(LHS_gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)
dev.off()


#### Publication bias models ####

#Meta-analytic model for fitness publication bias
fit_pb_test <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, mods = I(1/n_fitness_low),
                        random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))

#Meta-analytic model for GCs publication bias
GC_pb_test <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, mods = I(1/n_gc_low),
                        random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))






