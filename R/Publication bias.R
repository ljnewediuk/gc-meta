library(metafor)

#### Rosenberg FSN ####
#Check for publication bias with Rosenber Fail-safe  Numner
#Remember, if FSN >5k+10 then publication bias unlikely
#Fitness, FSN= 2855, target: FSN>615
Fitnessfsnstudy<-fsn(yi_f, vi_f, data=dat_es, type="Rosenberg", digits=4)
Fitnessfsnstudy

#GC's, FSN = 5467, FSN>615
GCfsnstudy<-fsn(yi_gc, vi_gc, data=dat_es, type="Rosenberg", digits=4)
GCfsnstudy

#### Funnel plots for moderators ####

#Fitness funnel plot
f_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
                random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
funnel(f_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

#GCs funnel plot
gc_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
funnel(gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

# Sex moderator on GCs
sex_gc_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ factor(sex), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
funnel(sex_gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

# Sex moderator on fitness
sex_fit_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f,
       mods = ~ factor(sex), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
funnel(sex_fit_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

# longevity moderator on GCs
lon_gc_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ lifespan, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
funnel(lon_gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

# longevity moderator on fitness
lon_fit_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
       mods = ~ lifespan, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
funnel(lon_fit_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

# LHS moderator on fitness
LHS_fit_mod <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, 
       mods = ~ factor(life_history_stage), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))
funnel(LHS_fit_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)

# LHS moderator on GCs
LHS_gc_mod <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, 
       mods = ~ factor(life_history_stage), 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))
funnel(LHS_gc_mod, level=c(90, 95, 99), shade=c("white", "#FFC107", "#0097A7"), refline=0, legend=TRUE)


#### Publication bias models ####

#Bivariate model for fitness publication bias
fit_pb_test <- rma.mv(data = dat_es, yi = yi_f, V = vi_f, mods = I(1/n_fitness_low),
                        random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))

#Bivariate model for GCs publication bias
GC_pb_test <- rma.mv(data = dat_es, yi = yi_gc, V = vi_gc, mods = I(1/n_fitness_low),
                        random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type))








