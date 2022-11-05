#### Rosenberg FSN ####
#Check for publication bias with Rosenber Fail-safe  Numner
#Remember, if FSN >5k+10 (in this case, >225) then publication bias unlikely
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
                 
#### Subset analysis ####

