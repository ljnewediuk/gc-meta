## Post-hoc analysis examining sample type as a potential source of variation ##

#load neccesary packages
require(metafor)
require(tidyverse)

dat_es <- readRDS("derived_data/effect_sizes.rds")

#Load post-hoc data
PostData <- readRDS("input/post_hoc_data.rds") %>%
select("uid","sample_type", "invasiveness", "study_groups") %>%
  left_join(dat_es) 

##Characteristics of 'sample type', sample type 'invasivness', and 
##the 'study groups' (i.e., experimental v. control groups or temporal groups) and
## lab or field

# Field = 43 (19 studies), lab = 68 (29 studies)
PostData %>%
  select("location") %>%
  table() 
PostData %>%
  select("study_id", "location") %>%
  filter(location == "lab") %>%
  select("study_id") %>%
  unique() %>%
  count()


# Blood = 90 (36 studies), FCM = 12 (6 studies), water = 1 (1 study), 
#whole body = 6 (4 studies), yolk = 2 (1 study) 
PostData %>%
  select("sample_type") %>%
  table() 
PostData %>%
  select("study_id", "sample_type") %>%
  filter(sample_type == "blood") %>%
 select("study_id") %>%
   unique() %>%
  count()

#invasive = 98 (41 studies), non-invasive = 13 (7 studies)
PostData %>%
  select("invasiveness") %>%
  table()
PostData %>%
  select("study_id", "invasiveness") %>%
  filter(invasiveness == "non_invasive") %>%
  select("study_id") %>%
  unique() %>%
  count()

#groups = 98 (42 studies), temporal = 13 (6 studies)
PostData %>%
  select("study_groups") %>%
  table()
PostData %>%
  select("study_id", "study_groups") %>%
  filter(study_groups == "temporal") %>%
  select("study_id") %>%
  unique() %>%
  count()



# Testing the sample type as a moderator 
# Blood as sample type signicant moderator, p val = 0.0002; 95% CI = 0.3324 - 1.0838
rma.mv(data = PostData, yi = yi_gc, V = vi_gc, 
       mods = ~ sample_type, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))

# Testing sampling invasiveness as a moderator 
#invasive sampling type is significant moderator, p val= 0.0002, 95% CI = 0.2855 - 0.9331
rma.mv(data = PostData, yi = yi_gc, V = vi_gc, 
       mods = ~ invasiveness, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))

# Testing sampling lab vs. field as a moderator 
#invasive sampling type is significant moderator, p val= 0.0002, 95% CI = 0.2855 - 0.9331
rma.mv(data = PostData, yi = yi_gc, V = vi_gc, 
       mods = ~ location, 
       random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type))


# GC's model with sampling type as a random effect
rma.mv(data = PostData, yi = yi_gc, V = vi_gc, 
                random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type, ~1|sample_type))
                     lower = f_mod$ci.lb, upper = f_mod$ci.ub)


# Fitness model with lab/field as a random effect
f_mod <- rma.mv(data = PostData, yi = yi_f, V = vi_f, 
                random = list(~ 1|study_id/uid, ~1|species_name, ~1|fitness_type, ~1|location))
lower = f_mod$ci.lb, upper = f_mod$ci.ub)

# Glucocorticoids model with lab/field as a random effect
gc_mod <- rma.mv(data = PostData, yi = yi_gc, V = vi_gc, 
                 random = list(~ 1|study_id/uid, ~1|species_name, ~1|stressor_type, ~1|location))
