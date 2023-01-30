
# Fit bivariate models for correlation between gc and fitness ====
# 
# Also plot the posterior distribution, trace plot, and scatter plot of the
# relationship

library(MCMCglmm)
library(tidyverse)
library(cowplot)

# Load data
dat_es <- readRDS('derived_data/effect_sizes.rds') %>% 
  filter(yr %in% 2008:2021) %>%
  select(study_id, yi_f, yi_gc, vi_f, vi_gc, 
         fitness_type, stressor_type, life_history_stage) %>%
  na.omit()

# Specify uninformative parameter-expanded prior
P <- list(R = list(V = diag(2), nu = 1.002, fix = 2), 
          G = list(G1 = list(V = diag(2), nu = 2, 
                             alpha.mu = c(0,0), alpha.V = diag(c(25^2, 1)))))

# Fit model to test effect of fitness type on gc-fitness relationship
mod <- MCMCglmm(cbind(scale(yi_f), scale(yi_gc)) ~ trait -1 +
                  trait:fitness_type +
                  trait:stressor_type +
                  trait:life_history_stage,
                random = ~ us(trait):study_id,
                rcov = ~us(trait):units,
                mev = c(dat_es$vi_f, dat_es$vi_gc),
                family = c('gaussian', 'gaussian'),
                nitt = 420000,
                burnin = 20000,
                thin = 100,
                prior = P,
                verbose = F,
                data = dat_es)
# Summarize model
summary(mod)

# Posterior for correlation between repro and fitness
mod_cor <- mod$VCV[, 2]/
  (sqrt(mod$VCV[, 1]) * 
     sqrt(mod$VCV[, 4]))

# Make data frame of posterior distribution draws by iteration number
mod_cor_dat <- as.data.frame(mod_cor) %>% 
  rename('draws' == var1) %>% 
  mutate(iteration = 1:4000)

# Get Kernel density estimates for posterior
dens <- density(mod_cor)
dens_dat <- data.frame(x = dens$x, y = dens$y)
# Make quantiles as lower/upper 95% HPDI and mean of  posterior
quantiles <- c(HPDinterval(mod_cor)[1], mean(mod_cor), HPDinterval(mod_cor)[2])
dens_dat$quant <- factor(findInterval(dens_dat$x, quantiles))

# Plot scatter glucocorticoids ~ fitness
scatter_plot <- ggplot(data = dat_es, aes(x = yi_gc, y = yi_f)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_errorbar(aes(ymin = yi_f - sqrt(vi_f), ymax = yi_f + sqrt(vi_f)), 
                width = 0.1, alpha = 0.4) +
  geom_errorbarh(aes(xmin = yi_gc - sqrt(vi_gc), xmax = yi_gc + sqrt(vi_gc)), 
                 height = 0.1, alpha = 0.4) +
  geom_point(size = 2) + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 18, vjust = -3),
        axis.title.y = element_text(size = 18, vjust = 4),
        axis.text = element_text(size = 18),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank()) +
  ylab('Fitness effect size') +  xlab('Glucocorticoids effect size')
  
# Plot posterior density
post_plot <- ggplot(data = dens_dat, aes(x, y)) + 
  geom_line(colour = '#2636e3', linewidth = 1) + 
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quant), alpha = 0.4) + 
  geom_linerange(aes(x = mean(mod_cor), ymin = 0, ymax = 0.605), 
                 colour = '#2636e3', size = 1) +
  geom_linerange(aes(y = 0, xmin = min(x), xmax = max(x)),
                 colour = '#2636e3', size = 1) +
  scale_fill_manual(values = c('white', '#2636e3', '#2636e3', 'white')) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 18, vjust = -3),
        axis.title.y = element_text(size = 18, vjust = 4),
        axis.text = element_text(size = 18),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank()) +
  ylab('Density') + xlab('Value')

# Trace plot
trace_plot <- ggplot(mod_cor_dat) +
  geom_line(aes(x = iteration/1000, y = draws), alpha = 0.5) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = 18, vjust = -3),
        axis.title.y = element_text(size = 18, vjust = 4),
        axis.text = element_text(size = 18),
        panel.background = element_rect(colour = 'black', fill = 'white'),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        plot.margin = unit(c(0.5, 0.5, 1, 1), 'cm'),
        panel.grid = element_blank()) +
  ylab('Value') + xlab('Iteration (1000s)')

# Plot in single row with labels
plot_grid(trace_plot, post_plot, scatter_plot, nrow = 1, 
          labels = 'auto', label_size = 25)

# Save plot
ggsave('figures/posterior_plot.tiff', 
       last_plot(), device = 'tiff', 
       width = 32, height = 12, units = 'cm', dpi = 300)

