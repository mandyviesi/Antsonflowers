### Effect of ants on the pollination of attended plants ###
### Biases on meta-analysis
### Update: September 25th 2024 ###
### Amanda Vieira da Silva ### 
### @mandyviesi ###

# Packages  -----------

library(metafor)
library(scales)
library(V.PhyloMaker)
library(dplyr)
library(orchaRd)
library(ggplot2)


## ### ## ## ## ##
# DATA ----------
## ## ## ## ## ##  

# Data

meta <- readxl::read_xlsx("meta.xlsx", sheet = 1)

# Checking data
glimpse(meta)
head(meta)
View(meta)

## ## ## ## ## ## ##
# Phylogeny  -------
## ## ## ## ## ## ##

# Plant species
plant_species <- meta |> 
  select(plant_sp, plant_genus, plant_fam)

# Phylogeny
plant_phylo <- V.PhyloMaker::phylo.maker(sp.list = plant_species)

# Ultrametric tree
ultmetree <-  compute.brlen(plant_phylo$scenario.3, method = "Grafen")

# Creating the variance-covariance matrix
cov.matrix <-  vcv(ultmetree, corr = TRUE)

# Writing sp2 as species in cov.matrix
meta$plant_sp2 <- gsub(" ", "_", x = meta$plant_sp)
meta$plant_sp2 <- stringr::str_to_title(meta$plant_sp2)

# Bias  -----------------------------------

## With outlier ---------------------------

# Funnel
funnel(meta$yi, meta$vi, yaxis="sei",
       xlab = "Effect size (Hedges' g)", digits = 2, las = 1) 

funnel(meta$yi, meta$vi, yaxis="seinv",
       #xlim = c(-3, 3),
       xlab = "Effect size (Hedges' g)",  digits = 2, las = 1)

# Calculating square root of variance
meta$sei <- sqrt(meta$vi)

### Study ---------------------------

# Bias of small study (equation 21 - Nakagawa et al, 2022)
pub_bias_study_out <- rma.mv(yi = yi, V = vi,
                         mod = ~1 + sei,
                         random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                         R = list(plant_sp2 = cov.matrix),
                         data = meta)

pub_bias_study_out

study_pred <- predict(pub_bias_study_out)

dfstudy <- data.frame(sei = meta$sei,
                      predi = study_pred$pred,
                      lower = study_pred$ci.lb,
                      upper = study_pred$ci.ub)

jpeg("fig4a_study.jpg", width = 1600, height = 1100, res = 300)
study_fig <- ggplot(data = meta, aes(x = sei, y = yi)) + 
  geom_point(aes(size = (1/sqrt(yi))), shape = 21, fill = "grey85", colour = "grey60", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black", alpha = 0.5) + 
  geom_line(data = dfstudy, aes(x = sei, y = predi), size = 1, colour = "#0a0a0a") + 
  geom_ribbon(data = dfstudy, aes(ymin = lower,  ymax = upper, y = 0), alpha = 0.3, fill = "#808080") + 
  labs(x = "Square root of variance (sei)", y = "Effect size (Hedges' g)", size = "Precison (1/SE)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        legend.text = element_text(size = 11), 
        legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_blank(), 
        legend.direction = "horizontal",
        legend.title = element_text(size = 12), 
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"))
dev.off()


### Time lag bias -------------------

# Time-lag bias (equation 23 - Nakagawa et al,  2022)

time_lag_overall <- rma.mv(yi = yi, V = vi,
                           mods= ~1 + year,
                           random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                           R = list(plant_sp2 = cov.matrix),
                           data = meta)
time_lag_overall

time_pred <- predict(time_lag_overall)

dftime <- data.frame(year = meta$year,
                     predi = time_pred$pred,
                     lower = time_pred$ci.lb,
                     upper = time_pred$ci.ub)

time_fig <- ggplot(data = meta, aes(x = year, y = yi)) + 
  geom_point(aes(size = (1/sqrt(yi))), shape = 21, fill = "grey85", colour = "grey60", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black", alpha = 0.5) + 
  geom_line(data = dftime, aes(x = year, y = predi), size = 1, colour = "#0a0a0a") + 
  geom_ribbon(data = dftime, aes(ymin = lower,  ymax = upper, y = 0), alpha = 0.3, fill = "#808080") + 
  labs(x = "Year of publication", y = "Effect size (Hedges' g)", size = "Precison (1/SE)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        legend.text = element_text(size = 11), 
        legend.position = "top", 
        legend.justification = c(0, 0), 
        legend.background = element_blank(), 
        legend.direction = "horizontal",
        legend.title = element_text(size = 12), 
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"))

jpeg("fig4_pubias.jpg", width = 1800, height = 1000, res = 300)
ggpubr::ggarrange(study_fig, time_fig, ncol = 2, common.legend = TRUE, labels = c("(a)", "(b)"))
dev.off()


### Overall bias ------------------

# High heterogeneity: combining equation 21 and 23 (equation 24 Nakagawa et al 2022)
publication.bias.multi <- rma.mv(yi = yi, V = vi,
                                 mods= ~1 + year + sei + measure_type + functional_group + resource_location : measure_type, 
                                 random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                                 R = list(plant_sp2 = cov.matrix),
                                 control=list(optimizer="optim"),
                                 data = meta)

publication.bias.multi


## Without outlier -----------------

meta2 <- meta[-meta$yi < abs(28),]

# Funnel
funnel(meta2$yi, meta2$vi, yaxis="sei",
       xlab = "Effect size (Hedges' g)", digits = 2, las = 1) 

funnel(meta2$yi, meta2$vi, yaxis="seinv",
       #xlim = c(-3, 3),
       xlab = "Effect size (Hedges' g)",  digits = 2, las = 1)

### Study ---------------------------

# Bias of small study (equation 21 - Nakagawa et al, 2022)
pub_bias_study_out <- rma.mv(yi = yi, V = vi,
                             mod = ~1 + sei,
                             random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                             R = list(plant_sp2 = cov.matrix),
                             data = meta2)

pub_bias_study_out

study_pred <- predict(pub_bias_study_out)

dfstudy <- data.frame(sei = meta2$sei,
                      predi = study_pred$pred,
                      lower = study_pred$ci.lb,
                      upper = study_pred$ci.ub)

jpeg("fig4a_study_noout.jpg", width = 1600, height = 1100, res = 300)
study_fignoout <- ggplot(data = meta2, aes(x = sei, y = yi)) + 
  geom_point(aes(size = (1/sqrt(yi))), shape = 21, fill = "grey85", colour = "grey60", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black", alpha = 0.5) + 
  geom_line(data = dfstudy, aes(x = sei, y = predi), size = 1, colour = "#0a0a0a") + 
  geom_ribbon(data = dfstudy, aes(ymin = lower,  ymax = upper, y = 0), alpha = 0.3, fill = "#808080") + 
  labs(x = "Square root of variance (sei)", y = "Effect size (Hedges' g)", size = "Precison (1/SE)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        legend.text = element_text(size = 11), 
        legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_blank(), 
        legend.direction = "horizontal",
        legend.title = element_text(size = 12), 
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"))
dev.off()


### Time lag bias -------------------

# Time-lag bias (equation 23 - Nakagawa et al,  2022)

time_lag_overall <- rma.mv(yi = yi, V = vi,
                           mods= ~1 + year,
                           random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                           R = list(plant_sp2 = cov.matrix),
                           data = meta2)
time_lag_overall

time_pred <- predict(time_lag_overall)

dftime <- data.frame(year = meta$year,
                     predi = time_pred$pred,
                     lower = time_pred$ci.lb,
                     upper = time_pred$ci.ub)

jpeg("fig4b_timelagnoout.jpg", width = 1400, height = 1000, res = 300)
time_fignoout <- ggplot(data = meta2, aes(x = year, y = yi)) + 
  geom_point(aes(size = (1/sqrt(yi))), shape = 21, fill = "grey85", colour = "grey60", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 2, colour = "black", alpha = 0.5) + 
  geom_line(data = dftime, aes(x = year, y = predi), size = 1, colour = "#0a0a0a") + 
  geom_ribbon(data = dftime, aes(ymin = lower,  ymax = upper, y = 0), alpha = 0.3, fill = "#808080") + 
  labs(x = "Year of publication", y = "Effect size (Hedges' g)", size = "Precison (1/SE)") + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        legend.text = element_text(size = 11), 
        legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_blank(), 
        legend.direction = "horizontal",
        legend.title = element_text(size = 12), 
        plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"))
dev.off()

jpeg("fig4_pubiasnoout.jpg", width = 1800, height = 1000, res = 300)
ggpubr::ggarrange(study_fignoout, time_fignoout, ncol = 2, common.legend = TRUE, labels = c("(a)", "(b)"))
dev.off()


### Overall bias ------------------

# High heterogeneity: combining equation 21 and 23 (equation 24 Nakagawa et al 2022)
publication.bias.multi <- rma.mv(yi = yi, V = vi,
                                 mods= ~1 + year + sei + measure_type + functional_group + resource_location : measure_type, 
                                 random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                                 R = list(plant_sp2 = cov.matrix),
                                 control=list(optimizer="optim"),
                                 data = meta2)

publication.bias.multi
