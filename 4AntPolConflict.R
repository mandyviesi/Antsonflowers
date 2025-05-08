### Effect of ants on the pollination of attended plants ###
### Meta-analysis and data visualization ###
### Update: September 25th 2024 ###
### Amanda Vieira da Silva ### 
### @mandyviesi ###


# Packages  -----------

library(metafor)
library(V.PhyloMaker2)
library(scales)
library(dplyr)
library(orchaRd)
library(ggplot2)


## ### ## ## ## ##
# DATA ----------
## ## ## ## ## ##  

# Data
meta <- readxl::read_xlsx("meta.xlsx", sheet = 1, na = "NA")

# As factor
meta <- meta |> 
  mutate(ID = as.factor(ID),
         functional_group = as.factor(functional_group),
         resource_location = as.factor(resource_location),
         measure_type = factor(measure_type, levels = c("visitation", "plant_fitness")))

# Checking data
glimpse(meta)
summary(meta)
View(meta)

## ## ## ## ## ## ##
# Phylogeny  -------
## ## ## ## ## ## ##

# Plant species
plant_species <- meta |>  
  select(plant_sp, plant_genus, plant_fam)

# Phylogeny
plant_phylo <- V.PhyloMaker2::phylo.maker(sp.list = plant_species)

# Ultrametric tree
ultmetree <-  compute.brlen(plant_phylo$scenario.3, method = "Grafen") 

# Checking the tree
#tiff("plant_phylogeny.tif", res = 300, compression = "lzw", height = 1800, width = 1200)
plot(ultmetree,no.margin = TRUE, cex=0.9, align.tip.label = TRUE, type = "phylogram") 

#dev.off()

# Creating the variance-covariance matrix
cov.matrix <-  vcv(ultmetree, corr = TRUE)

# Writing sp2 as species in cov.matrix
meta$plant_sp2 <- gsub(" ", "_", x = meta$plant_sp)
meta$plant_sp2 <- stringr::str_to_title(meta$plant_sp2)


## ## ## ## ## ## ## ##
# OVERALL MODEL ------
## ## ## ## ## ## ## ##

## Model with outlier -----------------------

overall_out <- rma.mv(yi,vi, 
                      random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                      R = list(plant_sp2 = cov.matrix),
                      data = meta)

summary(overall_out)
confint(overall_out)

# Checking normality
overall_out_nrm <- rstandard(overall_out)
res_overall_out <- overall_out_nrm$resid
qqnorm(res_overall_out)
qqline(res_overall_out)


## Model without outlier ------------------

# The outlier
meta |>  filter(yi < -28)

# Repeating analysis without outlier (it has a hedges' g value of -28)
meta2 <- meta[-meta$yi < abs(28),]

# Min and max values of yi
min(meta2$yi)
max(meta2$yi)

# Number of studies
meta2 |> 
  distinct(ID) |> 
  count()

# Number of plant species
meta2 |> 
  distinct(plant_sp) |> 
  count()

# Model
overall_noout <- rma.mv(yi,vi, 
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix),
                        data = meta2)

# It does not change conclusion
summary(overall_noout)
confint(overall_noout)

# Checking normality
overall_noout_nrm <- rstandard(overall_noout)
res_overall_noout <- overall_noout_nrm$resid
qqnorm(res_overall_noout)
qqline(res_overall_noout)


## ## ## ## ## ## ## ##
# MODERATORS  ---------
## ## ## ## ## ## ## ##


## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## MEASURE -------------------------------------

### Models  -------------------------------------

# With outlier

type_out <- rma.mv(yi,vi, mods =~ measure_type,
                   random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                   R = list(plant_sp2 = cov.matrix),
                   data = meta)
summary(type_out)
confint(type_out)

# Checking normality
type_out_nrm <- rstandard(type_out)
type_out_res <- type_out_nrm$resid
qqnorm(type_out_res)
qqline(type_out_res)


# Without outlier
type_noout <- rma.mv(yi,vi, mods =~ measure_type,
                     random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                     R = list(plant_sp2 = cov.matrix),
                     data = meta2)
summary(type_noout)
confint(type_noout)

# Checking normality
type_noout_nrm <- rstandard(type_noout)
type_noout_res <- type_noout_nrm$resid
qqnorm(type_noout_res)
qqline(type_noout_res)


### Plot (measure) --------------------------------------------------

#Model
type2fig <- rma.mv(yi,vi, mods =~ measure_type - 1,
                   random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                   R = list(plant_sp2 = cov.matrix),
                   data = meta2)

# Plot
tiff("fig1.tif", width = 1700, height = 1400, res = 300, compression = "lzw")
orchard_plot(type2fig, 
             mod = "Measure type", xlab = "Hedges' g \n", alpha = 0.4) +
  theme_classic() + 
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        legend.position = "top") +
  scale_y_discrete(labels = c("Visitation", "Plant performance")) +
  ylab("Pollination metric") + 
  coord_flip() +
  scale_colour_manual(values = c("#fb8dd2", "#89ffc6")) +
  scale_fill_manual(values = c("magenta3", "springgreen3")) +
  scale_size(range = c(0.5, 6), name = "Precision") +
  guides(size = guide_legend(override.aes = list(colour = "gray")))
dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## EFN LOCATION  -------------------------------

### Models-------------------------------------

# Model with outlier
mresource_out <- rma.mv(yi,vi, 
                        mods = ~ resource_location : measure_type,
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix), 
                        data = meta)

summary(mresource_out)
confint(mresource_out)

# Checking normality
mresource_out_nrm <- rstandard(mresource_out)
mresource_out_res <- mresource_out_nrm$resid
qqnorm(mresource_out_res)
qqline(mresource_out_res)

# Model without outlier
mresource_noout <- rma.mv(yi,vi, 
                          mods = ~ resource_location : measure_type,
                          random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                          R = list(plant_sp2 = cov.matrix),
                          data = meta2)

summary(mresource_noout)
confint(mresource_noout)

# Checking normality
mresource_noout_nrm <- rstandard(mresource_noout)
mresource_noout_res <- mresource_noout_nrm$resid
qqnorm(mresource_noout_res)
qqline(mresource_noout_res)


### Plot  ----------------------------------------------------

# Model to plot
mresource_fig <- rma.mv(yi,vi, 
                        mods = ~ measure_type : resource_location  - 1,
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix),
                        data = meta2)

# Plot
tiff("fig2.tif", width = 1900, height = 1400, res = 300, compression = "lzw")
orchard_plot(mresource_fig, mod = "measure_type : resource_location", 
                      xlab = "Hedges' g \n", alpha = 0.4) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14), 
        axis.ticks.x = element_blank(),
        legend.position = "top") +
  scale_y_discrete(labels = c("", "", "", "")) +
  coord_flip(clip = "off", xlim = c(-2, 6)) +
  annotate(geom = "text", label = "Vegetative", x = -2.9, y = 1.5, size = 5) +
  annotate(geom = "text", label = "Reproductive", x = -2.9, y = 3.5, size = 5) +
  scale_colour_manual(values = c("#fb8dd2", "#89ffc6", "#fb8dd2", "#89ffc6")) +
  scale_fill_manual(values = c("magenta3",  "springgreen3","magenta3", "springgreen3")) +
  scale_size(range = c(0.5, 6), name = "Precision") +
  labs(y = "\n EFN location") +
  guides(size = guide_legend(override.aes = list(colour = "gray"))) +
  geom_point(x = 5, y = 2.8, col = "magenta3", size = 3) +
  annotate("text", label = "Floral visits", x = 5, y = 3.4, size = 4) +
  geom_point(x = 4, y = 2.8, col = "springgreen3", size = 3) +
  annotate("text", label = "Plant performance", x = 4, y = 3.8, size = 4)
dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## FLORAL VISITORS  -------------------

## Description

# This model includes only visitation metrics, then, it does not have the outlier

# Checking functional groups
meta |> 
  group_by(functional_group) |> 
  count()

# Removing wasps and diptera
fvis <- meta |> 
  filter(!functional_group == "wasps") |> 
  filter(!functional_group == "diptera")

# Checking number of studies remained for this moderator
fvis |> 
  select(ID, functional_group) |> 
  filter(!is.na(functional_group)) |>  
  distinct(ID) |>  
  count()

# Checking functional groups
fvis |> 
  group_by(functional_group) |> 
  count()


### Model (fv) ---------------------------

# Dropping tips from the phylogeny by keeping only the tips from the fvis dataset
## First: Creating an object with species names
fvis_plants <- unique(fvis$plant_sp2)

# Keeping tips from this data
fvis_phylo <- keep.tip(phy = ultmetree, tip = fvis_plants)

# Creating a new variance-covariance matrix
cov.matrix_vis <-  vcv(fvis_phylo, corr = TRUE)

# Model
mfloralvisitor <- rma.mv(yi, vi, mods = ~functional_group,
                         random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                         R = list(plant_sp2 = cov.matrix_vis),
                         data = fvis)

summary(mfloralvisitor)
confint(mfloralvisitor)


# Checking normality
mfloralvisitor_nrm <- rstandard(mfloralvisitor)
mofloralvisitor_res <- mfloralvisitor_nrm$resid
qqnorm(mofloralvisitor_res)
qqline(mofloralvisitor_res)

### Plot (fv) ------------------------------------------------------

# Model to plot
mfloralvisitor_fig <- rma.mv(yi, vi, mods = ~functional_group - 1,
                             random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                             R = list(plant_sp2 = cov.matrix_vis),
                             data = fvis)

# Plot
tiff("fig3.tif", width = 1900, height = 1400, res = 300, compression = "lzw")
orchard_plot(mfloralvisitor_fig, mod = "functional_group", xlab = "Hedges' g \n") +
  theme_classic() + 
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        legend.position = "top") +
  scale_y_discrete(labels = c("Bee", "Butterfly")) +
  coord_flip() +
  #scale_x_continuous(limits = c(-2,6))+
  scale_size(range = c(0.5, 6), name = "Precision") +
  scale_colour_manual(values = rep("#fb8dd2", times = 2)) +
  scale_fill_manual(values = rep("magenta3", times = 2)) +
  labs(y = "Flower visitors") +
  guides(size = guide_legend(override.aes = list(colour = "#fb8dd2")))
dev.off()

