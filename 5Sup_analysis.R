### Effect of ants on the pollination of attended plants ###
### Supplementary analysis ###
### 08/05/2023 ###


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
meta <- readxl::read_xlsx("meta.xlsx", sheet = 1)

# As factor
meta <- meta |> 
  mutate(ID = as.factor(ID),
         functional_group = as.factor(functional_group),
         resource_location = as.factor(resource_location),
         ant_aggressiveness = as.factor(ant_aggressiveness),
         measure_type = as.factor(measure_type))

# Checking data
glimpse(meta)
View(meta)

# Checking IDs that have measure both visitation and plant fitness
ID_check <- meta |> 
  group_by(ID, measure_type) |> 
  count()

# Checking both
ID_check$ID

# I just need this
ID_both <- c(17, 30, 32, 42, 183, 207, 215, 394)

# Subsetting
meta <- subset(meta, ID %in% ID_both)

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

# Without outlier
type_noout <- rma.mv(yi,vi, mods =~ measure_type,
                     random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                     R = list(plant_sp2 = cov.matrix),
                     data = meta2)
summary(type_noout)
confint(type_noout)


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
  scale_y_discrete(labels = c("Visitation", "Plant fitness")) +
  ylab("Ant effect") + 
  coord_flip() +
  scale_colour_manual(values = c("#fb8dd2", "#89ffc6")) +
  scale_fill_manual(values = c("magenta3", "springgreen3")) +
  scale_size(range = c(0.5, 6), name = "Precision") +
  guides(size = guide_legend(override.aes = list(colour = "gray")))
dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## EFN location ----------------------------------------

### Models----------------------------------

# Model with outlier
mresource_out <- rma.mv(yi,vi, 
                        mods = ~ resource_location : measure_type,
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix), 
                        data = meta)

summary(mresource_out)
confint(mresource_out)

# Model without outlier
mresource_noout <- rma.mv(yi,vi, 
                          mods = ~ resource_location : measure_type,
                          random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                          R = list(plant_sp2 = cov.matrix),
                          data = meta2)

summary(mresource_noout)
confint(mresource_noout)


### Plot  ----------------------------------------------------

# Model to plot
mresource_fig <- rma.mv(yi,vi, 
                        mods = ~ measure_type : resource_location  - 1,
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix),
                        data = meta2)

# Plot

fig2a <- orchard_plot(mresource_fig, mod = "measure_type : resource_location", 
                      xlab = "Hedges' g \n", alpha = 0.4) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14), 
        axis.ticks.x = element_blank(),
        legend.position = "top") +
  scale_y_discrete(labels = c("", "", "", "")) +
  coord_flip(clip = "off", xlim = c(-2, 6)) +
  annotate(geom = "text", label = "Non-reproductive", x = -2.9, y = 1.5, size = 5) +
  annotate(geom = "text", label = "Reproductive", x = -2.9, y = 3.5, size = 5) +
  scale_colour_manual(values = c("#fb8dd2", "#89ffc6", "#fb8dd2", "#89ffc6")) +
  scale_fill_manual(values = c("magenta3",  "springgreen3","magenta3", "springgreen3")) +
  scale_size(range = c(0.5, 6), name = "Precision") +
  labs(y = "\n EFN location") +
  guides(size = guide_legend(override.aes = list(colour = "gray")))
fig2a

## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ANT BEHAVIOR   ----------------------------------------------------

## Description

# Counting
meta |> 
  group_by(ID) |> 
  count(ant_agressiveness) |> View()

# Dominant ant species
meta |> 
  select(most_frequent_ant_sp, ant_agressiveness) |> 
  filter(ant_agressiveness == "dominant") |> 
  group_by(most_frequent_ant_sp) |> 
  count() +
  arrange(desc(n))

# Subordinate ant species
meta |> 
  select(most_frequent_ant_sp, ant_agressiveness) |> 
  filter(ant_agressiveness == "asubordinate") |> 
  group_by(most_frequent_ant_sp) |> 
  count() |> 
  arrange(desc(n))

# Checking number of studies remained for this moderator
meta |> 
  select(ID, ant_agressiveness) |> 
  filter(!is.na(ant_agressiveness)) |> 
  distinct(ID) |>  
  count()


### Models  --------------------------------------------------

# Model with outlier
maggressiveness_out <- rma.mv(yi, vi, mods = ~ant_aggressiveness : measure_type,
                              random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                              R = list(plant_sp2 = cov.matrix),
                              data = meta)

summary(maggressiveness_out)
confint(maggressiveness_out)


# Model without outlier

# Checking number of studies remained for this moderator
meta2 |> 
  select(ID, ant_aggressiveness) |> 
  filter(!is.na(ant_aggressiveness)) |>  
  distinct(ID) |> 
  count()


# Model
maggressiveness_noout <- rma.mv(yi, vi, mods = ~ant_aggressiveness : measure_type,
                                random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp), 
                                R = list(plant_sp2 = cov.matrix),
                                control = list(optimizer = "optim"),
                                data = meta2)

summary(maggressiveness_noout)
confint(maggressiveness_noout)



### Plot -------------------------------------------------------

# Model to plot
maggressiveness2fig <- rma.mv(yi, vi, mods = ~measure_type : ant_aggressiveness - 1,
                              random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                              R = list(plant_sp2 = cov.matrix),
                              data = meta2)

# Plot
fig2b <- orchard_plot(maggressiveness2fig, 
                      mod = "ant_agressiveness", xlab = "Hedges' g \n") +
  theme_classic() + 
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        legend.position = "top") +
  scale_y_discrete(labels = c("", "", "", "")) +
  coord_flip(clip = "off", xlim = c(-2, 6)) +
  annotate(geom = "text", label = "Subordinate", x = -2.9, y = 1.5, size = 5) +
  annotate(geom = "text", label = "Dominant", x = -2.9, y = 3.5, size = 5) +
  scale_colour_manual(values = c("#fb8dd2", "#89ffc6", "#fb8dd2", "#89ffc6")) +
  scale_fill_manual(values = c("magenta3", "springgreen3", "magenta3","springgreen3")) +
  scale_size(range = c(0.5, 6), name = "Precision") +
  labs(y = "\n Ant aggressiveness") +
  guides(size = guide_legend(override.aes = list(colour = "gray"))) +
  geom_point(x = 5, y = 3, col = "magenta3", size = 3) +
  annotate("text", label = "Visitation", x = 5, y = 3.4, size = 4) +
  geom_point(x = 4, y = 3, col = "springgreen3", size = 3) +
  annotate("text", label = "Plant fitness", x = 4, y = 3.5, size = 4)
fig2b



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## FLOWER VISITORS  -------------------

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

mfloralvisitor <- rma.mv(yi, vi, mods = ~functional_group,
                         random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                         R = list(plant_sp2 = cov.matrix),
                         data = fvis)

summary(mfloralvisitor)
confint(mfloralvisitor)


### Plot (fv) ------------------------------------------------------

# Model to plot
mfloralvisitor_fig <- rma.mv(yi, vi, mods = ~functional_group - 1,
                             random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                             R = list(plant_sp2 = cov.matrix),
                             data = fvis)

# Plot
tiff("fig3.tif", width = 3500, height = 2000, compression = "lzw", res = 350)
orchard_plot(mfloralvisitor_fig, mod = "functional_group", xlab = "Hedges' g \n") +
  theme_classic() + 
  theme(axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 14),
        legend.position = "top") +
  scale_y_discrete(labels = c("Hymenoptera", "Lepidoptera")) +
  coord_flip() +
  scale_size(range = c(0.5, 6), name = "Precision") +
  scale_colour_manual(values = rep("#fb8dd2", times = 2)) +
  scale_fill_manual(values = rep("magenta3", times = 2)) +
  labs(y = "Identity of flower visitors") +
  guides(size = guide_legend(override.aes = list(colour = "#fb8dd2")))
dev.off()

# Figure 2 ----------------------------

tiff("fig2.tif", width = 3500, height = 2000, compression = "lzw", res = 350)
ggpubr::ggarrange(fig2a, fig2b, ncol = 2, common.legend = TRUE)
dev.off()
