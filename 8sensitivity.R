### Effect of ants on the pollination of attended plants ###
### Sensitivity analysis ###
### Update: September 25th 2024 ###
### Amanda Vieira da Silva ### 
### @mandyviesi ###

# Packages ----------------------------

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

# The outlier
meta |>  filter(yi < -28)

# Repeating analysis without outlier (it has a hedges' g value of -28)
meta2 <- meta[-meta$yi < abs(28),]

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

# Creating the variance-covariance matrix
cov.matrix <-  vcv(ultmetree, corr = TRUE)

# Writing sp2 as species in cov.matrix
meta$plant_sp2 <- gsub(" ", "_", x = meta$plant_sp)
meta$plant_sp2 <- stringr::str_to_title(meta$plant_sp2)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# OVERALL MODEL --------------------------

## With outlier -------------------------

# Creating a dataset to save results
resm1 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

for(i in 1:nrow(meta)){
  
  # Data excluding each observation
  data_m1 <- meta[-i,]
  
  # Dropping tips from the phylogeny
  plants_m1 <- unique(data_m1$plant_sp2)
  
  # Keeping tips from this data
  phylo_m1 <- keep.tip(phy = ultmetree, tip = plants_m1)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m1 <-  vcv(phylo_m1, corr = TRUE)
  
  # Model
  overall_out <- rma.mv(yi,vi, 
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix_m1),
                        data = data_m1)
  
  # Saving results
  restemp <- data.frame(QM = overall_out$QM,
                        pvalue = overall_out$pval)
  
  restemp$iteration = i
  
  resm1 <- rbind(resm1, restemp)
}

# Checking results
glimpse(resm1)
summary(resm1)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm1 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))


## Without outlier -----------------------

# Creating a dataset to save results
resm2 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

for(i in 1:nrow(meta2)){
  
  # Data excluding each observation
  data_m2 <- meta2[-i,]
  
  # Dropping tips from the phylogeny
  plants_m2 <- unique(data_m2$plant_sp2)
  
  # Keeping tips from this data
  phylo_m2 <- keep.tip(phy = ultmetree, tip = plants_m2)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m2 <-  vcv(phylo_m2, corr = TRUE)
  
  # Model
  overall_out2 <- rma.mv(yi,vi, 
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix_m2),
                        control=list(optimizer="optim"),
                        data = data_m2)
  
  # Saving results
  restemp2 <- data.frame(QM = overall_out2$QM,
                        pvalue = overall_out2$pval)
  
  restemp2$iteration = i
  
  resm2 <- rbind(resm2, restemp2)
}

# Checking results
glimpse(resm2)
summary(resm2)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm2 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))


## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# POLLINATION METRIC ---------------------

## With outlier -------------------------

# Creating a dataset to save results
resm3 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

# Model 
for(i in 1:nrow(meta)){
  
  # Data excluding each observation
  data_m3 <- meta[-i,]
  
  # Dropping tips from the phylogeny
  plants_m3 <- unique(data_m3$plant_sp2)
  
  # Keeping tips from this data
  phylo_m3 <- keep.tip(phy = ultmetree, tip = plants_m3)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m3 <-  vcv(phylo_m3, corr = TRUE)
  
  # Model
  type_out <- rma.mv(yi,vi, mods =~ measure_type,
                     random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                     R = list(plant_sp2 = cov.matrix_m3),
                     data = data_m3)
  
  # Saving results
  restemp3 <- data.frame(QM = type_out$QM,
                         pvalue = type_out$pval[2])
  
  restemp3$iteration = i
  
  resm3 <- rbind(resm3, restemp3)
}

# Checking results
glimpse(resm3)
summary(resm3)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm3 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))

## Without outlier -------------------------

# Creating a dataset to save results
resm4 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

# Model 
for(i in 1:nrow(meta2)){
  
  # Data excluding each observation
  data_m4 <- meta2[-i,]
  
  # Dropping tips from the phylogeny
  plants_m4 <- unique(data_m4$plant_sp2)
  
  # Keeping tips from this data
  phylo_m4 <- keep.tip(phy = ultmetree, tip = plants_m4)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m4 <-  vcv(phylo_m4, corr = TRUE)
  
  # Model
  type_out2 <- rma.mv(yi,vi, mods =~ measure_type,
                     random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                     R = list(plant_sp2 = cov.matrix_m4),
                     data = data_m4)
  
  # Saving results
  restemp4 <- data.frame(QM = type_out2$QM,
                         pvalue = type_out2$pval[2])
  
  restemp4$iteration = i
  
  resm4 <- rbind(resm4, restemp4)
}

# Checking results
glimpse(resm4)
summary(resm4)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm4 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# EFN LOCATION  -------------------------------

## With outlier -------------------------

# Creating a dataset to save results
resm5 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

# Model 
for(i in 1:nrow(meta)){
  
  # Data excluding each observation
  data_m5 <- meta[-i,]
  
  # Dropping tips from the phylogeny
  plants_m5 <- unique(data_m5$plant_sp2)
  
  # Keeping tips from this data
  phylo_m5 <- keep.tip(phy = ultmetree, tip = plants_m5)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m5 <-  vcv(phylo_m5, corr = TRUE)
  
  # Model
  mresource_out <- rma.mv(yi,vi, 
                          mods = ~ resource_location : measure_type,
                          random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                          R = list(plant_sp2 = cov.matrix_m5), 
                          data = data_m5)
  
  # Saving results
  restemp5 <- data.frame(QM = mresource_out$QM,
                         pvalue = mresource_out$pval[2])
  
  restemp5$iteration = i
  
  resm5 <- rbind(resm5, restemp5)
}

# Checking results
glimpse(resm5)
summary(resm5)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm5 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))

## Without outlier -------------------------

# Creating a dataset to save results
resm6 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

# Model 
for(i in 1:nrow(meta2)){
  
  # Data excluding each observation
  data_m6 <- meta2[-i,]
  
  # Dropping tips from the phylogeny
  plants_m6 <- unique(data_m6$plant_sp2)
  
  # Keeping tips from this data
  phylo_m6 <- keep.tip(phy = ultmetree, tip = plants_m6)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m6 <-  vcv(phylo_m6, corr = TRUE)
  
  # Model
  mresource_out2 <- rma.mv(yi,vi, 
                          mods = ~ resource_location : measure_type,
                          random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                          R = list(plant_sp2 = cov.matrix_m6), 
                          data = data_m6)
  
  # Saving results
  restemp6 <- data.frame(QM = mresource_out2$QM,
                         pvalue = mresource_out2$pval[2])
  
  restemp6$iteration = i
  
  resm6 <- rbind(resm6, restemp6)
}

# Checking results
glimpse(resm6)
summary(resm6)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm6 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# FLORAL VISITORS  ---------------------------

# Removing wasps and diptera
fvis <- meta2 |> 
  filter(!functional_group == "wasps") |> 
  filter(!functional_group == "diptera")

# Creating a dataset to save results
resm7 <- data.frame(QM = NULL, 
                    pvalue = NULL, 
                    iteration = NULL)

# Model 
for(i in 1:nrow(fvis)){
  
  # Data excluding each observation
  data_m7 <- fvis[-i,]
  
  # Dropping tips from the phylogeny
  plants_m7 <- unique(data_m7$plant_sp2)
  
  # Keeping tips from this data
  phylo_m7 <- keep.tip(phy = ultmetree, tip = plants_m7)
  
  # Creating a new variance-covariance matrix
  cov.matrix_m7 <-  vcv(phylo_m7, corr = TRUE)
  
  # Model
  mfloralvisitor <- rma.mv(yi, vi, mods = ~functional_group,
                           random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                           R = list(plant_sp2 = cov.matrix_m7),
                           data = data_m7)
  
  # Saving results
  restemp7 <- data.frame(QM = mfloralvisitor$QM,
                         pvalue = mfloralvisitor$pval[2])
  
  restemp7$iteration = i
  
  resm7 <- rbind(resm7, restemp7)
}

# Checking results
glimpse(resm7)
summary(resm7)

# Summarising the result (mean and standard deviation values of QM and p-values)
resm7 |> 
  summarise(QMmean = mean(QM),
            QMsd = sd(QM),
            pmean = mean(pvalue),
            psd = sd(pvalue),
            pmin = min(pvalue),
            pmax = max(pvalue))
