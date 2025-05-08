### Effect of ants on the pollination of attended plants ###
### Heterogeneity ###
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
         ant_aggressiveness = as.factor(ant_aggressiveness),
         measure_type = as.factor(measure_type))

# Checking data
glimpse(meta)
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
plot(ultmetree,no.margin = TRUE, cex=0.9, align.tip.label = TRUE, type = "phylogram") 

# Creating the variance-covariance matrix
cov.matrix <-  vcv(ultmetree, corr = TRUE)


## ## ## ## ## ## ## ##
# OVERALL MODEL ------
## ## ## ## ## ## ## ##

# Writing sp2 as species in cov.matrix
meta$plant_sp2 <- gsub(" ", "_", x = meta$plant_sp)
meta$plant_sp2 <- stringr::str_to_title(meta$plant_sp2)

## With outlier -----------------------

overall_out <- rma.mv(yi,vi, 
                  random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                  R = list(plant_sp2 = cov.matrix),
                  data = meta)

confint(overall_out)

## Heterogeneity (overall w/o) 

# Equation 20 (Nakagawa & Santos 2012)
meta$wi <-  1/(meta$vi) 

# Equation 22 (Nakagawa & Santos 2012)
s2m.02 <- sum(meta$wi*(length(meta$wi)-1))/(sum(meta$wi)^2-sum(meta$wi^2))

## ID (between-study difference)
I2_ID <-  (overall_out$sigma2[1]/
             (overall_out$sigma2[1]+overall_out$sigma2[2]+overall_out$sigma2[3]+s2m.02)) * 100
I2_ID

I2_ID - qchisq(0.95, df=1)/2
I2_ID + qchisq(0.95, df=1)/2

## Plant phylogeny
I2_phylogeny <- (overall_out$sigma2[2]/
                   (overall_out$sigma2[1]+overall_out$sigma2[2]+overall_out$sigma2[3]+s2m.02)) * 100
I2_phylogeny

I2_phylogeny - qchisq(0.95, df=1)/2
I2_phylogeny + qchisq(0.95, df=1)/2

## Plant species
I2_species <- (overall_out$sigma2[3]/
                   (overall_out$sigma2[1]+overall_out$sigma2[2]+overall_out$sigma2[3]+s2m.02)) * 100
I2_species

I2_species - qchisq(0.95, df=1)/2
I2_species + qchisq(0.95, df=1)/2

## Total
I2_total <-  ((overall_out$sigma2[1]+overall_out$sigma2[2]+overall_out$sigma2[3])/
                (overall_out$sigma2[1]+overall_out$sigma2[2]+overall_out$sigma2[3]+s2m.02)) * 100
I2_total

I2_total - qchisq(0.95, df=1)/2
I2_total + qchisq(0.95, df=1)/2


## Without outlier ------------------

# Repeating analysis without outlier (it has a hedges' g value of -28)
meta2 <- meta[-meta$yi < abs(28),]

# Model
overall_noout <- rma.mv(yi,vi, 
                   random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                   R = list(plant_sp2 = cov.matrix),
                   data = meta2)

confint(overall_noout)

## Heterogeneity (without outlier)

# Equation 20 (Nakagawa & Santos 2012)
meta2$wi <-  1/(meta2$vi) 

# Equation 22 (Nakagawa & Santos 2012)
s2m.02wo <- sum(meta2$wi*(length(meta2$wi)-1))/(sum(meta2$wi)^2-sum(meta2$wi^2))

## ID (between-study difference)
I2_ID2 <-  (overall_noout$sigma2[1]/
              (overall_noout$sigma2[1]+overall_noout$sigma2[2]+overall_noout$sigma2[3]+s2m.02wo)) * 100
I2_ID2

I2_ID2 - qchisq(0.95, df=1)/2
I2_ID2 + qchisq(0.95, df=1)/2


## Plant phylogeny
I2_phylogeny2 <- (overall_noout$sigma2[2]/
                    (overall_noout$sigma2[1]+overall_noout$sigma2[2]+overall_noout$sigma2[3]+s2m.02wo)) * 100
I2_phylogeny2

I2_phylogeny2 - qchisq(0.95, df=1)/2
I2_phylogeny2 + qchisq(0.95, df=1)/2

## Plant species
I2_species <- (overall_noout$sigma2[3]/
                    (overall_noout$sigma2[1]+overall_noout$sigma2[2]+overall_noout$sigma2[3]+s2m.02wo)) * 100
I2_species

I2_species - qchisq(0.95, df=1)/2
I2_species + qchisq(0.95, df=1)/2

## Total
I2_total2 <-  ((overall_noout$sigma2[1]+overall_noout$sigma2[2]+overall_noout$sigma2[3])/
                 (overall_noout$sigma2[1]+overall_noout$sigma2[2]+overall_noout$sigma2[3]+s2m.02wo)) * 100
I2_total2

I2_total2 - qchisq(0.95, df=1)/2
I2_total2 + qchisq(0.95, df=1)/2


## ## ## ## ## ## ## ##
# MODERATORS  ---------
## ## ## ## ## ## ## ##


## ## ## ## ## ## ## ## 
## MEASURE ------------

# Transforming measure type in factor
meta$measure_type <- as.factor(meta$measure_type)

# With outlier
type_out <- rma.mv(yi,vi, mods =~ measure_type,
                   random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                   R = list(plant_sp2 = cov.matrix),
                   data = meta)

confint(type_out)



### With outlier (measure) -------------------------------------------------

## ID (between-study difference)
I2_ID_mt <-  (type_out$sigma2[1]/
                (type_out$sigma2[1]+type_out$sigma2[2]+type_out$sigma2[3]+s2m.02)) * 100
I2_ID_mt

I2_ID_mt - qchisq(0.95, df=1)/2
I2_ID_mt + qchisq(0.95, df=1)/2


## Plant phylogeny
I2_phylogeny_mt <- (type_out$sigma2[2]/
                      (type_out$sigma2[1]+type_out$sigma2[2]+type_out$sigma2[3]+s2m.02)) * 100
I2_phylogeny_mt

I2_phylogeny_mt - qchisq(0.95, df=1)/2
I2_phylogeny_mt + qchisq(0.95, df=1)/2

## Plant species
I2_species_mt <- (type_out$sigma2[3]/
                      (type_out$sigma2[1]+type_out$sigma2[2]+type_out$sigma2[3]+s2m.02)) * 100
I2_species_mt

I2_species_mt - qchisq(0.95, df=1)/2
I2_species_mt + qchisq(0.95, df=1)/2


## Total
I2_total2_mt <- ((type_out$sigma2[1]+type_out$sigma2[2]+type_out$sigma2[3])/
                   (type_out$sigma2[1]+type_out$sigma2[2]+type_out$sigma2[3]+s2m.02)) * 100
I2_total2_mt

I2_total2_mt - qchisq(0.95, df=1)/2
I2_total2_mt + qchisq(0.95, df=1)/2


### Without outlier (measure) ----------------------------------------------

# Heterogeneity without outlier
type_noout <- rma.mv(yi,vi, mods =~ measure_type,
                     random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                     R = list(plant_sp2 = cov.matrix),
                     data = meta2)

confint(type_noout)

## ID (between-study difference)
I2_ID_mtwo <-  (type_noout$sigma2[1]/
                  (type_noout$sigma2[1]+type_noout$sigma2[2]+type_noout$sigma2[3]+s2m.02wo)) * 100
I2_ID_mtwo

I2_ID_mtwo - qchisq(0.95, df=1)/2
I2_ID_mtwo + qchisq(0.95, df=1)/2


## Plant phylogeny
I2_phylogeny_mtwo <- (type_noout$sigma2[2]/
                        (type_noout$sigma2[1]+type_noout$sigma2[2]+type_noout$sigma2[3]+s2m.02wo)) * 100
I2_phylogeny_mtwo

I2_phylogeny_mtwo - qchisq(0.95, df=1)/2
I2_phylogeny_mtwo + qchisq(0.95, df=1)/2

## Plant species
I2_species_mtwo <- (type_noout$sigma2[3]/
                        (type_noout$sigma2[1]+type_noout$sigma2[2]+type_noout$sigma2[3]+s2m.02wo)) * 100
I2_species_mtwo

I2_species_mtwo - qchisq(0.95, df=1)/2
I2_species_mtwo + qchisq(0.95, df=1)/2


## Total
I2_total2_mtwo <- ((type_noout$sigma2[1]+type_noout$sigma2[2]+type_noout$sigma2[3])/
                     (type_noout$sigma2[1]+type_noout$sigma2[2]+type_noout$sigma2[3]+s2m.02wo)) * 100
I2_total2_mtwo

I2_total2_mtwo - qchisq(0.95, df=1)/2
I2_total2_mtwo + qchisq(0.95, df=1)/2


## ## ## ## ## ## ## ## 
## EFN LOCATION -------

### With outlier (EFN) -------

# Model
mresource_out <- rma.mv(yi,vi, 
                        mods = ~ resource_location : measure_type,
                        random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                        R = list(plant_sp2 = cov.matrix), 
                        data = meta)

confint(mresource_out)


## ID (between-study difference)
I2_ID_efn <-  (mresource_out$sigma2[1]/
                 (mresource_out$sigma2[1]+mresource_out$sigma2[2]+mresource_out$sigma2[3]+s2m.02)) * 100
I2_ID_efn

I2_ID_efn - qchisq(0.95, df=1)/2
I2_ID_efn + qchisq(0.95, df=1)/2

## Plant phylogeny
I2_phylogeny_efn <- (mresource_out$sigma2[2]/
                       (mresource_out$sigma2[1]+mresource_out$sigma2[2]+mresource_out$sigma2[3]+s2m.02)) * 100
I2_phylogeny_efn

I2_phylogeny_efn - qchisq(0.95, df=1)/2
I2_phylogeny_efn + qchisq(0.95, df=1)/2

## Plant species
I2_species_efn <- (mresource_out$sigma2[3]/
                       (mresource_out$sigma2[1]+mresource_out$sigma2[2]+mresource_out$sigma2[3]+s2m.02)) * 100
I2_species_efn

I2_species_efn - qchisq(0.95, df=1)/2
I2_species_efn + qchisq(0.95, df=1)/2

## Total
I2_total2_efn <- ((mresource_out$sigma2[1]+mresource_out$sigma2[2]+mresource_out$sigma2[3])/
                    (mresource_out$sigma2[1]+mresource_out$sigma2[2]+mresource_out$sigma2[3]+s2m.02)) * 100
I2_total2_efn

I2_total2_efn - qchisq(0.95, df=1)/2
I2_total2_efn + qchisq(0.95, df=1)/2

### Without outlier (EFN) ---------------------

# Model without outlier
mresource_noout <- rma.mv(yi,vi, 
                          mods = ~ resource_location : measure_type,
                          random=list(~1|ID, ~1|plant_sp2, ~1|plant_sp),
                          R = list(plant_sp2 = cov.matrix),
                          data = meta2)

confint(mresource_noout)

## ID (between-study difference)
I2_ID_efnwo<-  (mresource_noout$sigma2[1]/
                 (mresource_noout$sigma2[1]+mresource_noout$sigma2[2]+mresource_noout$sigma2[3]+s2m.02)) * 100
I2_ID_efnwo

I2_ID_efnwo - qchisq(0.95, df=1)/2
I2_ID_efnwo + qchisq(0.95, df=1)/2

## Plant phylogeny
I2_phylogeny_efnwo <- (mresource_noout$sigma2[2]/
                       (mresource_noout$sigma2[1]+mresource_noout$sigma2[2]+mresource_noout$sigma2[3]+s2m.02)) * 100
I2_phylogeny_efnwo

I2_phylogeny_efnwo - qchisq(0.95, df=1)/2
I2_phylogeny_efnwo + qchisq(0.95, df=1)/2

## Plant species
I2_species_efnwo <- (mresource_noout$sigma2[3]/
                     (mresource_noout$sigma2[1]+mresource_noout$sigma2[2]+mresource_noout$sigma2[3]+s2m.02)) * 100
I2_species_efnwo

I2_species_efnwo - qchisq(0.95, df=1)/2
I2_species_efnwo + qchisq(0.95, df=1)/2

## Total
I2_total2_efnwo <- ((mresource_noout$sigma2[1]+mresource_noout$sigma2[2]+mresource_noout$sigma2[3])/
                    (mresource_noout$sigma2[1]+mresource_noout$sigma2[2]+mresource_noout$sigma2[3]+s2m.02)) * 100
I2_total2_efnwo

I2_total2_efnwo - qchisq(0.95, df=1)/2
I2_total2_efnwo + qchisq(0.95, df=1)/2


## ## ## ## ## ## ## ## ##
## FLORAL VISITORS  ----


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

### Heterogeneity (fv) ----------------------------

## ID (between-study difference)
I2_ID_fv <-  (mfloralvisitor$sigma2[1]/
                (mfloralvisitor$sigma2[1]+mfloralvisitor$sigma2[2]+mfloralvisitor$sigma2[3]+s2m.02)) * 100
I2_ID_fv

I2_ID_fv - qchisq(0.95, df=1)/2
I2_ID_fv + qchisq(0.95, df=1)/2

## Plant phylogeny
I2_phylogeny_fv <- (mfloralvisitor$sigma2[2]/
                      (mfloralvisitor$sigma2[1]+mfloralvisitor$sigma2[2]+mfloralvisitor$sigma2[3]+s2m.02)) * 100
I2_phylogeny_fv

I2_phylogeny_fv - qchisq(0.95, df=1)/2
I2_phylogeny_fv + qchisq(0.95, df=1)/2

## Plant species
I2_species_fv <- (mfloralvisitor$sigma2[3]/
                      (mfloralvisitor$sigma2[1]+mfloralvisitor$sigma2[2]+mfloralvisitor$sigma2[3]+s2m.02)) * 100
I2_species_fv

I2_species_fv - qchisq(0.95, df=1)/2
I2_species_fv + qchisq(0.95, df=1)/2

## Total
I2_total2_fv <- ((mfloralvisitor$sigma2[1]+mfloralvisitor$sigma2[2]+mfloralvisitor$sigma2[3])/
                   (mfloralvisitor$sigma2[1]+mfloralvisitor$sigma2[2]+mfloralvisitor$sigma2[3]+s2m.02)) * 100
I2_total2_fv

I2_total2_fv - qchisq(0.95, df=1)/2
I2_total2_fv + qchisq(0.95, df=1)/2


