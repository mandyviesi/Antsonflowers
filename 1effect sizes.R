### Effect of ants on their attended plants ###
### Calculating effect sizes ###
### 23/09/2022 ###

# Packages  -----------

library(metafor)

## ### ## ## ## ##
# DATA ----------
## ## ## ## ## ##  

# Data

dados <- read.csv2(file = "V6_18_09_2022.csv")

# Checking data
glimpse(dados)
head(dados)
View(dados)


## ## ## ## ## ## ## ## ## ## ## ##
# Calculating effect sizes --------
## ## ## ## ## ## ## ## ## ## ## ##

# Calculating Hedges' g and deviation of d 
meta <- escalc(measure = "SMD",
               n1i = n_with_ants,
               n2i = n_without_ants,
               m1i = mean_with_ants,
               m2i = mean_without_ants,
               sd1i = sd_with_ants,
               sd2i = sd_without_ants,
               add.measure = TRUE,
               append = TRUE,
               data = dados)

writexl::write_xlsx(meta, "meta.xslx")

