### Effect of ants on the pollination of attended plants ###
### Descriptive data ###
### Update: September 25th 2024 ###
### Amanda Vieira da Silva ###
### @mandyviesi ###

# Packages ------------------------------------------

library(dplyr)


# Data ---------------------------------------------

meta <- readxl::read_xlsx("meta.xlsx", sheet = 1)

# Checking data
glimpse(meta)
head(meta)
View(meta)


# Descriptive statistics -----------------

# Number of studies
meta |> 
  distinct(ID) |> 
  count()
19/27

# Number of plant species 
meta |> 
  distinct(plant_sp) |> 
  count()

# Type of ant effect per study
meta |> 
  select(ID, measure_type) |> 
  distinct(ID, measure_type) |>
  View()

# Plant family
meta |> 
  group_by(plant_fam) |> 
  count(plant_fam) |> 
  arrange(desc(n))

# Location of extrafloral nectaries
meta |> 
  group_by(resource_location, plant_sp) |> 
  count() |> 
  arrange() |> View()
names(meta)

# Floral visitors
meta |>  
  #group_by(ID) |> 
  summarise(functional_group) |>  
  count(functional_group) 

# Floral visitor NA
meta |>  
  group_by(ID) |>  
  summarise(functional_group) |>  
  count(functional_group) |> View()

