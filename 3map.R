### Effect of ants on the pollination of attended plants ###
### Spatial representation where studies were conducted ###
### By: Amanda Vieira da Silva ###

# Packages ---------------------------------------

library(dplyr)
library(ggmap)
library(maps)
library(ggplot2)
library(ggspatial)


# Data -------------------------------------------

mapping <- readxl::read_xlsx("map.xlsx")

# Checking
glimpse(mapping)

# Plot
tiff("map2.tif", height = 1800, width = 2800, res = 300, compression = "lzw")
ggplot() +  
  borders("world", fill="lightgray", colour="darkgray")+ 
  coord_fixed() + 
  geom_point(data = mapping, aes(x = longitude, y = latitude), 
             color = "purple",
             size = 2, 
             alpha = 0.6) + 
  scale_y_continuous(breaks = c(-45, 0, 45)) +
  scale_x_continuous(breaks = c(-120, -60, 0, 60, 120)) +
  theme(axis.title = element_blank(),
        axis.text = element_text(color = "darkgrey"),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "gray", 
                                        linetype = "dotted", linewidth = 0.2),
        panel.background = element_rect(fill = "aliceblue"))
#  annotation_north_arrow(style = north_arrow_orienteering(), 
#                         width = unit(0.8, "cm"), height = unit(0.8, "cm"),
#                         location = "tl") 
dev.off()
