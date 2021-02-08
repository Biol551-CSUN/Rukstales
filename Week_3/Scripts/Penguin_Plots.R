### Making plots with penguin dataset
### Created by: Emily Rukstales
### Created on: 2/8/2021
##############################################################

### Load libraries ###########################################
library(palmerpenguins)
library(tidyverse)


### Data #####################################################
glimpse(penguins)


### Plots ####################################################
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
                     size = body_mass_g,
                     alpha = flipper_length_mm)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #color palette for color blindness


# Faceting
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           color = species)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(species~sex) + #facet_grid makes everything a square, facet_wrap allows for more manipulation
  guides(color = FALSE)
