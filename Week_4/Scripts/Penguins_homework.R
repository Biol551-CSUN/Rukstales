### This script is for the Week 4a homework #######################
### Created by Emily Rukstales ####################################
### Created on 2021-02-15 #########################################


### Load libraries ################################################
library(tidyverse)
library(here)
library(palmerpenguins)
library(PNWColors)
  

### Homework Part 1 #################################################

view(penguins)
penguins_summary <- penguins %>%
  drop_na(species) %>%
  drop_na(island) %>%
  drop_na(sex) %>%
  group_by(species, island, sex) %>%
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            var_body_mass = var(body_mass_g, na.rm = TRUE))
view(penguins_summary)


### Homework Part 2 ###############################################

penguins_plot <- penguins %>%
  filter(sex != "male") %>%
  mutate(log_body_mass = log(body_mass_g)) %>%
  select(species, island, sex, log_body_mass)
view(penguins_plot)


pal <- pnw_palette("Starfish", 3)

ggplot(data = penguins_plot,
       mapping = aes(x = species,
                     y = log_body_mass,
                     color = island,
                     fill = island)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(alpha = 0.6, # jitter points and make points transparent
              position = position_jitterdodge( # adjust points so that they stay within each boxplot
                jitter.width = NULL,
                jitter.height = 0,
                dodge.width = 0.75)) +
  labs(x = "Species",
       y = "log(Body mass)",
       color = "Island",
       fill = "Island",
       title = "Penguin body mass by species and island") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme_bw() +
  ggsave(here("Week_4", "Output", "PenguinPlotLab.png"),
              width = 7, height = 5)















      