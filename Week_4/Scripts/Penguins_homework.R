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
penguins_summary <- penguins %>% #create new dataframe to view summary
  drop_na(species, island, sex, body_mass_g) %>% #drop NAs
  group_by(species, island, sex) %>% #group by species, island, and sex
  summarise(mean_body_mass = mean(body_mass_g), #calc. mean body mass
            var_body_mass = var(body_mass_g)) #calc. variance of body mass
view(penguins_summary)


### Homework Part 2 ###############################################

penguins_plot <- penguins %>% #create dataframe for plot
  filter(sex != "male") %>% #filter out males
  mutate(log_body_mass = log(body_mass_g)) %>% #create new column for log(body mass)
  select(species, island, sex, log_body_mass) #keep only these columns
view(penguins_plot)


pal <- pnw_palette("Starfish", 3) #store palette as object

ggplot(data = penguins_plot, #create ggplot
       mapping = aes(x = species,
                     y = log_body_mass,
                     color = island,
                     fill = island)) +
  geom_boxplot(alpha = 0.7) + #make it a boxplot, lower transparency
  geom_jitter(alpha = 0.6, # jitter points and make points transparent
              position = position_jitterdodge( # adjust points so that they stay within each boxplot
                jitter.width = NULL,
                jitter.height = 0,
                dodge.width = 0.75)) +
  labs(x = "Species", #change labels
       y = "log(Body mass)",
       color = "Island",
       fill = "Island",
       title = "Penguin body mass by species and island") +
  scale_color_manual(values = pal) + #set color palette
  scale_fill_manual(values = pal) +
  theme_bw() + #choose theme
  ggsave(here("Week_4", "Output", "PenguinPlotLab.png"),
              width = 7, height = 5)















      