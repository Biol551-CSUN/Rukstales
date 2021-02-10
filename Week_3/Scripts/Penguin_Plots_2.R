
### The purpose of this script is to plot penguin data ####
### Created by: Emily Rukstales ###########################
### Created on: 2021-02-10 ################################



### Load Libraries ########################################
library(palmerpenguins)
library(tidyverse)
library(here)
library(praise)
library(devtools)
library(beyonce)
library(PNWColors)


### Load Data #############################################
glimpse(penguins)
view(penguins)


### Data Analysis ########################################

# Plot from lecture
plot1 <- ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) +
  geom_point() +
  geom_smooth(method = "lm") + # does a linear regression for each group (species)
    labs(x = "Bill Depth (mm)",
         y = "Bill Length (mm)") +
    scale_color_manual(values = beyonce_palette(2)) + # use beyonce color palette for points/lines
    theme_bw() +
    theme(axis.title = element_text(size = 20), # change axes label size
          panel.background = element_rect(fill = "linen")) + # fill background with linen color
  ggsave(here("Week_3", "Output", "PenguinPlot1.png"),
         width = 7, height = 5)
    #coord_flip()
    #scale_color_manual(values = c("orange", "purple", "green"))
    #scale_color_viridis_d() +
    #scale_x_continuous(limits = c(0,20)) + #sets x axis limits from 0 to 20
    #scale_y_continuous(limits = c(0,50)) +
    #scale_x_continuous(breaks = c(14,17,21),
                       #labels = c("low", "medium", "high"))
   


# Lab/group plots

penguins<-penguins%>%
  drop_na(sex) # get rid of NA values from sex column

view(penguins) # view new dataframe

pal <- pnw_palette("Winter",2) # store color palette, use 2 colors

# Plot with facet_wrap by island
ggplot(data = penguins,
       mapping = (aes(x = species,
                      y = flipper_length_mm,
                      color = sex, # color of points and lines in boxplot
                      fill = sex))) + # fill in boxplot with color
      geom_boxplot(alpha = 0.5) + # create boxplot with some transparency
      geom_jitter(alpha = 0.6, # jitter points and make points transparent
                  position = position_jitterdodge( # adjust points so that they stay within each boxplot
                    jitter.width = NULL,
                    jitter.height = 0,
                    dodge.width = 0.75)) +
      facet_wrap(~island, ncol = 3) + # make a separate plot for each island
        labs(x = "Species",
             y = "Flipper Length (mm)",
             color = "Sex",
             fill = "Sex",
             title = "Flipper length of male and female penguins",
             subtitle = "Penguin distribution by island") +
      scale_color_manual(values = pal, # set color or points and boxplot lines using pal color palette
                         labels = c("Female", "Male")) + # change labels on legend
      scale_fill_manual(values = pal, # fill boxplots with color
                        labels = c("Female", "Male")) +
      theme_bw() +
  ggsave(here("Week_3", "Output", "PenguinPlotLab1.png"),
         width = 7, height = 5)


# Plot without islands
ggplot(data = penguins,
       mapping = (aes(x = species,
                      y = flipper_length_mm,
                      color = sex,
                      fill = sex))) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(alpha = 0.6,
              position = position_jitterdodge(
                jitter.width = NULL,
                jitter.height = 0,
                dodge.width = 0.75)) +
  labs(x = "Species",
       y = "Flipper Length (mm)",
       color = "Sex",
       fill = "Sex",
       title = "Flipper length of male and female penguins",
       subtitle = "Penguin distribution by island") +
  scale_color_manual(values = pal,
                     labels = c("Female", "Male")) +
  scale_fill_manual(values = pal,
                    labels = c("Female", "Male")) +
  theme_bw() +
  ggsave(here("Week_3", "Output", "PenguinPlotLab2.png"),
         width = 7, height = 5)










