### Advanced plotting techniques

### Load Libraries
library(tidyverse)
library(here)
library(palmerpenguins)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)

#### Patchwork package ###

#plot 1
p1 <- penguins %>%
  ggplot(aes(x = body_mass_g,
             y = bill_length_mm,
             color = species)) +
  geom_point()
p1


#plot 2
p2 <- penguins %>%
  ggplot(aes(x = sex,
           y = body_mass_g,
           color = species)) +
  geom_point() +
  geom_jitter(width = 0.2)
p2

#combine plots
p1 + p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

#stack plots vertically
p1/p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')


### ggrepel
view(mtcars)

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text_repel() + # creates a text label
  geom_point(color = 'red')



### gganimate
penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1 # The length of the pause between transitions
  )+
  ease_aes("sine-in-out") +
  ggtitle('Year: {closest_state}') +
  anim_save(here("Week_8","Outputs","mypengiungif.gif"))
