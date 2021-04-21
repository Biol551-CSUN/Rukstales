### Lab Week 12: using factors to make a plot
### Created by Emily Rukstales
### Created on 2021-04-21
################################################################################


### Load Libraries ###

library(tidyverse)
library(here)
library(PNWColors)

### Load Data ###

rm(list = ls()) #clear environment
intertidal <- read_csv(here("Week_12", "Data", "intertidaldata.csv")) 
intertidal_latitude <- read_csv(here("Week_12", "Data", "intertidaldata_latitude.csv"))


### Data wrangling ###

intertidal$Quadrat <- intertidal$Quadrat %>% #remove extra characters from values in Quadrat
  str_replace_all(" ", "")%>%
  str_replace_all("\\.", "") %>%
  str_replace_all("1", "") 
    
intertidal <- intertidal %>%
  select(c(1:10)) %>% #don't want the counted organisms
  pivot_longer(c(4:10), #turn organism columns into one column
               names_to = "organism",
               values_to = "percent_cover") %>%
  mutate(Quadrat = factor(Quadrat, levels = c("Low", "Mid", "High")), #create factors
         Site = factor(Site, levels = c("Starfish Point", "Bodega", "Scripps")),
         Transect = factor(Transect),
         Organism = factor(organism, levels = c("Bare Rock", "Algae", "Small Barnacles", "Large Barnacles", "Gooseneck Barnacles", "Mussels", "Anemone"))) %>%
  group_by(Site, Transect, Quadrat) %>% 
  mutate(percent_cover = (percent_cover/sum(percent_cover))*100) %>% #recalculate percent covers since they didn't add up to 100
  ungroup()

levels(intertidal$Quadrat) #make sure all the levels look good
levels(intertidal$Site)
levels(intertidal$Transect)
levels(intertidal$Organism)

intertidal_summary <- intertidal %>%
  filter(Site == "Starfish Point" | Site == "Bodega" | Site == "Scripps") %>% #filter for 3 sites ad high, mid, and low latitudes
  droplevels() %>% #remove excess levels (sites that were filtered out)
  group_by(Site, Quadrat, Organism) %>%
  summarise(mean_percent_cover = mean(percent_cover)) #calculate mean % cover of each organism by site and tide height


### Plot ###

pal = pnw_palette("Sailboat", 7)

intertidal_summary %>%
  ggplot(aes(x = Quadrat, #tide height on x-axis
             y = mean_percent_cover, #percent cover on y-axis
             fill = Organism)) + #color by organism
  geom_bar(position = "stack", #create stacked bar plot
           stat = "identity") +
  facet_wrap(~Site) + #create plot for each site
  theme_bw() +
  theme(panel.grid = element_blank(), #remove gridlines
        axis.ticks.x = element_blank(), #remove x-axis ticks
        strip.background = element_rect(fill = "#f0f0f0"), #change panel title background to a lighter shade
        plot.margin = margin(20, 10, 20, 20)) + #add a small margin around the plot
  scale_y_continuous(expand = c(0, 0)) + #make the stacked bars stretch from bottom to top (no extra space)
  scale_fill_manual(values = pal) + #change color palette
  labs(title = "Percent cover of organisms in the rocky intertidal", #change titles and axis labels
       subtitle = "Sites from highest to lowest latitude",
       x = "\n Tide height",
       y = "Mean percent cover") +
  ggsave(here("Week_12", "Outputs", "Percent_cover.png"),
         width = 10, height = 6)
  








