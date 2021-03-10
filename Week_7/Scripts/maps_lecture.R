### Learning how to make maps
### Created by Emily Rukstales
### Created on: 2021-03-08
##################################################################



### Load Libraries ###############################################
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)



### Load data ####################################################

popdata <- read_csv(here("Week_7", "Data", "CApopdata.csv"))

stars <- read_csv(here("Week_7", "Data", "stars.csv"))



### Making maps ##################################################

#view lat and long for specific region (country, state, county etc.)
world <- map_data("world")
head(world)

usa <- map_data("usa")
head(usa)

states <- map_data("state")
head(states)

counties <- map_data("county")
head(counties)


#make world map
ggplot() +
  geom_polygon(data = world,
               aes(x = long,
                   y = lat,
                   group = group, #always remember to use group
                   fill = region),
               color = "black") + #makes outlines black
  guides(fill = FALSE) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_map(projection = "mercator", #mercator projection: good for USA, bad for everywhere else
            xlim = c(-180, 180))


#make a map of CA
CA_data <- states %>%
  filter(region == "california")

ggplot() + 
  geom_polygon(data = CA_data,
               aes(x = long,
                   y = lat,
                   group = group),
               color = "black",
               fill = "slateblue2") +
  coord_map(projection = "mercator") +
  theme_void()



#make a map of CA counties
#join popdata (has populations) with counties (has lat and long)
CApop_county <- popdata %>%
  select("subregion" = County, Population) %>% #to join data frames, need common column heading (subregion), keep Population column
  inner_join(counties) %>%
  filter(region == "california")

ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  coord_map() +
  theme_void() +
  scale_fill_gradient(trans = "log10")


#make a map of CA counties with seastars
ggplot() +
  geom_polygon(data = CApop_county,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = Population),
               color = "black") +
  geom_point(data = stars,
             aes(x = long,
                 y = lat,
                 size = star_no),
             color = "red",
             alpha = 0.7) +
  coord_map() +
  theme_void() +
  scale_fill_gradient(trans = "log10") +
  labs(size = "# stars/m2") +
  ggsave(here("Week_7", "Outputs", "CApop.pdf"))






