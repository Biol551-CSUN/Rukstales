### The purpose of this script is to plot penguin data and use the {dplyr} package
### Created by Emily Rukstales
### Created on 2021-02-15


### Load libraries
library(palmerpenguins)
library(tidyverse)
library(here)
library(devtools)
library(dadjoke)


### Data analysis
view(penguins)
filter(.data = penguins, sex == "female")

filter(.data = penguins, year == "2008")
filter(.data = penguins, body_mass_g > 5000)

filter(.data = penguins, sex == "female", body_mass_g > 4000) #filter with multiple conditions

filter(.data = penguins, year == "2008" | year == "2009")
filter(.data = penguins, island != "Dream")
filter(.data = penguins, species == "Adelie" & species == "Gentoo")

penguins2<-mutate(.data = penguins,
                  body_mass_kg = body_mass_g/1000, #convert mass to kg
                  bill_length_depth = bill_length_mm/bill_depth_mm) #calculate ratio of bill length to depth
view(penguins2)

penguins2<-mutate(.data = penguins2, after_2008 = ifelse(year >= 2008, "After 2008", "Before 2008"))
View(penguins2)

penguins2 <- mutate(.data = penguins2,
                    flipper_length_mass = flipper_length_mm + body_mass_g)
penguins2 <- mutate(.data = penguins2,
                    sex_capital = ifelse(sex == "male", "Male", "Female")) #if sex is male, change it to Male; if sex is NOT male (female) change it to Female
view(penguins2)


penguins3 <- penguins %>% #make penguins3 dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log of biomass
  select(species, island, sex, log_mass) #select columns to keep in new dataframe
view(penguins3)


penguins %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE), #calculate mean and minimum flipper length
            min_flipper = min(flipper_length_mm, na.rm = TRUE))


penguins %>%
  group_by(island) %>% #will group the summaries by island
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm = TRUE))


penguins %>%
  group_by(island, sex) %>% #will group the summaries by island and sex
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm = TRUE))


penguins %>%
  drop_na(sex)

penguins %>%
  drop_na(sex) %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))


penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()



groan()











