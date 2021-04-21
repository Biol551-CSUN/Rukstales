




### load libraries
library(tidyverse)
library(here)



### load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
income_mean <- tuesdata$income_mean



### Factors!

fruits<-factor(c("Apple", "Grape", "Banana"))
fruits

test<- c("A","1","2")
as.numeric(test)

#{forcats}
glimpse(starwars)

starwars %>%
  filter(!is.na(species)) %>%
  count(species, sort = TRUE)

star_counts<-starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>% #species with less than 3 individuals get lumped together as "other"
  count(species)
star_counts

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE),
             y = n)) +
  geom_col() +
  labs(x = "Species")


#reordering line plots
glimpse(income_mean)

total_income<-income_mean %>%
  group_by(year, income_quintile)%>%
  summarise(income_dollars_sum = sum(income_dollars))%>%
  mutate(income_quintile = factor(income_quintile))

total_income %>%
  ggplot(aes(x = year,
             y = income_dollars_sum,
             color = income_quintile)) +
  geom_line() #colored in random order

total_income%>%
  ggplot(aes(x = year,
             y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+
  geom_line()+
  labs(color = "income quantile")


x1 <- factor(c("Jan", "Mar", "Apr", "Dec"))
x1

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1


starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor
  filter(n>3) %>% # only keep species that have more than 3
  droplevels() #drop extra levels
starwars_clean 

levels(starwars_clean$species)

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor
  filter(n>3) %>% # only keep species that have more than 3
  droplevels() %>% #drop extra levels
  mutate(species = fct_recode(species, "Humanoid" = "Human"))
starwars_clean 




