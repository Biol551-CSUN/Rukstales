### Learning how to do joins #############
### Created by Emily Rukstales############
### Created on: 2021-02-22################


### Load Libraries ######
library(tidyverse)
library(here)
library(cowsay)


### Load Data ####
# Environmental data from each site
EnviroData <- read_csv(here("Week_5", "Data", "site.characteristics.data.csv"))

#Thermal performance data
TPCData <- read_csv(here("Week_5", "Data", "Topt_data.csv"))


### Data analysis ######
EnviroData_wide <- EnviroData %>%
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>%
  arrange(site.letter) #arrange the dataframe by site in descending order
View(EnviroData_wide)

#left join
FullData_left <- left_join(TPCData, EnviroData_wide) %>% #joins by site.letter (same between dataframes)
relocate(where(is.numeric), .after = where(is.character)) #rearranges columns, puts numeric data after the character data
head(FullData_left)

#summary stats
FullData_summary <- FullData_left %>%
  pivot_longer(cols = E:substrate.cover,
               names_to = "Variables",
               values_to = "Values") %>%
  group_by(site.letter, Variables) %>%
  summarise(Value_means = mean(Values),
            Value_vars = var(Values))

#create a tibble
T1 <- tibble(Site.ID = c("A", "B", "C", "D"),
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T2 <- tibble(Site.ID = c("A", "B", "D", "E"),
             pH = c(7.3, 7.8, 8.1, 7.9))
left_join(T1, T2)
right_join(T1, T2)
inner_join(T1, T2)
full_join(T1, T2) 
semi_join(T1, T2) #deletes all rows with missing data, deletes columns not in T1
anti_join(T1, T2) #only keeps things missing from T1

say("hello", by = "daemon")


