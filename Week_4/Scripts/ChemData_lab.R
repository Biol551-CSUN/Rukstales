### This script is for the week 4b lab assignment ##############################
### Created by Emily Rukstales #################################################
### Created on 2021-02-17 ######################################################
################################################################################

### Load Libraries #############################################################
library(tidyverse)
library(here)


### Load Data ##################################################################
ChemData <- read_csv(here("Week_4", "Data", "chemicaldata_maunalua.csv"))


### Data Analysis ##############################################################

# Summary statistics
ChemData_summary <- ChemData %>%
  filter(complete.cases(.)) %>% #filter out NAs
  separate(col = Tide_time, #separate tide and time columns
           into = c("Tide", "Time"),
           sep = "_")%>% #splits data everywhere there's an underscore
  filter(Season == "SPRING") %>%
  pivot_longer(cols = Temp_in:percent_sgd, #pivot columns into long columns
               names_to = "Variables", #one column for all the variable names
               values_to =  "Values") %>% #one column for all the values
  group_by(Variables, Site, Tide) %>% #select these columns
  summarise (Value_means = mean(Values, na.rm = TRUE)) %>% #calculate means
  pivot_wider(names_from = Variables, #change back to wide format
              values_from = Value_means)

View(ChemData_summary)
  

# Make dataframe for plot
ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) %>%
  separate(col = Tide_time,
           into = c("Tide", "Time"),
           sep = "_")%>% #splits data everywhere there's an underscore
  rename("Nitrate + Nitrite (umol/L)" = "NN", #rename variables
         "Percent SGD" = "percent_sgd",
         "Total Alkylinity (umol/L)" = "TA",
         "Temperature (C)" = "Temp_in",
         "Phosphate (umol/L)" = "Phosphate",
         "Silicate (umol/L)" = "Silicate") %>%
  pivot_longer(cols = c("Temperature (C)", "Phosphate (umol/L)":"Percent SGD"), #pivot to long format
               names_to = "Variables", #one column for all the variable names
               values_to =  "Values") %>% #one column for the values
  mutate(Site = ifelse(Site == "BP", "Black Point", "Wailupe")) # rename sites

View(ChemData_clean)


# Make plot
ggplot(data = ChemData_clean, #use ChemData_clean
       aes(x = Salinity, #put salinity on x axis
           y = Values, #put values of each variable on y axis
           color = Site)) + #color by site
  geom_line() + #make it a line graph
  theme_bw() +
  theme(legend.position = c(0.63, 0.15), #move legend into empty space
        legend.background = element_blank(), 
        legend.box.background = element_rect(color = "black"), #put a border around legend
        plot.margin = margin(50, 50, 30, 30)) + #put margin around plot
  guides(color = guide_legend(nrow = 1)) + #make legend horizontal
  facet_wrap(~Variables, scales = "free") + #make one plot for each variable
  labs(y = " ") + #remove redundant y label
  ggsave(here("Week_4", "Output", "ChemData_plot.png"),
         width = 7, height = 7)




















