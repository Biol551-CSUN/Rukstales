### The purpose of this script is to practice using tidyr functions ############
### Created by Emily Rukstales #################################################
### Created on 2021-02-17 ######################################################
################################################################################

### Load Libraries #############################################################
library(tidyverse)
library(here)


###Load Data ###################################################################
ChemData <- read_csv(here("Week_4", "Data", "chemicaldata_maunalua.csv"))
view(ChemData)
glimpse(ChemData)


### Data analysis ##############################################################
ChemData_clean <- ChemData %>% #create new dtaframe
  filter(complete.cases(.)) %>%
  separate(col = Tide_time,
           into = c("Tide", "Time"),
           sep = "_")%>% #splits data everywhere there's an underscore
  unite(col = "Site_Zone", #create a new column called Site_Zone
        c(Site, Zone), #unite these columns
        sep = ".", #separate values with a period
        remove = FALSE) %>% #keep OG column
  pivot_longer(cols = Temp_in:percent_sgd, #pivot columns into long columns
               names_to = "Variables", #one columns for all the variable names
               values_to =  "Values") #one column for the values
view(ChemData_clean)

# Summarize all variables using long format dataframe
ChemData_clean %>%
  group_by(Variables, Site) %>%
  summarise(Param_means = mean(Values, na.rm =TRUE),
            Param_vars = var(Values, na.rm = TRUE))
  
ChemData_clean %>%
  group_by(Variables, Site, Zone, Tide) %>%
  summarise(Param_means = mean(Values, na.rm =TRUE),
            Param_vars = var(Values, na.rm = TRUE),
            Param_SD = sd(Values, na.rm = TRUE))

# Use facet_wrap
ChemData_clean %>%
  ggplot(aes(x = Site, y = Values)) +
         geom_boxplot() +
         facet_wrap(~Variables, scales = "free") # use uniques scale for each variable plot
  
# Convert data back to wide format
ChemData_wide <- ChemData_clean%>%
  pivot_wider(names_from = Variables,
              values_from = Values)

# Start over from beginning and get dataframe with just summary stats
ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) %>%
  separate(col = Tide_time,
           into = c("Tide", "Time"),
           sep = "_",
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") %>%
  group_by(Variables, Site, Time) %>%
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Variables,
              values_from = mean_vals) %>%
  write_csv(here("Week_4", "Output", "Summary.csv")) #export summary stats to .csv file
view(ChemData_clean)  















