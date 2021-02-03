###  This is my first script. I am leaning how to import data
###  Created by: Emily Rukstales
### Created on: 2021-02-03
##############################################################


### Load Libraries #######################
library(tidyverse)
library(here)


### Read in Data #########################
WeightData <- read.csv(here("Week_2", "Data", "weightdata.csv"))


### Data Analysis ########################
head(WeightData)
tail(WeightData)
View(WeightData)

