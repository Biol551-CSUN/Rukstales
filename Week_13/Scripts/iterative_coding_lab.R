### Iterative coding lab
### Created by Emily Rukstales
### Created on: 2021-04-26


### Load Libraries
library(tidyverse)
library(here)

### Set path and make a vector with the files
TPPath <- here("Week_13", "Data", "homework")
files <- dir(path = TPPath, pattern = ".csv") #save files in a vector
files

### Create an empty dataframe
TP_data <- data.frame(matrix(nrow = length(files), ncol = 5)) #5 columns for file name and variables
colnames(TP_data) <- c("filename", "mean_temp", "mean_light", "SD_temp", "SD_light") #name columns

### Test functions
raw_data <-read_csv(paste0(TPPath, "/", files[1])) #test by reading in first file
mean_temp <- mean(raw_data$Temp.C, na.rm = TRUE) #calculate mean temp
SD_temp <- sd(raw_data$Temp.C, na.rm = TRUE) #calculate SD of temp

### Create for loop
for(i in 1:length(files)){
  raw_data <- read_csv(paste0(TPPath, "/", files[i])) #create vector with path to each file
  TP_data$filename[i] <- files[i] #make a column with names of files
  TP_data$mean_temp[i] <- mean(raw_data$Temp.C, na.rm = TRUE)
  TP_data$mean_light[i] <- mean(raw_data$Intensity.lux, na.rm = TRUE)
  TP_data$SD_temp[i] <- sd(raw_data$Temp.C, na.rm = TRUE)
  TP_data$SD_light[i] <- sd(raw_data$Intensity.lux, na.rm = TRUE)
}

### Use purrr
files_map <- dir(path = TPPath, pattern = ".csv", full.names = TRUE)

TP_data_map <- files_map %>%
  set_names() %>% #set ID of each list to the file name
  map_df(read_csv, .id = "filename") %>% #function is read_csv, save ID as filename column
  group_by(filename) %>%
  summarise(mean_temp = mean(Temp.C, na.rm = TRUE),
            mean_light = mean(Intensity.lux, na.rm = TRUE),
            SD_temp = sd(Temp.C, na.rm = TRUE),
            SD_light = sd(Intensity.lux, na.rm = TRUE))











