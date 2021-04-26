### Iterative Coding


###Load Libraries
library(tidyverse)
library(here)


#Simple for loop
print(paste("The year is", 2000))

years <- c(2015:2021)

for(i in years){
  print(paste("The year is", i))
}

#save the loop in dataframe
year_data <- data.frame(matrix(ncol = 2, nrow = length(years)))

colnames(year_data) <-c("year", "year_name")

for(i in 1:length(years)){
  year_data$year_name[i] <- paste("The year is", years[i])
  year_data$year[i] <- years[i]
}


#use loops to read in multiple csv files
test_data <- read_csv(here("Week_13", "Data", "011521_CT316_1pcal.csv"))
glimpse(test_data)

CondPath <- here("Week_13", "Data")
files <- dir(path = CondPath, pattern = ".csv")
files

cond_data <- data.frame(matrix(nrow = length(files), ncol = 3))
colnames(cond_data) <- c("filename", "mean_temp", "mean_sal")
cond_data

raw_data <-read_csv(paste0(CondPath, "/", files[1])) #testby reading in first file

mean_temp <- mean(raw_data$Temperature, na.rm = TRUE) #calculate mean temp

for(i in 1:length(files)){
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  cond_data$filename[i] <- files[i]
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm = TRUE)
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm = TRUE)
}



### use [purrr] to do loops (instead of base R)

#calculate the mean from a random set of numbers 10 times
1:10 %>%
  map(rnorm, n = 15) %>% #rnorm takes random numbers from normal distribution
  map_dbl(mean)

#make function
1:10 %>%
  map(function(x) rnorm(15, x)) %>%
  map_dbl(mean)

#use formula
1:10 %>%
  map(~ rnorm(15, .x)) %>%
  map_dbl(mean)

#bring in files using purrr
CondPath <- here("Week_13", "Data")
files<-dir(path = CondPath, pattern = ".csv")
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE) #use full.names instead of paste to get file path

data <- files %>%
  set_names() %>% #set ID of each list to the file name
  map_df(read_csv, .id = "filename") %>%#function is read_csv, save ID as filename column
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_dal = mean(Salinity, na.rm = TRUE))


  



















