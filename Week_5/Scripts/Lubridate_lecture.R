### Learning how to deal with dates and times using {lubridate}#################
### Created by Emily Rukstales ################
### Created on: 2021-02-24 ###############


### Load Libraries ########
library(tidyverse)
library(here)
library(lubridate)
library(devtools)
library(CatterPlots)

##############
now()
now(tzone = "EST")
now(tzonr = "GMT")
today()
today(tzone = "GMT")

#Convert into ISO
mdy("February 24 2021") #use mutate and column name to change entire column
dmy("24/02/2021")

ymd_hms("2021-02-24 10:22:20 PM") #changes to ISO and military time

datetimes <- c("02/24/2021 22:22:20",
               "02/25/2021 11:21:!0",
               "02/26/2021 8:01:52")
datetimes <- mdy_hms(datetimes)
month(datetimes) #extract just months
month(datetimes, label = TRUE) #gives abbreviated names instead of numbers
month(datetimes, label = TRUE, abbr = FALSE) #gives full month names
day(datetimes) #extract days
wday(datetimes, label = TRUE) #Extracts weekdays
hour(datetimes)
minute(datetimes)
second(datetimes)

datetimes + hours(4) #adds four hours to times
datetimes + days(2)

round_date(datetimes, "minute") #rounds times to nearest minute
round_date(datetimes, "5 mins") #rounds to nearest 5 mins


CondData <- read_csv(here("Week_5", "Data", "CondData.csv")) %>%
  mutate(datetime = ymd_hms(date))
view(CondData)










