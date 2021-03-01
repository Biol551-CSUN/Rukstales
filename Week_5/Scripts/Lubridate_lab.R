### This script is for the Week 5b lab using lubridate ###############
### Created by Emily Rukstales #######################################
### Created on: 2021-02-24 ###########################################


### Load Libraries ####
library(tidyverse)
library(here)
library(lubridate)


### Summary data and plot ####
CondData <- read_csv(here("Week_5", "Data", "CondData.csv")) %>%
  mutate(datetime = ymd_hms(date)) %>%
  mutate(datetime = round_date(datetime, "10 seconds")) %>%
  select(-date)

DepthData <- read_csv(here("Week_5", "Data", "DepthData.csv")) %>%
  mutate(datetime = ymd_hms(date)) %>%
  select(-date)

CondDepthData <- inner_join(CondData, DepthData) %>%
  mutate(hour = hour(datetime)) %>%
  mutate(minute = minute(datetime)) %>%
  group_by(hour, minute) %>%
  summarise(Mean_date = mean(datetime),
            Mean_depth = mean(Depth),
            Mean_salinity = mean(SalinityInSitu_1pCal),
            Mean_temp = mean(TempInSitu)) %>%
  write_csv(here("Week_5", "Outputs", "CondDepthData_summary.csv"))


ggplot(data = CondDepthData,
       aes(x = Mean_date,
           y = Mean_depth,
           color = Mean_temp)) +
  geom_line() +
  scale_y_reverse() +
  scale_color_gradient(low = '#1e55eb',
                       high = '#eb1e96',
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              title.hjust = 0.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = margin(30, 30, 30, 30),
        legend.position = c(.67, .7)) +
  labs(x = "Time",
       y = "Mean Depth (m)",
       color = "Mean Salinity (psu)",
       title = "Mean depth and salinity over time",
       subtitle = "January 15, 2021") +
  ggsave(here("Week_5", "Outputs", "CondDepthData_plot.png"),
         width = 7, height = 5)


















