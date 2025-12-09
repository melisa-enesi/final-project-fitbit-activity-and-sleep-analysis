library(tidyverse)
library(lubridate)

# Load data
daily <- read_csv("data/dailyActivity_merged.csv")
sleep <- read_csv("data/sleepDay_merged.csv")
hourly_steps <- read_csv("data/hourlySteps_merged.csv")
hourly_cal <- read_csv("data/hourlyCalories_merged.csv")
weight <- read_csv("data/weightLogInfo_merged.csv")

# Clean dates
daily <- daily %>%
  mutate(ActivityDate = mdy(ActivityDate))

sleep <- sleep %>%
  mutate(SleepDay = mdy_hms(SleepDay),
         SleepDate = as_date(SleepDay))

hourly_steps <- hourly_steps %>%
  mutate(ActivityHour = mdy_hms(ActivityHour))

hourly_cal <- hourly_cal %>%
  mutate(ActivityHour = mdy_hms(ActivityHour))

weight <- weight %>%
  mutate(Date = mdy_hms(Date))

# Merge daily + sleep for Question 1
daily_sleep <- daily %>%
  inner_join(sleep, by = c("Id" = "Id", "ActivityDate" = "SleepDate"))

