#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)

#### Clean data ####

# first clean the raw_central_intake_call dataset
raw_central_intake_call <- read_csv("inputs/data/raw_central_intake_call.csv")

cleaned_central_intake_call <-
  raw_central_intake_call |> 
  # rename desired columns with appropriate names
  rename(Total_Calls_Coded = `Total calls coded`, 
         Referral_to_Shelter = `Code 1A - Referral to a Sleeping/Resting Space`, 
         Information_Homelessness_and_Prevention = 
           `Code 2C - Information - Homelessness & Prevention Services`) |> 
  # keep only the desired columns for easier computation
  select(Date, Total_Calls_Coded, Referral_to_Shelter, 
         Information_Homelessness_and_Prevention)


# then clean the raw_homeless_death_counts dataset
raw_homeless_death_counts <- read_csv("inputs/data/raw_homeless_death_counts.csv")


last_day_of_month = c(31,28,31,30,31,30,31,31,30,31,30,31)

cleaned_homeless_death_counts <- raw_homeless_death_counts |> 
  #cleaning up the names of columns to exclude spaces
  rename(Year = `Year of death`, Month = `Month of death`) |> 
  # filter out rows that has Month == unknown
  # we will lose some data here, but dates otherwise cannot correspond
  # to other datasets in the paper
  filter(!(Month %in% c("Unknown"))) |> 
  mutate(Month = c(rep(x = 1:12, times = 6), c(1:6)), 
         # Since date types need a day, I will use the last day of the month
         # this is because death count of the entire month only ends when 
         # the last day of the month has past
         Day = c(rep(x = last_day_of_month, times = 6), c(31,28,31,30,31,30))) |> 
  # Febuary of 2020 is a special month, there was 29 days in Feb of 2020
  mutate(Day = ifelse(row_number() == 38, 29, Day)) |> 
  # combining the year, month, and day together to form a date class
  mutate(Date = make_date(Year, Month, Day)) |>
  # mutate a new column that will serve as the name displayed in graphs later
  mutate(Date_Displayed = zoo:: as.yearmon(Date))


# select only the data we want to use in analysis
cleaned_homeless_death_counts <- cleaned_homeless_death_counts |> 
  select(Date_Displayed, Date, Count)
cleaned_homeless_death_counts

#### Save data ####
# save as cleaned_central_intake_call
write_csv(cleaned_central_intake_call, 
          "outputs/data/cleaned_central_intake_call.csv")

# save as cleaned_homeless_death_counts
write_csv(cleaned_homeless_death_counts, 
          "outputs/data/cleaned_homeless_death_counts.csv")

