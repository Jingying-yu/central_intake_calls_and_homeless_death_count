#### Preamble ####
# Purpose: Cleans the raw datasets obtained from OpenDataToronto to keep variables of interest for further analysis
# Author: Sandy Yu
# Date: 22 January, 2024
# Contact: jingying.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: downloaded and save required data and understand what aspects of the datasets are valueable


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)

#### Clean data ####

# first clean the raw_central_intake_call dataset
raw_central_intake_call <- read_csv("inputs/data/raw_central_intake_call.csv")

# we want to only observe data for date that are contained in both 
# the central_intake_call dataset and the homeless_death_count dataset
# this would be November 3rd 2020 to June 30th 2023

cleaned_central_intake_call_per_day <-
  raw_central_intake_call |> 
  # rename desired columns with appropriate names
  rename(Total_Calls_Coded = `Total calls coded`, 
         Referral_to_Shelter = `Code 1A - Referral to a Sleeping/Resting Space`, 
         Information_Homelessness_and_Prevention = 
           `Code 2C - Information - Homelessness & Prevention Services`) |> 
  # keep only the desired columns for easier computation
  select(Date, Total_Calls_Coded, Referral_to_Shelter, 
         Information_Homelessness_and_Prevention) |> 
  filter(Date <= "2023-06-30") 

# reorganize and use group_by to sum up values by month 
# for easier visualization
cleaned_central_intake_call <-
  cleaned_central_intake_call_per_day |>
  group_by(Month = lubridate::ceiling_date(Date, "month") - 1) |> 
  summarise(Calls_Coded_month = sum(Total_Calls_Coded), 
            Referral_to_Shelter_month = sum(Referral_to_Shelter), 
            Information_Homelessness_and_Prevention_month = 
              sum(Information_Homelessness_and_Prevention)) |>
  mutate(Month = as.Date(Month))





# Now we want to clean the raw_homeless_death_counts dataset
raw_homeless_death_counts <- read_csv("inputs/data/raw_homeless_death_counts.csv")

Date_day = seq(as.Date("2020-11-01"), as.Date("2023-06-30"), by = 1)

# create a tibble for proper date class
date_homeless_death_counts <- 
  tibble(
    # we want just the last day of the month here because death counts are
    #only definitively summed up on the last day of the month
    "Date_month" = ceiling_date(Date_day, "month")
  ) |> distinct() |> mutate(Date_month = Date_month - 1)

cleaned_homeless_death_counts <- raw_homeless_death_counts|> 
  #cleaning up the names of columns to exclude spaces
  rename(Year = `Year of death`, Month = `Month of death`) |> 
  # filter out rows that has Month == unknown
  # we will lose some data here, but dates otherwise cannot correspond
  # to other datasets in the paper
  filter(!(Month %in% c("Unknown")))

# we want to keep only the value from Nov 2020 to June 2023
cleaned_homeless_death_counts <- 
  cbind(date_homeless_death_counts, cleaned_homeless_death_counts[-(1:46),]) |>
  select(Date_month, Count)

# we now want to combine data from both datasets
combined_data <- 
  cbind(cleaned_central_intake_call, cleaned_homeless_death_counts) |>
  select(Month, Calls_Coded_month, Referral_to_Shelter_month, Information_Homelessness_and_Prevention_month, Count) |>
  rename(Death_Count = Count)



#### Save data ####
# save as cleaned_central_intake_call_per_day
write_csv(cleaned_central_intake_call_per_day, 
          "outputs/data/cleaned_central_intake_call_per_day.csv")
# save as cleaned_central_intake_call
write_csv(cleaned_central_intake_call, 
          "outputs/data/cleaned_central_intake_call.csv")

# save as cleaned_homeless_death_counts
write_csv(cleaned_homeless_death_counts, 
          "outputs/data/cleaned_homeless_death_counts.csv")

# save as combined_data
write_csv(combined_data, 
          "outputs/data/combined_data.csv")
