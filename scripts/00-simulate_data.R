#### Preamble ####
# Purpose: Simulates data to predict issues that we may encounter with the real datasets
# Author: Sandy Yu
# Date: January 22, 2024
# Contact: jingying.yu@mail.utoronto.ca 
# License: MIT
# Pre-requisites: look at sketches file or have a basic understanding of what the data is used for


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)


#### Simulate data ####
# set seed so we can have the same simulation in each run
set.seed(21)

# first simulate the central intake calls dataset. 
# This is a dataset spanning across 3 years (each row is 1 day)
# thus 3*365 = 1095 rows
simulated_central_intake_call <-
  tibble(
    "Date" = seq(as.Date("2020-11-01"), as.Date("2023-10-31"), by = 1),
    # first simulate number of total calls coded
    # size = 3*365 because we are simulating 3 years worth of data
    "Total_Calls_Coded" = sample(x = 100:1000, size = 3*365, replace = TRUE),
    "Referred_to_Shelter" = sample(0:150, size = 3*365, replace = TRUE),
    "Info_Homelessness_and_Prevention" = sample(0:150, 3*365, replace = TRUE)
  )

# reorganize and use group_by to sum up values by month
simulated_month_central_intake_call <- simulated_central_intake_call |>
  group_by(Month = lubridate::ceiling_date(Date, "month") - 1) |> 
  summarise(Calls_Coded_month = sum(Total_Calls_Coded), 
            Referred_to_Shelter_month = sum(Referred_to_Shelter), 
            Info_Homelessness_and_Prevention_month = 
              sum(Info_Homelessness_and_Prevention))




# next we would want to simulate homeless death count dataset
# This is a dataset that records death counts of homeless people, recorded monthly
# This dataset also span across 3 year, thus has 3*12 = 36 rows

Date_day = seq(as.Date("2020-11-01"), as.Date("2023-10-31"), by = 1)

simulated_date_homeless_death_counts <- 
  tibble(
    # we want just the last day of the month here because death counts are
    #only definitively summed up on the last day of the month
    "Date_month" = ceiling_date(Date_day, "month")
  ) |> distinct() |> mutate(Date_month = Date_month - 1)

simulated_homeless_death_counts <- simulated_date_homeless_death_counts |>
  mutate(Count = sample(x = 0:100, size = 3*12))




# now I want to graph the data simulate
# I will draw data from the two tables onto the same graph using ggplot()

# 1st graph contain line graph of central_intake_call$total_calls_coded overtime
# overlaying with bar graph of homeless death counts over time
ggplot(simulated_homeless_death_counts, aes(x = Date_month, y = Count)) + 
  geom_bar(stat = "identity") + 
  geom_line(data = simulated_month_central_intake_call, aes(x = Month, 
    # since I only want to observe and analyse correlation between 
    # homeless death counts and calls coded
    # I lowered the line containing calls coded trend towards the 
    # bar plot by dividing the Calls_Coded_month value by 200
    y = Calls_Coded_month/200))
                                                                                                                  aes(x = Month, 
                                                                                                                      # since I only want to observe and analyse correlation between 
                                                                                                                      # homeless death counts and calls coded
                                                                                                                      # I lowered the line containing calls coded trend towards the 
                                                                                                            y = Calls_Coded_month/200))


