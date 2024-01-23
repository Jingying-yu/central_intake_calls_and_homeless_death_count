#### Preamble ####
# Purpose: Tests cleaned data to ensure all classes, values, etc. of variables are kept/included
# Author: Sandy Yu
# Date: 22 January, 2024
# Contact: jingying.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: written code for data cleaning


#### Workspace setup ####
library(tidyverse)
library(lubridate)

#### Test data ####
# Tests for cleaned_central_intake_call
# test that no numeric values are negative for each numeric columns
any(cleaned_central_intake_call$Calls_Coded_month >= 0)
any(cleaned_central_intake_call$Referral_to_Shelter_month >= 0)
any(cleaned_central_intake_call$Information_Homelessness_and_Prevention_month >= 0)

# test that dates are unique
unique_dates <- cleaned_central_intake_call$Month |> unique()
start_date <- as.Date("2020-11-01")
# we do not input the end_date here as 2023-06-30 because we want to count ALL
# the months, not just the month between 2020-11-01 and 2023-06-30
end_date <- as.Date("2023-07-01")
length(unique_dates) == interval(start_date, end_date) %/% months(1)

# test that date is within the bounds of 2020-11-01 to 2023-06-30
start_date <- as.Date("2020-11-01")
end_date <- as.Date("2023-07-01")
start_date <= cleaned_central_intake_call$Month[1]
end_date >= cleaned_central_intake_call$Month[32]

# check that dates are chronological
chronological_dates <- list()

for (n in 1:(length(cleaned_central_intake_call$Month)-1)) {
  if (cleaned_central_intake_call$Month[[n]] < cleaned_central_intake_call$Month[[n+1]]) {
    chronological_dates[[n]] <- TRUE
  }
  else{
    chronological_dates[[n]] <- FALSE
  }
}
all(chronological_dates == "TRUE")


# Tests for cleaned_homeless_death_counts
# check that Date_Displayed has "chr" as conlumn type, Date_month has "date", and Count has "dbl"
class(cleaned_homeless_death_counts$Date_Displayed) == "character"
class(cleaned_homeless_death_counts$Date_month) == "Date"
class(cleaned_homeless_death_counts$Count) == "numeric"

# check that Count values are not negative (>=0)
any(cleaned_homeless_death_counts$Count >= 0)

# check that dates are chronological
chronological_dates <- list()

for (n in 1:(length(cleaned_homeless_death_counts$Date_month)-1)) {
  if (cleaned_homeless_death_counts$Date_month[[n]] < cleaned_homeless_death_counts$Date_month[[n+1]]) {
    chronological_dates[[n]] <- TRUE
  }
  else{
    chronological_dates[[n]] <- FALSE
  }
}
all(chronological_dates == "TRUE")


