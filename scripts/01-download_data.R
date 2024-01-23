#### Preamble ####
# Purpose: Downloads and saves the data from OpenDataToronto portal
# Author: Sandy Yu
# Date: 22 January, 2024
# Contact: jingying.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: know where to obtain datasets for this paper



#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
# [...UPDATE THIS...]

#### Download data ####
# Data of Central Intake Calls received that were assigned a wrap-up code 
# for tracing and follow-up purposes
raw_central_intake_call_wrap_up <-
  read_csv(
    file = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/central-intake-calls/resource/4858b2dc-9b4e-425c-aaeb-ef23d819cbc4/download/Central%20Intake%20Call%20Wrap-Up%20Codes%20Data.csv",
    show_col_types = FALSE
  )


# Data of Homeless Death Counts in Toronto (by month)
raw_homeless_death_counts <-
  read_csv(
    file = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/a7ae08f3-c512-4a88-bb3c-ab40eca50c5e/resource/dc4ec2fa-d655-46ca-af32-f216d26e9804/download/Homeless%20deaths%20by%20month.csv",
    show_col_types = FALSE
  )



#### Save data ####
# [...UPDATE THIS...]

# save our acquired central intake call data
write.csv(
  x = raw_central_intake_call_wrap_up, 
  file = "inputs/data/raw_central_intake_call.csv"
)

# save our acquired Toronto homeless death count data
write.csv(
  x = raw_homeless_death_counts,
  file = "inputs/data/raw_homeless_death_counts.csv"
)
         
