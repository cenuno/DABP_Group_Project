#
# Author:   Cristian E. Nuno
# Purpose:  Clean master file
# Date:     October 15, 2018
#

# load necessary packages -----
library(here)
library(tidyverse)

# load necessary data -----
df <- read_csv("https://raw.githubusercontent.com/cenuno/DABP_Group_Project/master/write_data/clean_crop_2011_2017.csv")

states <- read_csv("https://raw.githubusercontent.com/cenuno/DABP_Group_Project/master/raw_data/states.csv")

# check dim and colnames
glimpse(df) # 1152 rows by 17 columns

# check state spelling
df %>%
  count(state) %>%
  View("check state spelling")

# ah...it looks like folks did not fully spell out each state's name
df <-
  df %>%
  # there is no documentation for a territory abbreviated as NA
  # so removing it from the data set
  filter(is.na(state) == FALSE) %>%
  # re-naming values so that they are standardized
  mutate(state = case_when(
    state == "Concticut" ~ "Connecticut"
    , state == "Marianas" ~ "N. Mariana Is."
    , state == "Masacuset" ~ "Massachusetts"
    , state == "Misisippi" ~ "Mississippi"
    , state == "New Hamp" ~ "New Hampshire"
    , state == "Newjersey" ~ "New Jersey"
    , state == "New Mexco" ~ "New Mexico"
    , state == "No Caroln" ~ "North Carolina"
    , state == "No Dakota" ~ "North Dakota"
    , state == "Penslvana" ~ "Pennsylvania"
    , state %in% c("Pr", "Puerto Ri") ~ "Puerto Rico" 
    , state == "Rhode Isl" ~ "Rhode Island"
    , state == "S Carolin" ~ "South Carolina"
    , state == "S Dakota" ~ "South Dakota"
    , state %in% c("Vi", "Virgin Is", "Virgin Islands Of The U.S.") ~ "Virgin Islands"
    , state == "Washngton" ~ "Washington"
    , state == "West Va" ~ "West Virginia"
    , TRUE ~ as.character(state)
  )) %>%
  # there are only two years worth of data for N. Mariana Is.
  # so remove them from the data
  filter(!state == "N. Mariana Is.") %>%
  # drop the state columns 
  select(-abb, -region, -division) %>%
  # re join the state columns
  left_join(y = states, by = c("state" = "name")) %>%
  # make sure data is arranged properly
  arrange(type, year, state) %>%
  # to incorporate guam, puerto rico, and virgin islands
  # create standardized federal regions not captures in the states data frame
  # note: source is https://www.fema.gov/fema-regional-contacts
  mutate(standard_fed_regions = case_when(
    state %in% c("Connecticut", "Maine", "Massachusetts"
                 , "New Hampshire", "Rhode Island", "Vermont") ~ "region I"
    , state %in% c("New Jersey", "New York"
                   , "Puerto Rico", "Virgin Islands") ~ "region II"
    , state %in% c("Delaware", "District of Columbia", "Maryland"
                   , "Pennsylvania", "Virginia", "West Virginia") ~ "region III"
    , state %in% c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi"
                   , "North Carolina", "South Carolina", "Tennessee") ~ "region IV"
    , state %in% c("Illinois", "Indiana", "Michigan", "Minnesota"
                   , "Ohio", "Wisconsin") ~ "region V"
    , state %in% c("Arkansas", "Louisiana", "New Mexico"
                   , "Oklahoma", "Texas") ~ "region VI"
    , state %in% c("Iowa", "Kansas", "Missouri", "Nebraska") ~ "region VII"
    , state %in% c("Colorado", "Montana", "North Dakota"
                   , "South Dakota", "Utah", "Wyoming") ~ "region VIII"
    , state %in% c("Arizona", "California", "Hawaii"
                   , "Nevada", "American Samoa"
                   , "Guam", "N. Mariana Is.") ~ "region IX"
    , state %in% c("Alaska", "Idaho", "Oregon", "Washington") ~ "region X"
  )
  , standard_fed_regions = factor(standard_fed_regions
                                  , levels = paste("region"
                                                   , c("I", "II", "III"
                                                       , "IV", "V", "VI"
                                                       , "VII", "VIII", "IX"
                                                       , "X")))
  # make sure each state has an abbreviation
  , abb = case_when(
    state == "Guam" ~ "GU"
    , state == "Puerto Rico" ~ "PR"
    , state == "Virgin Islands" ~ "VI"
    , TRUE ~ as.character(abb)
  )) %>%
  # rearrange columns
  select(type, year, state, abb
         , standard_fed_regions, region, division
         , barley:wheat)

# check dim
dim(df) # [1] 1146   18

# export results ----
write_csv(df, here("write_data", "v2_clean_crop_2011_2017.csv"))
