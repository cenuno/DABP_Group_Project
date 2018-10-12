#
# Author:   Cristian E. Nuno
# Purpose:  Store state name and abbreviations in a .csv file
# Date:     October 11, 2018
#

# load necessary packages ----
library(tidyverse)

# load necessary data ----
df <-
  tibble(name = state.name
         , abb = state.abb
         , region = state.region
         , division = state.division)

# export file ----
setwd("/Users/cristiannuno/Desktop/Advanced_Analytics/DABP_Group_Project/raw_data/")

write_csv(df, "states.csv")

# end of script #