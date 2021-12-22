
##########################
# Format detection data  #
##########################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
# Detection data
# - - - Detection data as extracted from VUE with time correction and with milliseconds
df <- read.csv("data/raw/Belwind_oct2020_detections_builtin.csv", stringsAsFactors = F)

#### Select and filter ####
df <- df %>% 
  select(
    datetime = Date.and.Time..UTC., 
    receiver_id = Receiver, 
    transmitter = Transmitter) %>% 
  mutate(
    date = date(parse_date_time(datetime, orders = "ymd HMS"))) %>% 
  # Remove detections before deployment date and after recovery date
  filter(ymd("2020-05-13") < date & date < ymd("2020-10-12")) %>% 
  select(-date)

#### Save ####
write.csv(df, "data/interim/df_sync_det.csv", row.names = F)





