#######################
# Format deploy data  #
#######################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
# Metadata
deploy <- read.csv("data/raw/deploy_read20210127.csv", stringsAsFactors = F)
rec <- read.csv("data/raw/rec_read20210127.csv", stringsAsFactors = F)

# Event data (to get depths)
ev <- read.csv("data/interim/ev.csv", stringsAsFactors = F)

#### Format rec and deploy data ####
rec <- rec %>% 
  filter(manufacturer == "VEMCO") %>% # remove Thelma receiver for det efficiency (for now)
  select(receiver_id, built_in_tag_id) 

deploy <- deploy %>% 
  filter(!is.na(recover_date_time) & # refine to finished deployments
           receiver_id %in% rec$receiver_id) %>% # refine to receivers in rec
  select(receiver_id,
         station_name,
         deploy_date_time,
         recover_date_time,
         deploy_latitude,
         deploy_longitude,
         transmit_power_output) %>% 
  separate(station_name, sep = "_", into = c(NA, "turbine", NA), remove = F) %>% # define turbine
  left_join(rec)

rec <- rec %>% 
  filter(receiver_id %in% deploy$receiver_id) # do not consider receivers from open deployments

#### Add depth data to deploy ####
deploy <- ev %>% 
  group_by(receiver_id) %>% 
  summarise(depth_median = median(depth)) %>% 
  left_join(deploy) %>% 
  filter(!is.na(station_name))
  
#### Save metadata ####
write.csv(deploy, "data/interim/deploy.csv", row.names = F)
write.csv(rec, "data/interim/rec.csv", row.names = F)
