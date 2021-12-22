
#############################
# Read data for exploration #
#############################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1


#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
df_sync_day <- read_csv("data/interim/df_sync_day.csv")
df_sync_hour <- read_csv("data/interim/df_sync_hour.csv")
df_dist <- read_csv("data/interim/df_dist.csv")
deploy <- read_csv("data/interim/deploy.csv")
df_env_hour <- read_csv("data/interim/df_env_hour.csv")
df_env_day <- read_csv("data/interim/df_env_day.csv")


#### Format for exploration ####
df_sync_day <- df_sync_day %>% 
  mutate(
    datetime_date = parse_date_time(datetime_date, orders = "ymd"),
    transmitter = as.factor(transmitter),
    receiver_id = as.factor(receiver_id),
    station_rec = as.factor(station_rec),
    transmit_power_output = as.factor(transmit_power_output),
    dist_class = factor(dist_class, levels = c("0","0-200", "200-300","300-400", 
                                               "400-500", "500-600", "600-1000", "1000-2000")),
    bear_class = factor(bear_class, 
                        levels = c("(0,30]","(30,60]", "(60,90]","(90,120]", 
                                   "(120,150]", "(150,180]", "(-180,-150]", 
                                   "(-150,-120]", "(-120,-90]", "(-90,-60]", 
                                   "(-60,-30]","(-30,0]")))

df_sync_hour <- df_sync_hour %>% 
  mutate(
    datetime_hour = parse_date_time(datetime_hour, orders = "ymd HMS"),
    transmitter = as.factor(transmitter),
    receiver_id = as.factor(receiver_id),
    station_rec = as.factor(station_rec),
    transmit_power_output = as.factor(transmit_power_output),

    dist_class = factor(dist_class, levels = c("0","0-200", "200-300","300-400", 
                                               "400-500", "500-600", "600-1000", "1000-2000")),
    bear_class = factor(bear_class, 
                        levels = c("(0,30]","(30,60]", "(60,90]","(90,120]", 
                                   "(120,150]", "(150,180]", "(-180,-150]", 
                                   "(-150,-120]", "(-120,-90]", "(-90,-60]", 
                                   "(-60,-30]","(-30,0]")))


df_env_day <- df_env_day %>% 
  mutate(datetime_date = parse_date_time(datetime_date, orders = "ymd")) 
df_env_hour <- df_env_hour %>% 
  mutate(datetime_hour = parse_date_time(datetime_hour, orders = "ymd HMS"))

