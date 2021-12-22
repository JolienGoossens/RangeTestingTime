
##############################
# Format environmental data  #
##############################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(lubridate)

#### Standardization formula ####
MyStd <- function(x) {  (x - mean(x))  /  sd(x) }

#### Get data ####
# Wind
# Wind data organized in files per month: make a list of these files and combine in one dataframe
wind_file_list <- list.files("data/external/MDK_emodnet/Westhinder")
wind_df_list <- lapply(unique(wind_file_list), function(file_name){
  file <- readLines(paste0("data/external/MDK_emodnet/Westhinder/", file_name))
  df_wind <- read.table(text = file[52:length(file)], sep = ";", header = T, stringsAsFactors = F)
})
df_wind <- plyr::ldply(wind_df_list)

# Current
df_cur <- read.csv("data/external/RBIN/BCZ_HydroState_V1_ba81_f8f4_280c.csv", stringsAsFactors = F)

#### Formatting: wind ####

# Select relevant columns: only time, wind direction and wind speed
# depth is always - 10, latitude is always 51.389, longitude is always 2.438
df_wind <- df_wind %>% 
  select(datetime = TIME,
         wind_dir = WDIR,
         wind_speed = WSPD)

# Format NA values and variable types
df_wind <- df_wind %>% 
  filter(wind_dir != "--" | wind_speed != "--") %>% 
  # Values '--' to NA
  mutate(wind_dir = ifelse(wind_dir == "--", NA, wind_dir),
         wind_speed = ifelse(wind_speed == "--", NA, wind_speed)) %>% 
  # Variable types
  mutate(datetime = parse_date_time(gsub(":000", "", datetime), orders = "dmy HMS", tz = "UTC"),
         wind_dir = as.numeric(wind_dir),
         wind_speed = as.numeric(gsub(",", ".", wind_speed)))

# Calculate direction and vector components with trigonometry
df_wind <- df_wind  %>% 
  mutate(wind_dir_vec = wind_dir - 180) %>% 
  mutate(wind_east = wind_speed * sin((pi/180) * wind_dir_vec),
         wind_north = wind_speed * cos((pi/180) * wind_dir_vec))

#### Formatting: current ####
# Change variable names
colnames(df_cur) <- c("datetime", "latitude", "longitude", "velo_bot_east", "velo_surf_east", "velo_bot_north", "velo_surf_north", "height_above_sealevel", "sal_surf", "temp_surf")

df_cur <- df_cur %>% 
  # Format datetime
  mutate(datetime = parse_date_time(datetime, orders = "ymd HMS", tz = "UTC"))  %>% 
  # Filter for one location (very close to each other)
  filter(longitude > 2.79) 

#### Add time variables ####
# Wind
df_wind <- df_wind %>% 
  mutate(datetime_hour = datetime,
         datetime_date = parse_date_time(date(datetime), orders = "ymd"))
second(df_wind$datetime_hour) = 0 # Set seconds to 0
minute(df_wind$datetime_hour) = 0 # Set minutes to 0

# Current
df_cur <- df_cur %>% 
  mutate(datetime_hour = datetime,
         datetime_date = parse_date_time(date(datetime), orders = "ymd"))

#### Make hourly dataframe ####
# Wind

# Calculate speed and direction from both mean and median to decide for best metric: calculate euclidean distances
# df_wind %>%
#   group_by(datetime_hour) %>%
#   summarise(wind_speed_mean = mean(wind_speed, na.rm = T),
#             wind_speed_median = median(wind_speed, na.rm = T),
#             wind_speed_max = max(wind_speed, na.rm = T),
#             wind_east_mean = mean(wind_east, na.rm = T),
#             wind_east_median = median(wind_east, na.rm = T),
#             wind_north_mean = mean(wind_north, na.rm = T),
#             wind_north_median = median(wind_north, na.rm = T)) %>%
#   mutate(wind_dir_mean = (180/pi) * atan2(wind_east_mean, wind_north_mean),
#          wind_dir_median = (180/pi) * atan2(wind_east_median, wind_north_median)) %>% 
#   mutate(wind_speed_mean_euc = sqrt((wind_east_mean^2)+(wind_north_mean^2)),
#          wind_speed_median_euc = sqrt((wind_east_median^2)+(wind_north_median^2)),
#          diff_wind_speed_mean = wind_speed_mean - wind_speed_mean_euc,
#          diff_wind_speed_median = wind_speed_median - wind_speed_median_euc) %>%
#   summary()
# Results: median lowest euclidean difference: take median

# Wind: calculate median and max per hour
df_wind_hour <- df_wind %>% 
  group_by(datetime_hour) %>% 
  summarise(
    wind_speed_median = median(wind_speed, na.rm = T),
    wind_speed_max = max(wind_speed, na.rm = T),
    wind_east_median = median(wind_east, na.rm = T),
    wind_north_median = median(wind_north, na.rm = T)) %>% 
  mutate(
    wind_speed_max = ifelse(wind_speed_max == -Inf, NA, wind_speed_max),
    wind_dir_median = (180/pi) * atan2(wind_east_median, wind_north_median)) 

# Current
df_cur_hour <- df_cur %>% 
  select(datetime_hour, velo_bot_east, velo_surf_east, velo_bot_north, velo_surf_north) %>% 
  mutate(
    cur_speed_bot = sqrt((velo_bot_east^2)+(velo_bot_north^2)),
    cur_speed_surf = sqrt((velo_surf_east^2) + (velo_surf_north^2)),
    cur_dir_bot = (180/pi) * atan2(velo_bot_east, velo_bot_north),
    cur_dir_surf = (180/pi) * atan2(velo_surf_east, velo_surf_north))

# Join wind and current data
df_env_hour <- left_join(df_wind_hour, df_cur_hour)

#### Make daily dataframe ####
# Wind: calculate median and max per day
df_wind_day <- df_wind %>% 
  group_by(datetime_date) %>% 
  summarise(
    wind_speed_median = median(wind_speed, na.rm = T),
    wind_speed_max = max(wind_speed, na.rm = T),
    wind_east_median = median(wind_east, na.rm = T),
    wind_north_median = median(wind_north, na.rm = T)) %>% 
  mutate(
    wind_dir_median = (180/pi) * atan2(wind_east_median, wind_north_median)) 

# Current: calculate median and max per day
df_cur_day <- df_cur %>% 
  # Calculate speed and direction with trigonometry
  mutate(
    cur_speed_bot = sqrt((velo_bot_east^2)+(velo_bot_north^2)),
    cur_speed_surf = sqrt((velo_surf_east^2) + (velo_surf_north^2)),
    cur_dir_bot = (180/pi) * atan2(velo_bot_east, velo_bot_north),
    cur_dir_surf = (180/pi) * atan2(velo_surf_east, velo_surf_north)) %>% 
  # Calculate median and max per day
  group_by(datetime_date) %>% 
  summarise(
    cur_speed_bot_median = median(cur_speed_bot, na.rm = T),
    cur_speed_surf_median = median(cur_speed_surf, na.rm = T),
    cur_speed_bot_max = max(cur_speed_bot, na.rm = T),
    cur_speed_surf_max = max(cur_speed_surf, na.rm = T),
    velo_bot_east_median = median(velo_bot_east, na.rm = T),
    velo_surf_east_median = median(velo_surf_east, na.rm = T),
    velo_bot_north_median = median(velo_bot_north, na.rm = T),
    velo_surf_north_median = median(velo_surf_north, na.rm = T)) %>% 
  mutate(
    cur_dir_bot_median = (180/pi) * atan2(velo_bot_east_median, velo_bot_north_median),
    cur_dir_surf_median = (180/pi) * atan2(velo_surf_east_median, velo_surf_north_median))

# Join wind and current data
df_env_day <- left_join(df_wind_day, df_cur_day)

#### Standardize environmental data #### 
# Day 
df_env_day <- df_env_day %>% 
  mutate(
    wind_speed_median.st = MyStd(wind_speed_median),
    wind_speed_max.st = MyStd(wind_speed_max),
    cur_speed_bot_median.st = MyStd(cur_speed_bot_median),
    cur_speed_bot_max.st = MyStd(cur_speed_bot_max))

# Hour
df_env_hour <- df_env_hour %>% 
  mutate(wind_speed_max = ifelse(wind_speed_max == -Inf, NA, wind_speed_max)) %>% 
  mutate(
    wind_speed_median.st = ifelse(is.na(wind_speed_median), NA, MyStd(na.omit(wind_speed_median))),
    wind_speed_max.st = ifelse(is.na(wind_speed_max), NA, MyStd(na.omit(wind_speed_max))),
    cur_speed_bot.st = MyStd(cur_speed_bot))

#### Save ####
write.csv(df_env_hour, "data/interim/df_env_hour.csv", row.names = F)
write.csv(df_env_day, "data/interim/df_env_day.csv", row.names = F)

