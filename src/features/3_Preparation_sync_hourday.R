
#####################################
# Link transmissions to detections  #
#####################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
ev <- read_csv("data/interim/ev.csv")
deploy <- read_csv("data/interim/deploy.csv")
df_sync <- read_csv("data/interim/df_sync.csv")
df_env_hour <- read_csv("data/interim/df_env_hour.csv")
df_env_day <- read_csv("data/interim/df_env_day.csv")

#### Standardization formula ####
MyStd <- function(x) {  (x - mean(x))  /  sd(x) }

#### Summarise sync data per day ####
df_sync_day <- df_sync %>% 
  group_by(receiver_id, transmitter, transmit_power_output, datetime_date, 
           distance, dist_class, bear) %>% 
  summarise(n_det = sum(detection)) %>% 
  as.data.frame()

df_sync_day <- df_sync_day %>% 
  # Set variable maxi as the number of transmissions
  filter(dist_class == "0") %>% # filter for sync tag transmission registrations
  mutate(maxi = n_det) %>% # calculate number of transmission within the day
  select(datetime_date, transmitter, maxi) %>% 
  left_join(df_sync_day) %>% # rejoin with original dataframe
  # Calculate detection out of transmission percentage
  mutate(perc_det = n_det/maxi) %>% 
  # Set detection bins
  mutate(det_bin = ifelse(perc_det == 0, 0, 1))

#### Summarise sync data per hour ####
df_sync_hour <- df_sync %>% 
  group_by(receiver_id, transmitter, transmit_power_output, datetime_hour, 
           distance, dist_class, bear) %>% 
  summarise(n_det = sum(detection)) %>% 
  as.data.frame()

df_sync_hour <- df_sync_hour %>% 
  # Set variable maxi as the number of transmissions
  filter(dist_class == "0") %>% # filter for sync tag transmission registrations
  mutate(maxi = n_det) %>% # calculate number of transmission within the hour
  select(datetime_hour, transmitter, maxi) %>%
  left_join(df_sync_hour) %>% # rejoin with original dataframe
  # Calculate detection out of transmission percentage
  mutate(perc_det = n_det/maxi) %>% 
  # Set detection bins
  mutate(det_bin = ifelse(perc_det == 0, 0, 1))

#### Add variables ####
df_sync_list <- lapply(unique(df_sync$receiver_id), function(receiver_id_i){
  ev_temp <- ev %>% 
    filter(receiver_id == receiver_id_i) %>% 
    arrange(datetime)
  df_sync_temp <- df_sync %>% 
    filter(receiver_id == receiver_id_i) %>% 
    filter(distance == 0) %>% 
    group_by(receiver_id, datetime_hour) %>% 
    summarise() %>% 
    as.data.frame() %>% 
    arrange(datetime_hour)
  # noise
  ts_noise_receiver <- approx(ev_temp$datetime, ev_temp$noise, xout = df_sync_temp$datetime_hour, rule = 2, method = "linear", ties = mean)
  df_sync_temp$ts_noise_rec <- ts_noise_receiver$y
  # tilt
  ts_tilt <- approx(ev_temp$datetime, ev_temp$tilt, xout = df_sync_temp$datetime_hour, rule = 2, method = "linear", ties = mean)
  df_sync_temp$ts_tilt <- ts_tilt$y
  # temperature
  ts_temp <- approx(ev_temp$datetime, ev_temp$temperature, xout = df_sync_temp$datetime_hour, rule = 2, method = "linear", ties = mean)
  df_sync_temp$ts_temp <- ts_temp$y
  # pings
  ts_pings <- approx(ev_temp$datetime, ev_temp$pings, xout = df_sync_temp$datetime_hour, rule = 2, method = "linear", ties = mean)
  df_sync_temp$ts_pings <- ts_pings$y
  return(df_sync_temp)
})
df_noise_rec <- plyr::ldply(df_sync_list) 

# Add noise and tilt to hour data
df_sync_hour <- df_sync_hour %>% 
  left_join(df_noise_rec)

# Add noise and tilt to day data
# Group and summarise per day
df_noise_rec_day <- df_noise_rec %>% 
  mutate(datetime_date = parse_date_time(date(datetime_hour), orders = "ymd")) %>% 
  group_by(receiver_id, datetime_date) %>% 
  summarise(
    ts_noise_rec_mean = mean(ts_noise_rec, na.rm = T),
    ts_noise_rec_median = median(ts_noise_rec, na.rm = T),
    ts_noise_rec_sd = sd(ts_noise_rec, na.rm = T),
    ts_tilt_median = median(ts_tilt, na.rm = T),
    ts_temp_median = median(ts_temp, na.rm = T),
    ts_pings_median = median(ts_pings, na.rm = T)) %>% 
  as.data.frame()

# Join data
df_sync_day <- df_sync_day %>% 
  left_join(df_noise_rec_day)

#### Add station name ####
# Receiver
df_sync_hour <- deploy %>% 
  # Add deploy date of receiver
  mutate(deploy_date = parse_date_time(deploy_date_time, orders = "ymd HMS")) %>% 
  mutate(deploy_date = parse_date_time(date(deploy_date), orders = "ymd")) %>% 
  #Add station name
  select(receiver_id, 
         station_rec = station_name,
         deploy_date) %>% 
  left_join(df_sync_hour)

df_sync_day <- deploy %>% 
  # Add deploy date of receiver
  mutate(deploy_date = parse_date_time(deploy_date_time, orders = "ymd HMS")) %>% 
  mutate(deploy_date = parse_date_time(date(deploy_date), orders = "ymd")) %>% 
  #Add station name
  select(receiver_id, 
         station_rec = station_name,
         deploy_date) %>% 
  left_join(df_sync_day)

# Transmitter
df_sync_hour <- deploy %>% 
  select(transmitter = built_in_tag_id, 
         station_trans = station_name) %>% 
  left_join(df_sync_hour)

df_sync_day <- deploy %>% 
  select(transmitter = built_in_tag_id, 
         station_trans = station_name) %>% 
  left_join(df_sync_day)

#### Add days since deployment ####
df_sync_hour <- df_sync_hour %>% 
  mutate(datetime_date = parse_date_time(date(datetime_hour), orders = "ymd")) %>% 
  mutate(days_since_deploy = as.numeric(difftime(datetime_date, deploy_date))) %>% 
  select(-deploy_date, - datetime_date)

df_sync_day <- df_sync_day %>% 
  mutate(days_since_deploy = as.numeric(difftime(datetime_date, deploy_date))) %>% 
  select(-deploy_date)

#### Format sync data: classes ####
# Day
df_sync_day <- df_sync_day %>% 
  mutate(
    # Fix NA values
    bear = ifelse(distance == 0, NA, bear),
    # Make classes for distance and bearing
    dist_class = factor(dist_class, levels = c("0","0-200", "200-300","300-400", 
                                               "400-500", "500-600", "600-1000", "1000-2000")),
    bear_class = cut(bear, breaks = seq(-180, 180, 30))) %>% 
  mutate(bear_class = factor(bear_class, 
                             levels = c("(0,30]","(30,60]", "(60,90]","(90,120]", 
                                        "(120,150]", "(150,180]", "(-180,-150]", 
                                        "(-150,-120]", "(-120,-90]", "(-90,-60]", 
                                        "(-60,-30]","(-30,0]")))

# Hour
df_sync_hour <- df_sync_hour %>% 
  mutate(
    # Fix NA values
    bear = ifelse(distance == 0, NA, bear),
    # Make classes for distance and bearing
    dist_class = factor(dist_class, levels = c("0","0-200", "200-300","300-400", 
                                               "400-500", "500-600", "600-1000", "1000-2000")),
    bear_class = cut(bear, breaks = seq(-180, 180, 30))) %>% 
  mutate(bear_class = factor(bear_class, 
                             levels = c("(0,30]","(30,60]", "(60,90]","(90,120]", 
                                        "(120,150]", "(150,180]", "(-180,-150]", 
                                        "(-150,-120]", "(-120,-90]", "(-90,-60]", 
                                        "(-60,-30]","(-30,0]")))

#### Join detection data and environmental data #### 
# Day
df_sync_day <- df_env_day %>% 
  select(
    datetime_date,
    wind_speed_median, cur_speed_bot_median,
    wind_speed_median.st, cur_speed_bot_median.st,
    wind_dir_median, cur_dir_bot_median) %>% 
  right_join(df_sync_day)

# Hour
df_sync_hour <- df_env_hour %>% 
  select(
    datetime_hour,
    wind_speed_median, cur_speed_bot,
    wind_speed_median.st, cur_speed_bot.st,
    wind_dir_median, cur_dir_bot) %>% 
  right_join(df_sync_hour)

#### Standardize detection related data #### 
# Day
df_sync_day <- df_sync_day %>% 
  mutate(ts_noise_rec_mean.st = MyStd(ts_noise_rec_mean),
         ts_noise_rec_median.st= MyStd(ts_noise_rec_median),
         ts_tilt_median.st = MyStd(ts_tilt_median),
         ts_temp_median.st = MyStd(ts_temp_median),
         ts_pings_median.st = MyStd(ts_pings_median),
         days_since_deploy.st = MyStd(days_since_deploy))

# Hour
df_sync_hour <- df_sync_hour %>% 
  mutate(ts_noise_rec.st = MyStd(ts_noise_rec),
         ts_tilt.st = MyStd(ts_tilt),
         ts_temp.st = MyStd(ts_temp),
         ts_pings.st = MyStd(ts_pings),
         days_since_deploy.st = MyStd(days_since_deploy))

#### Add azimuth #### 
# Day
df_sync_day <- df_sync_day %>% 
  # Calculate azimuth
  mutate(
    wind_azimuth = abs(wind_dir_median - bear),
    cur_azimuth = abs(cur_dir_bot_median - bear)) %>% 
  # Set to 0 - 180 range
  mutate(
    wind_azimuth = ifelse(wind_azimuth > 180, 180 - (wind_azimuth - 180), wind_azimuth),
    cur_azimuth = ifelse(cur_azimuth > 180, 180 - (cur_azimuth - 180), cur_azimuth)) %>% 
  # Standardize
  mutate(
    wind_azimuth.st = ifelse(is.na(wind_azimuth), NA, MyStd(na.omit(wind_azimuth))),
    cur_azimuth.st = ifelse(is.na(cur_azimuth), NA, MyStd(na.omit(cur_azimuth)))
  )

# Hour
df_sync_hour <- df_sync_hour %>% 
  # Calculate azimuth
  mutate(
    wind_azimuth = abs(wind_dir_median - bear),
    cur_azimuth = abs(cur_dir_bot - bear)) %>% 
  # Set to 0 - 180 range
  mutate(
    wind_azimuth = ifelse(wind_azimuth > 180, 180 - (wind_azimuth - 180), wind_azimuth),
    cur_azimuth = ifelse(cur_azimuth > 180, 180 - (cur_azimuth - 180), cur_azimuth)) %>% 
  # Standardize
  mutate(
    wind_azimuth.st = ifelse(is.na(wind_azimuth), NA, MyStd(na.omit(wind_azimuth))),
    cur_azimuth.st = ifelse(is.na(cur_azimuth), NA, MyStd(na.omit(cur_azimuth)))
  )

####  Add turbine /sand #### 
df_sync_day <- df_sync_day %>% 
  mutate(habitat = ifelse(station_rec %in% c("JJ_DE89", "JJ_CD78", "JJ_BC89"),
                          "Sand", "Turbine"))
df_sync_hour <- df_sync_hour %>% 
  mutate(habitat = ifelse(station_rec %in% c("JJ_DE89", "JJ_CD78", "JJ_BC89"),
                          "Sand", "Turbine"))

#### Create distance matrix ####
# Calculate distance and bearings combinations
df_dist <- expand.grid(receiver_id = deploy$receiver_id,
                       transmitter = deploy$built_in_tag_id)
df_dist <- df_dist %>%  # join with receiver and tag positions
  left_join(select(deploy, # receiver positions
                   receiver_id, deploy_latitude, deploy_longitude)) %>% 
  left_join(select(deploy, # transmitter positions
                   transmitter = built_in_tag_id, 
                   tag_latitude = deploy_latitude, 
                   tag_longitude = deploy_longitude))

df_dist <- df_dist %>% 
  mutate(bear = geosphere::bearing( # calculate bearing
    matrix(c(deploy_longitude, deploy_latitude), ncol = 2),
    matrix(c(tag_longitude, tag_latitude), ncol = 2))) %>% 
  mutate(distance = geosphere::distGeo( # calculate distance
    matrix(c(deploy_longitude, deploy_latitude), ncol = 2),
    matrix(c(tag_longitude, tag_latitude), ncol = 2))) %>% 
  select(receiver_id, transmitter, distance, bear)

# Add dist and bear class
df_dist <- df_dist %>% 
  mutate(dist_class = cut(
    distance,
    breaks = c(-.1, 1, 200, 300, 400, 500, 600, 1000, 2000),
    labels = c("0", "0-200", "200-300", "300-400", "400-500", "500-600", "600-1000", "1000-2000")),
    bear_class = cut(bear, breaks = seq(-180, 180, 30))) %>% 
  mutate(bear_class = factor(bear_class, 
                             levels = c("(0,30]","(30,60]", "(60,90]","(90,120]", 
                                        "(120,150]", "(150,180]", "(-180,-150]", 
                                        "(-150,-120]", "(-120,-90]", "(-90,-60]", 
                                        "(-60,-30]","(-30,0]")),
         dist_class = factor(dist_class, levels = c("0","0-200", "200-300","300-400", 
                                                    "400-500", "500-600", "600-1000", "1000-2000"))) 



#### Standardize distance ####
# Use matrix standardized values (date removed in df_sync, so not exactly 27*27*x)
df_dist <- df_dist %>% 
  mutate(distance.st = MyStd(distance))

df_sync_day <- df_dist %>% 
  select(receiver_id, transmitter, distance.st) %>% 
  right_join(df_sync_day)

df_sync_hour <- df_dist %>% 
  select(receiver_id, transmitter, distance.st) %>% 
  right_join(df_sync_hour)


#### Write data ####
write_csv(df_sync_day, "data/interim/df_sync_day.csv")
write_csv(df_sync_hour, "data/interim/df_sync_hour.csv")
write_csv(df_dist, "data/interim/df_dist.csv")

