
#####################################
# Link transmissions to detections  #
#####################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(lubridate)
library(zoo)
library(geosphere)

#### Get data ####
df_sync <- read.csv("data/interim/df_sync_det.csv", stringsAsFactors = F)

# Metadata
deploy <- read.csv("data/interim/deploy.csv", stringsAsFactors = F)
rec <- read.csv("data/interim/rec.csv", stringsAsFactors = F)

#### Filter for sync tags ####
df_sync <- df_sync %>% 
  mutate(datetime = parse_date_time(datetime, orders = "ymd HMS", tz = "UTC"))

#### Identify per signal whether it was detected ####
# For every transmitted signal, see if it was detected by any receiver ()
df_sync_list <- lapply(rec$built_in_tag_id, function(built_in_tag){
  print(paste0("Start: ", built_in_tag))
  # Define receiver to which the built-in transmitter belongs
  receiver_built_in = rec %>% 
    filter(built_in_tag_id == built_in_tag) %>% 
    distinct(receiver_id) %>% 
    pull()
  # Make df of transmitted signals(= registered on receiver)
  df_temp_transmitted = df_sync %>% 
    filter(receiver_id == receiver_built_in & transmitter == built_in_tag) %>% 
    arrange(datetime) %>% 
    mutate(X = c(1:n()),
           datetime_reg = datetime,
           det_type = "Registration")
  
  # Make df of detected signals
  df_temp_detected = df_sync %>% 
    filter(receiver_id != receiver_built_in & transmitter == built_in_tag) %>% 
    mutate(X = NA, # for now, X is NA for the detected signals
           datetime_reg = NA, # for now, Time_minute is NA for the detected signals
           det_type = "Detection")
  
  # rbind those temporary dataframes
  df_temp <- rbind(df_temp_transmitted, df_temp_detected)
  
  # Connect transmitted signals to registered signals
  df_temp_list <- lapply(unique(na.omit(df_temp$X)), function(X_i){
    df_temp_sel = df_temp %>% filter(X == X_i)
    df_temp_X <- df_temp %>% filter(
      df_temp_sel$datetime - lubridate::seconds(100) <= df_temp$datetime & 
        df_temp$datetime < df_temp_sel$datetime + lubridate::seconds(100))
    df_temp_X <- df_temp_X %>% 
      mutate(X = df_temp_sel$X,
             datetime_reg = df_temp_sel$datetime_reg)
    return(df_temp_X)
  })
  
  df_temp <- plyr::ldply(df_temp_list) # make a dataframe of the list of all X
  
  # Make a template of all possible combinations  of X and TransmitterBuiltin
  df_template <- expand.grid(X = unique(na.omit(df_temp$X)),
                             receiver_id = unique(rec$receiver_id))
  df_template <- df_template %>% mutate(transmitter = built_in_tag)
  
  # Join the template with the data
  df_temp <- left_join(df_template, df_temp)
  
  # arrange along X and replace NA values
  df_temp <- df_temp %>% 
    arrange(X, desc(det_type)) %>% 
    mutate(datetime_reg = zoo::na.locf(datetime_reg),
           det_type = ifelse(is.na(det_type), "Detection", det_type),
           detection = ifelse(is.na(datetime), 0, 1))
  print(paste0("Done: ", built_in_tag))
  return(df_temp)
})

df_sync <- plyr::ldply(df_sync_list)


#### Calculate distance ####  
deploy_rec <- deploy %>% # position of receiver
  select(receiver_id, deploy_latitude, deploy_longitude)
deploy_tag <- deploy %>%  # position of transmitter
  select(transmitter = built_in_tag_id, 
         tag_latitude = deploy_latitude, 
         tag_longitude = deploy_longitude)

# Make dataframe of all possible combinations
df_dist <- expand.grid(receiver_id = deploy$receiver_id,
                       transmitter = deploy$built_in_tag_id)
df_dist <- df_dist %>%  # join with receiver and tag positions
  left_join(deploy_rec) %>% 
  left_join(deploy_tag)

df_dist <- df_dist %>% 
  mutate(bear = geosphere::bearing( # calculate bearing
    matrix(c(deploy_longitude, deploy_latitude), ncol = 2),
    matrix(c(tag_longitude, tag_latitude), ncol = 2))) %>% 
  mutate(distance = geosphere::distGeo( # calculate distance
    matrix(c(deploy_longitude, deploy_latitude), ncol = 2),
    matrix(c(tag_longitude, tag_latitude), ncol = 2))) %>% 
  select(receiver_id, transmitter, distance, bear)
rm(deploy_rec, deploy_tag) 

df_sync <- df_sync %>%
  left_join(df_dist) # join distance with detection data

# Add distance categories
df_sync <- df_sync %>% 
  mutate(dist_class = cut(
    distance,
    breaks = c(-.1, 1, 200, 300, 400, 500, 600, 1000, 2000),
    labels = c("0", "0-200", "200-300", "300-400", "400-500", "500-600", "600-1000", "1000-2000")
  ))

#### Add time variables hour and day ####
df_sync <- df_sync %>% 
  mutate(datetime_hour = datetime_reg,
         datetime_date = date(datetime_reg))
minute(df_sync$datetime_hour) <- 0
second(df_sync$datetime_hour) <- 0

#### Link to transmitting power
date_Lowtohigh <- date("2020-06-16") # Day of switching Low to High: 16 June 2020

df_sync <- df_sync %>% 
  mutate(date = date(datetime_reg)) %>% 
  # Remove detections of day of switching low to high
  filter(date != date_Lowtohigh) %>% 
  select(-date)

df_sync <- deploy %>%  # add column transmit power
  select(transmitter = built_in_tag_id,
         transmit_power_output) %>% 
  right_join(df_sync)

df_sync <- df_sync %>% # change Low to High for the date it was changed with VR100
  mutate(transmit_power_output = ifelse( 
    transmit_power_output == "Low" & date(datetime_reg) > date_Lowtohigh, 
    "High",
    transmit_power_output
  ))


#### Save data ####
write.csv(df_sync, "data/interim/df_sync.csv", row.names = F)

