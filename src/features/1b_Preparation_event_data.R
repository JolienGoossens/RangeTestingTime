
######################
# Format event data  #
######################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(lubridate)

#### Get data ####
# Event data: extracted from VUE after time correction
ev <- read.csv("data/raw/Belwind_oct2020_event.csv", stringsAsFactors = F)

#### Format time and filter####
ev <- ev %>% 
  mutate(datetime = parse_date_time(Date.and.Time..UTC., orders = "ymd HMS"),
         date = date(datetime)) %>% 
  # Remove detections before deployment date and after recovery date
  filter(ymd("2020-05-13") < date &
           date < ymd("2020-10-12")) %>% 
  # Remove day of switching Low to High
  filter(date != ymd("2020-06-16")) %>% 
  select(-date)

#### Get sensor information ####
# First make separate dataframes for every sensor, then join
# Tilt
ev_tilt <- ev %>% 
  filter(Description == "Tilt angle") %>% 
  select(receiver_id = Receiver, tilt = Data, datetime)

# Depth
ev_depth <- ev %>% 
  filter(Description == "Seawater depth") %>% 
  select(receiver_id = Receiver, depth = Data, datetime)

# Noise
ev_noise <- ev %>% 
  filter(Description == "Average noise") %>% 
  select(receiver_id =Receiver, noise = Data, datetime)

# Temperature 
ev_temp <- ev %>% 
  filter(Description == "Temperature") %>% 
  select(receiver_id = Receiver, temperature = Data, datetime)

# Pings 
ev_pings <- ev %>% 
  filter(Description == "Hourly Pings on 69 kHz") %>% 
  select(receiver_id = Receiver, pings = Data, datetime)


# Join all
ev <- ev_tilt %>% 
  left_join(ev_depth) %>% 
  left_join(ev_noise) %>% 
  left_join(ev_temp) %>% 
  left_join(ev_pings) %>%
  mutate(
    tilt = as.numeric(tilt),
    depth = as.numeric(depth),
    noise = as.numeric(noise),
    temperature = as.numeric(temperature),
    pings = as.numeric(pings)
  )

#### Save files ####
write.csv(ev, "data/interim/ev.csv", row.names = F)

