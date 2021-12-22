
###########################################
#  Model selection for hourly resolution  #
###########################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)

#### Get data ####
df_sync_hour <- read_csv("data/interim/df_sync_hour.csv")
df_dist <- read_csv("data/interim/df_dist.csv")
deploy <- read_csv("data/interim/deploy.csv")

#### Filter and select for model ####
df_model_hour_full <- df_sync_hour %>% 
  filter(distance != 0) %>% 
  filter(distance < 1100) %>% 
  select(datetime_hour, station_rec, receiver_id, transmitter,
         distance.st, distance, transmit_power_output,
         ts_noise_rec.st, ts_noise_rec,
         wind_speed_median.st, wind_azimuth.st, wind_speed_median, wind_azimuth,
         # cur_speed_bot.st,cur_speed_bot,  
         cur_azimuth.st, cur_azimuth,
         # ts_temp.st, ts_temp,
         ts_tilt.st, ts_tilt,
         days_since_deploy.st, days_since_deploy,
         det_bin, perc_det, n_det, maxi) 

#### Model selection: noise or current ####
# Take noise: takes into account more than current (+ allows to not take into account wind speed)
# Don't use temperature: weird in exploration

#### Base model with two power transformations ####
model_formula_hour_noise <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st + 
  transmit_power_output +
  ts_noise_rec.st + 
  wind_speed_median.st + 
  cur_azimuth.st +
  I(wind_azimuth.st^2) + 
  wind_azimuth.st+
  ts_tilt.st + 
  days_since_deploy.st

glm_hour_noise <- glm(model_formula_hour_noise, family = "binomial", data = df_model_hour_full)

#### Check interactions ####
glm_hour_noise_int <- update(glm_hour_noise, .~. 
                             + distance.st:transmit_power_output 
                             + distance.st:ts_noise_rec.st 
                             + distance.st:cur_azimuth.st)
drop1(glm_hour_noise_int, test = "LRT")
summary(glm_hour_noise_int)
# Remove current interaction (LRT 2, p value 0.1471541)

#### Remove redundant variables ####
glm_hour_noise_intnoise_inttrans <- update(glm_hour_noise, .~. 
                                           + distance.st:ts_noise_rec.st 
                                           + distance.st:transmit_power_output)
drop1(glm_hour_noise_intnoise_inttrans, test = "LRT")
summary(glm_hour_noise_intnoise_inttrans)

# Remove cur azimuth: not significant (LRT 2, p value 0.1203658)
glm_hour_noise_sel = update(glm_hour_noise_intnoise_inttrans, .~.
                            - cur_azimuth.st)
drop1(glm_hour_noise_sel, test = "LRT")
summary(glm_hour_noise_sel)

# Remove wind azimuth power: effect too little (est 0.0016373)
glm_hour_noise_sel = update(glm_hour_noise_intnoise_inttrans, .~.
                            - cur_azimuth.st
                            - I(wind_azimuth.st^2))
drop1(glm_hour_noise_sel, test = "LRT")
summary(glm_hour_noise_sel)

# Remove wind azimuth: effect too little (est 0.0016354)
glm_hour_noise_sel = update(glm_hour_noise_intnoise_inttrans, .~.
                            - cur_azimuth.st
                            - I(wind_azimuth.st^2)
                            - wind_azimuth.st)
drop1(glm_hour_noise_sel, test = "LRT")
summary(glm_hour_noise_sel)

# Remove days: effect too little (est -0.0965320)
glm_hour_noise_sel = update(glm_hour_noise_intnoise_inttrans, .~.
                            - cur_azimuth.st
                            - I(wind_azimuth.st^2)
                            - wind_azimuth.st
                            - days_since_deploy.st)
drop1(glm_hour_noise_sel, test = "LRT")
summary(glm_hour_noise_sel)

# Remove tilt: effect too little (est -0.1759538)
glm_hour_noise_sel = update(glm_hour_noise_intnoise_inttrans, .~.
                            - cur_azimuth.st
                            - I(wind_azimuth.st^2)
                            - wind_azimuth.st
                            - days_since_deploy.st
                            - ts_tilt.st )
drop1(glm_hour_noise_sel, test = "LRT")
summary(glm_hour_noise_sel)

# Remove wind speed (est -0.1468308) -> exploration: relationship with noise
glm_hour_noise_sel = update(glm_hour_noise_intnoise_inttrans, .~.
                            - cur_azimuth.st
                            - I(wind_azimuth.st^2)
                            - wind_azimuth.st
                            - days_since_deploy.st
                            - tilt.st
                            - wind_speed_median.st)
drop1(glm_hour_noise_sel, test = "LRT")
summary(glm_hour_noise_sel)

# Final model
model_formula_hour_noise_p <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st +
  distance.st *  transmit_power_output +
  distance.st * ts_noise_rec.st 

# Subset for the variables used
df_model_hour = df_model_hour_full %>% 
  select(datetime_hour, station_rec, receiver_id, transmitter,
         distance, distance.st,
         transmit_power_output,
         ts_noise_rec, ts_noise_rec.st,
         det_bin, perc_det, n_det, maxi)

glm_hour_noise_int_p <- glm(model_formula_hour_noise_p, family = "binomial", data = df_model_hour)
summary(glm_hour_noise_int_p)
drop1(glm_hour_noise_int_p, test = "LRT")

# Final model joepie!

#### Save model data set ####
write_csv(df_model_hour, "data/interim/df_model_hour.csv")
