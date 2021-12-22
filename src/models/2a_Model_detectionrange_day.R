
##########################################
#  Model selection for daily resolution  #
##########################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)

#### Get data ####
df_sync_day <- read_csv("data/interim/df_sync_day.csv")
df_dist <- read_csv("data/interim/df_dist.csv")
deploy <- read_csv("data/interim/deploy.csv")

#### Filter and select for model ####
df_model_day_full <- df_sync_day %>% 
  filter(distance != 0) %>% 
  filter(distance < 1100) %>%
  select(datetime_date, station_rec, receiver_id, transmitter,
         distance.st, distance, transmit_power_output,
         ts_noise_rec_median.st, ts_noise_rec_median,
         wind_speed_median.st, wind_azimuth.st, wind_speed_median, wind_azimuth,
         # cur_speed_bot_median.st, cur_speed_bot_median,
         # ts_temp_median.st, ts_temp_median,
         ts_tilt_median.st, ts_tilt_median,
         days_since_deploy.st, days_since_deploy,
         det_bin, perc_det, n_det, maxi) 

#### Model selection: noise or current ####
# Take noise: takes into account more than current (+ allows to not take into account wind speed)
# Don't use temperature: weird in exploration

#### Base model with two power transformations ####
model_formula_day_noise <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st + 
  transmit_power_output +
  ts_noise_rec_median.st +
  wind_speed_median.st + 
  I(wind_azimuth.st^2) + 
  wind_azimuth.st + 
  ts_tilt_median.st + 
  days_since_deploy.st


glm_day_noise <- glm(model_formula_day_noise, family = "binomial", data = df_model_day_full)

#### Check interactions ####
glm_day_noise_int <- update(glm_day_noise, .~. 
                             + distance.st:transmit_power_output 
                             + distance.st:ts_noise_rec_median.st)
drop1(glm_day_noise_int, test = "LRT")
summary(glm_day_noise_int)



#### Remove redundant variables ####
# Remove wind_azimuth power: only limited contribution (est 0.0027, LRT 11)
glm_day_noise_sel = update(glm_day_noise_int, .~.
                           - I(wind_azimuth.st^2))
drop1(glm_day_noise_sel, test = "LRT")
summary(glm_day_noise_sel)

# Remove wind_azimuth: effect too little (est 0.0071, LRT 79)
glm_day_noise_sel = update(glm_day_noise_int, .~.
                           - I(wind_azimuth.st^2)
                           - wind_azimuth.st)
drop1(glm_day_noise_sel, test = "LRT")
summary(glm_day_noise_sel)
# Remove days: effect too little (est -0.0842)
glm_day_noise_sel = update(glm_day_noise_int, .~.
                           - I(wind_azimuth.st^2)
                           - wind_azimuth.st
                           - days_since_deploy.st)
drop1(glm_day_noise_sel, test = "LRT")
summary(glm_day_noise_sel)
# Remove tilt: effect too little (est -0.0918)
glm_day_noise_sel = update(glm_day_noise_int, .~.
                           - I(wind_azimuth.st^2)
                           - wind_azimuth.st
                           - days_since_deploy.st
                           - ts_tilt_median.st)
drop1(glm_day_noise_sel, test = "LRT")
summary(glm_day_noise_sel)


# Remove temperature: effect too little
glm_day_noise_sel = update(glm_day_noise_int, .~.
                           - I(wind_azimuth.st^2)
                           - wind_azimuth.st
                           - days_since_deploy.st
                           - ts_tilt_median.st)
drop1(glm_day_noise_sel, test = "LRT")
summary(glm_day_noise_sel)

# Remove median wind speed: effect too little (est -0.1560357)
model_formula_day_noise_p <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st +
  distance.st * transmit_power_output +
  distance.st * ts_noise_rec_median.st

# Subset for the variables used
df_model_day = df_model_day_full %>% 
  select(datetime_date, station_rec, receiver_id, transmitter,
         distance, distance.st,
         transmit_power_output,
         ts_noise_rec_median, ts_noise_rec_median.st,
         det_bin, perc_det, n_det, maxi)

glm_day_noise_int_p <- glm(model_formula_day_noise_p, family = "binomial", data = df_model_day_full)

drop1(glm_day_noise_int_p, test = "LRT")
summary(glm_day_noise_int_p)

# Final model joepie!

#### Save model data set ####
write_csv(df_model_day, "data/interim/df_model_day.csv")
