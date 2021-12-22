
###################################
#  Explore data before modelling  #
###################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load data and packages ####
source("src/backfun/Readandformat_exploration.R")
library(ggpubr)
library(geofacet)

Sys.setlocale("LC_ALL", "English")

#### Make grid for plotting ####
# Generated with online tool geofacet
mygrid <- data.frame(
  name = c("JJ_CD78", "JJ_C8_1", "JJ_C8_6", "JJ_B8_1", "JJ_C8_5", "JJ_C8_2", "JJ_C8_3", "JJ_C8_4", "JJ_DE89", "JJ_BC89", "JJ_D9_6", "JJ_D9_1", "JJ_A9_1", "JJ_B9_6", "JJ_B9_1", "JJ_E9_1", "JJ_D9_5", "JJ_D9_2", "JJ_C9_1", "JJ_B9_2", "JJ_B9_5", "JJ_D9_3", "JJ_D9_4", "JJ_B9_4", "JJ_B9_3", "JJ_C10_1", "JJ_B10_1"),
  code = c("CD78", "C8_1", "C8_6", "B8_1", "C8_5", "C8_2", "C8_3", "C8_4", "DE89", "BC89", "D9_6", "D9_1", "A9_1", "B9_6", "B9_1", "E9_1", "D9_5", "D9_2", "C9_1", "B9_2", "B9_5", "D9_3", "D9_4", "B9_4", "B9_3", "C10_1", "B10_1"),
  row = c(1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9),
  col = c(5, 4, 5, 3, 5, 4, 4, 5, 8, 3, 7, 6, 1, 3, 2, 8, 7, 6, 4, 2, 3, 6, 7, 3, 2, 5, 3),
  stringsAsFactors = FALSE
)
#geofacet::grid_preview(mygrid)

#### Exploration of each independent variable: wind speed ####
# Density
p1 <- df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_speed_median)) +
  geom_density(aes(wind_speed_max), colour = "red") +
  labs(x= "", y = "", title = "Day: Wind speed (black: median; red: max)")

p2 <- df_env_hour %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_speed_median)) +
  geom_density(aes(wind_speed_max), colour = "red")+
  labs(x= "", y = "", title = "Hour: Wind speed")

p3 <- df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_speed_median.st)) +
  geom_density(aes(wind_speed_max.st), colour = "red") +
  labs(x= "", y = "", title = "Day: Wind speed standardized")


p4 <- df_env_hour %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_speed_median.st)) +
  geom_density(aes(wind_speed_max.st), colour = "red") +
  labs(x= "", y = "", title = "Hour: Wind speed standardized")

ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_windspeed.jpg"), width = 16, height = 10)

# Over time
p1 <- df_env_day %>% 
  ggplot(aes(datetime_date)) +
  theme_bw()+
  geom_path(aes(y = wind_speed_median)) +
  geom_path(aes(y = wind_speed_max), colour = "red") +
  labs(x= "", y = "", title = "Day: Wind speed (black: median; red: max)")

p2 <- df_env_hour %>% 
  ggplot(aes(datetime_hour)) +
  theme_bw()+
  geom_path(aes(y = wind_speed_median)) +
  geom_path(aes(y = wind_speed_max), colour = "red") +
  labs(x= "", y = "", title = "Hour: Wind speed")

p3 <- df_env_day %>% 
  ggplot(aes(datetime_date)) +
  theme_bw()+
  geom_path(aes(y = wind_speed_median.st)) +
  geom_path(aes(y = wind_speed_max.st), colour = "red") +
  labs(x= "", y = "", title = "Day: Wind speed standardized")

p4 <- df_env_hour %>% 
  ggplot(aes(datetime_hour)) +
  theme_bw()+
  geom_path(aes(y = wind_speed_median.st)) +
  geom_path(aes(y = wind_speed_max.st), colour = "red") +
  labs(x= "", y = "", title = "Hour: Wind speed standardized")
ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_windspeed_time.jpg"), width = 16, height = 10)

# Standardization: matters little wether using median or maximum -> median

df_env_hour %>% 
  ggplot(aes(x = datetime_hour, y = wind_speed_median)) +
  theme_bw()+
  geom_path() +
  geom_path(data = df_env_day, aes(x = datetime_date, y = wind_speed_median), colour = "red")+
  geom_path(data = df_env_day, aes(x = datetime_date, y = wind_speed_max), colour = "blue") +
  labs(x= "", y = "", title = "Wind speed hourly (black) + daily (red: median; blue: max)")
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_windspeed_hourday.jpg"), width = 16, height = 10)
# Daily wind speed metrics representative of hourly variation

#### Exploration of each independent variable: current speed ####
# Density
p1 <- df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(cur_speed_bot_median)) +
  geom_density(aes(cur_speed_bot_max), colour = "red") +
  labs(x= "", y = "", title = "Day: Current speed (black: median; red: max)")

p2 <- df_env_hour %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(cur_speed_bot)) +
  labs(x= "", y = "", title = "Hour: Current speed")

p3 <- df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(cur_speed_bot_median.st)) +
  geom_density(aes(cur_speed_bot_max.st), colour = "red") +
  labs(x= "", y = "", title = "Day: Current speed standardized")

p4 <- df_env_hour %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(cur_speed_bot)) +
  labs(x= "", y = "", title = "Hour: Current speed standardized")

ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_currentspeed.jpg"), width = 16, height = 10)


# Over time
p1 <- df_env_day %>% 
  ggplot(aes(datetime_date)) +
  theme_bw()+
  geom_path(aes(y = cur_speed_bot_median)) +
  geom_path(aes(y = cur_speed_bot_max), colour = "red") +
  labs(x= "", y = "", title = "Day: Current speed (black: median; red: max)")

p2 <- df_env_hour %>% 
  ggplot(aes(datetime_hour)) +
  theme_bw()+
  geom_path(aes(y = cur_speed_bot)) +
  labs(x= "", y = "", title = "Hour: Current speed")

p3 <- df_env_day %>% 
  ggplot(aes(datetime_date)) +
  theme_bw()+
  geom_path(aes(y = cur_speed_bot_median.st)) +
  geom_path(aes(y = cur_speed_bot_max.st), colour = "red") +
  labs(x= "", y = "", title = "Day: Current speed standardized")

p4 <- df_env_hour %>% 
  ggplot(aes(datetime_hour)) +
  theme_bw()+
  geom_path(aes(y = cur_speed_bot.st)) +
  labs(x= "", y = "", title = "Hour: Current speed standardized")
ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_currentspeed_time.jpg"), width = 16, height = 10)

# Standardization: matters little wether using median or maximum -> median

p1 <- df_env_hour %>% 
  ggplot(aes(x = datetime_hour, y = cur_speed_bot)) +
  theme_bw()+
  geom_path() +
  geom_path(data = df_env_day, aes(x = datetime_date, y = cur_speed_bot_median), colour = "red")+
  geom_path(data = df_env_day, aes(x = datetime_date, y = cur_speed_bot_max), colour = "blue") +
  labs(x= "", y = "", title = "Current speed hourly (black) + daily (red: median; blue: max)")

p2 <- df_env_hour %>% 
  ggplot(aes(x = datetime_hour, y = cur_speed_bot.st)) +
  theme_bw()+
  geom_path() +
  geom_path(data = df_env_day, aes(x = datetime_date, y = cur_speed_bot_median.st), colour = "red")+
  geom_path(data = df_env_day, aes(x = datetime_date, y = cur_speed_bot_max.st), colour = "blue") +
  labs(x= "", y = "", title = "Standardized current speed hourly (black) + daily")
# Daily wind speed metrics representative of hourly variation
ggarrange(p1, p2, nrow = 2)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_currentspeed_hourday.jpg"), width = 16, height = 10)


#### Exploration of each independent variable: wind direction ####
# Density
p1 <- df_sync_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_azimuth), colour = "red") +
  geom_density(data = df_sync_hour, aes(wind_azimuth)) +
  labs(x= "", y = "", title = "Wind azimuth hourly (black) and daily (red)")

p2 <- df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_dir_median), colour = "red") +
  geom_density(data = df_env_hour, aes(wind_dir_median)) +
  labs(x= "", y = "", title = "Wind direction hourly (black) and daily (red)")

p3 <- df_sync_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(wind_azimuth.st), colour = "red") +
  geom_density(data = df_sync_hour, aes(wind_azimuth.st)) +
  labs(x= "", y = "", title = "Standardized wind azimuth hourly (black) and daily (red)")

ggarrange(p1, p2, p3)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_winddir.jpg"), width = 16, height = 10)


# Over time
df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_path(data = df_env_hour, aes(datetime_hour, wind_dir_median), size = 1.5) +
  geom_path(aes(datetime_date, wind_dir_median), colour = "red", size = 1.5) +
  labs(x= "", y = "", title = "Wind direction hourly (black) and daily (red)")
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_winddir_time.jpg"), width = 16, height = 10)

# Daily wind direction representative of hourly variation

#### Exploration of each independent variable: current direction ####
# Density
p1 <- df_sync_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(cur_azimuth), colour = "red") +
  geom_density(data = df_sync_hour, aes(wind_azimuth)) +
  labs(x= "", y = "", title = "Current azimuth hourly (black) and daily (red)")

p2 <- df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(cur_dir_bot_median), colour = "red") +
  geom_density(data = df_env_hour, aes(cur_dir_bot)) +
  labs(x= "", y = "", title = "Current direction hourly (black) and daily (red)")

p3 <- df_sync_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(data = df_sync_hour, aes(cur_azimuth.st)) +
  geom_density(aes(cur_azimuth.st), colour = "red") +
  labs(x= "", y = "", title = "Standardized current azimuth hourly (black) and daily (red)")

ggarrange(p1, p2, p3)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_currentdir.jpg"), width = 16, height = 10)

# Over time
df_env_day %>% 
  ggplot() +
  theme_bw()+
  geom_path(data = df_env_hour, aes(datetime_hour, cur_dir_bot)) +
  geom_path(aes(datetime_date, cur_dir_bot_median), colour = "red", size = 1) +
  labs(x= "", y = "", title = "Current direction hourly (black) and daily (red)")
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_currentdir_time.jpg"), width = 16, height = 10)

# Daily current direction not representative of hourly variation (tidal currents)


#### Exploration of each independent variable: noise ####
# Density
p1 <- df_sync_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(ts_noise_rec_mean)) +
  geom_density(aes(ts_noise_rec_median), colour = "red") +
  labs(x= "", y = "", title = "Day: Noise")

p2 <- df_sync_hour %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(ts_noise_rec)) +
  labs(x= "", y = "", title = "Hour: Noise")

p3 <- df_sync_day %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(ts_noise_rec_mean.st)) +
  geom_density(aes(ts_noise_rec_median.st), colour = "red") +
  labs(x= "", y = "", title = "Day: Noise standardized")

p4 <- df_sync_hour %>% 
  ggplot() +
  theme_bw()+
  geom_density(aes(ts_noise_rec.st)) +
  labs(x= "", y = "", title = "Hour: Noise standardized")

ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_noise.jpg"), width = 16, height = 10)


# Over time
# p1 <- df_sync_day %>% 
#   ggplot(aes(datetime_date)) +
#   theme_bw()+
#   geom_path(aes(y = ts_noise_rec_median), colour = "red") +
#   geom_path(aes(y = ts_noise_rec_mean)) +
#   labs(x= "", y = "", title = "Day: Noise")
# 
# p2 <- df_sync_hour %>% 
#   ggplot(aes(datetime_hour)) +
#   theme_bw()+
#   geom_path(aes(y = ts_noise_rec)) +
#   labs(x= "", y = "", title = "Hour: Noise")
# 
# p3 <- df_sync_day %>% 
#   ggplot(aes(datetime_date)) +
#   theme_bw()+
#   geom_path(aes(y = ts_noise_rec_median.st), colour = "red") +
#   geom_path(aes(y = ts_noise_rec_mean.st)) +
#   labs(x= "", y = "", title = "Day: Noise standardized")
# 
# p4 <- df_sync_hour %>% 
#   ggplot(aes(datetime_hour)) +
#   theme_bw()+
#   geom_path(aes(y = ts_noise_rec.st)) +
#   labs(x= "", y = "", title = "Hour: Noise standardized")
# ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_noise_time.jpg"), width = 16, height = 10)

# Median seems more sensitive to variation

# df_sync_hour %>% 
#   ggplot(aes(x = datetime_hour, y = ts_noise_rec)) +
#   geom_path() +
#   geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_median), colour = "red")+
#   geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_mean), colour = "blue") +
#   labs(x= "", y = "", title = "Noise hourly (black) + daily")
# 
# df_sync_hour %>% 
#   ggplot(aes(x = datetime_hour, y = ts_noise_rec.st)) +
#   geom_path() +
#   geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_median.st), colour = "red")+
#   geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_mean.st), colour = "blue") +
#   facet_wrap(~receiver_id) +
#   labs(x= "", y = "", title = "Standardized noise hourly (black) + daily")
# Median seems more sensitive to variation

df_sync_hour %>% 
  ggplot(aes(x = datetime_hour, y = ts_noise_rec), size = 0.1) +
  geom_path() +
  geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_median), colour = "red")+
  geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_mean), colour = "blue") +
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Noise hourly (black) + daily")
# ggsave(filename = paste0("reports/figures/geowrap_dayhour_noise.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  ggplot(aes(x = datetime_hour, y = ts_noise_rec.st), size = 0.1) +
  geom_path() +
  geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_median.st), colour = "red")+
  geom_path(data = df_sync_day, aes(x = datetime_date, y = ts_noise_rec_mean.st), colour = "blue") +
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Noise hourly (black) + daily")

df_sync_day %>% 
  arrange(datetime_date) %>% 
  ggplot(aes(x = datetime_date), size = 0.1) +
  theme_bw() +
  geom_path(aes(y = ts_noise_rec_median), colour = "red") +
  geom_path(aes(y = ts_noise_rec_mean), colour = "blue") +
  facet_geo(~station_rec, grid = mygrid)
# ggsave(filename = paste0("reports/figures/geowrap_day_noise.jpg"), width = 16, height = 10)

#### Exploration of each independent variable: temperature ####
df_sync_hour %>% 
  arrange(datetime_hour) %>% 
  ggplot(aes(x = datetime_hour, y = ts_temp), size = 0.1) +
  geom_path() +
  geom_path(data = arrange(df_sync_day, datetime_date),
            aes(x = datetime_date, y = ts_temp_median), colour = "red")+
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Temperature hourly (black) + daily median")
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_temp_dayhour.jpg"), width = 16, height = 10)

#### Exploration of each independent variable: tilt ####
df_sync_hour %>% 
  arrange(datetime_hour) %>% 
  ggplot(aes(x = datetime_hour, y = ts_tilt), size = 0.1) +
  geom_path() +
  geom_path(data = arrange(df_sync_day, datetime_date),
            aes(x = datetime_date, y = ts_tilt_median), colour = "red")+
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Tilt hourly (black) + daily median")
# ggsave(filename = paste0("reports/figures/exploration/independent_var/independent_tilt_dayhour.jpg"), width = 16, height = 10)

#### Exploration of each independent variable: distance ####
df_dist %>% 
  filter(distance != 0) %>% 
  ggplot(aes(distance)) +
  theme_minimal() +
  geom_histogram(binwidth = 100, colour = "red") +
  scale_x_continuous(breaks = seq(100, 1600, by = 100), labels = as.character(seq(100, 1600, by = 100)))
# ggsave(filename = paste0("reports/figures/exploration/independent_var/distance_histogram.jpg"), width = 8, height = 5)

p1 <- df_dist %>% 
  filter(distance != 0) %>% 
  group_by(receiver_id, bear_class, distance) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance)) +
#  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  geom_boxplot() +
  geom_point() +
  labs(x = "", y ="", title = "Stations distance vs bearing")

p2 <- df_dist %>% 
  filter(distance != 0) %>% 
  group_by(receiver_id, bear_class, distance) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance)) +
  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot()+
  geom_point()+
  labs(x = "", y ="", title = "Stations distance vs bearing")

p3 <- df_dist %>% 
  filter(distance != 0) %>% 
  filter(distance < 500) %>% 
  group_by(receiver_id, bear_class, distance) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance)) +
  #  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  geom_boxplot() +
  geom_point() +
  labs(x = "", y ="", title = "Stations distance vs bearing (< 500 m)")

p4 <- df_dist %>% 
  filter(distance != 0) %>% 
  filter(distance < 500) %>% 
  group_by(receiver_id, bear_class, distance) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance)) +
  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot()+
  geom_point()+
  labs(x = "", y ="", title = "Stations distance vs bearing (< 500 m)")

ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/independent_var/distance_circ.jpg"), width = 16, height = 10)

# Standardized distance
p1 <- df_dist %>% 
  filter(distance != 0) %>% 
  group_by(receiver_id, bear_class, distance.st) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance.st)) +
  #  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  geom_boxplot() +
  geom_point() +
  labs(x = "", y ="", title = "Stations standardized distance vs bearing")

p2 <- df_dist %>% 
  filter(distance != 0) %>% 
  group_by(receiver_id, bear_class, distance.st) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance.st)) +
  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot()+
  geom_point()+
  labs(x = "", y ="", title = "Stations standardized distance vs bearing")

p3 <- df_dist %>% 
  filter(distance != 0) %>% 
  filter(distance < 500) %>% 
  group_by(receiver_id, bear_class, distance.st) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance.st)) +
  #  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  geom_boxplot() +
  geom_point() +
  labs(x = "", y ="", title = "Stations standardized distance vs bearing (< 500 m)")

p4 <- df_dist %>% 
  filter(distance != 0) %>% 
  filter(distance < 500) %>% 
  group_by(receiver_id, bear_class, distance.st) %>% 
  summarise() %>% 
  ggplot(aes(bear_class, distance.st)) +
  coord_polar() +
  scale_x_discrete(drop=FALSE)+ 
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  geom_boxplot()+
  geom_point()+
  labs(x = "", y ="", title = "Stations standardized distance vs bearing (< 500 m)")

ggarrange(p1, p2, p3, p4)




#### Exploration relationship with each independent variable ####
# only possible for percentage

df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(wind_speed_median, perc_det)) +
  geom_point(size = 0.1) +
  geom_path()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  ggplot(aes(wind_speed_median, perc_det)) +
  geom_point(size = 0.1)





#### Zuur ####
#### Zuur: Step 1: Outliers ####
p1 <- df_sync_day %>% 
  filter(distance != 0) %>%
  ggplot(aes(x =0, y = perc_det)) +
  geom_boxplot() +
  labs(x='', y ="", title= "Daily detection percentage")

p2 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot(aes(x =0, y = perc_det)) +
  geom_boxplot() +
  labs(x='', y ="", title= "Hourly detection percentage")

p3 <- df_sync_day %>% 
  filter(distance != 0) %>%
  ggplot(aes(x = perc_det, y =datetime_date))+
  geom_point(size = 0.1)+
  labs(x='', y ="")

p4 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot(aes(x = perc_det, y =datetime_hour))+
  geom_point(size = 0.1)+
  labs(x='', y ="")
ggarrange(p1, p2, p3, p4)
rm(p1, p2, p3, p4)
# Seems okay!


#### Zuur: Step 2: Homogeneity -> binary data  ####
# Boxplot
p1 <- df_sync_day %>% 
  filter(distance != 0) %>%
  ggplot(aes(x = dist_class, y = perc_det)) +
  geom_boxplot() +
  labs(x='', y ="", title= "Daily detection percentage")

p2 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot(aes(x = dist_class, y = perc_det)) +
  geom_boxplot()+
  labs(x='', y ="", title= "Hourly detection percentage")
ggarrange(p1, p2)
# make other plot! not testing homogeneity!

# ggsave(filename = paste0("reports/figures/exploration/zuur/zuur_2_homogeneity_boxplots.jpg"), width = 10, height = 8)


# Density plots
p1 <- df_sync_day %>% 
  filter(distance != 0) %>%
  ggplot() +
  geom_density(aes(perc_det))+
  labs(x='', y ="", title= "Daily detection percentage")

p2 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot() +
  geom_density(aes(perc_det))+
  labs(x='', y ="", title= "Hourly detection percentage")

p3 <- df_sync_day %>% 
  filter(distance != 0) %>%
  ggplot() +
  geom_density(aes(perc_det)) +
  facet_wrap(~dist_class, nrow = 7, scales = "free_y", strip.position = "right")+
  labs(x='', y ="")

p4 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot() +
  geom_density(aes(perc_det)) +
  facet_wrap(~dist_class, nrow = 7, scales = "free_y", strip.position = "right")+
  labs(x='', y ="")
ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/zuur/zuur_2_homogeneity.jpg"), width = 16, height = 10)

#### Zuur: Step 3: Normality -> binary data  ####
#### Zuur: Step 4: Zeros -> binary data  ####
#### Zuur: Step 5: Collinearity  ####
# Do vif test!!


# Pair plot
panel.cor <- function(x, y){# Correlation panel
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  p <- round(cor.test(x,y)$p.value, digits=2)
  txt <- paste0("R = ", r, ", p = ", p)
  cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor)
}
upper.panel<-function(x, y){# Customize upper panel
  points(x,y, pch = 19, cex = 0.8)
}

# Summarise to have better plot
df_sync_day_sum <- df_sync_day %>%
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id,
           wind_speed_median, cur_speed_bot_median, wind_dir_median, ts_noise_rec_median,
           ts_pings_median, ts_temp_median, ts_tilt_median, transmit_power_output) %>% 
  summarise(perc_det_median = median(perc_det))

pairs(perc_det_median~ts_noise_rec_median + cur_speed_bot_median +wind_speed_median + wind_dir_median+ ts_pings_median + ts_temp_median + ts_tilt_median + transmit_power_output,
      data=df_sync_day_sum,
      main="Scatterplot all data",
      lower.panel = panel.cor,
      upper.panel = upper.panel)
# ggsave(filename = paste0("reports/figures/exploration/zuur/zuur_5_pairsday.jpg"), width = 16, height = 10)



df_sync_hour_sum <- df_sync_hour %>%
  filter(distance != 0) %>% 
  group_by(datetime_hour, receiver_id,
           wind_speed_median, cur_speed_bot, wind_dir_median, ts_noise_rec,
           ts_pings, ts_temp, ts_tilt, cur_dir_bot, transmit_power_output) %>% 
  summarise(perc_det_median = median(perc_det))

pairs(perc_det_median~ts_noise_rec + cur_speed_bot +wind_speed_median + wind_dir_median + ts_pings + ts_temp + ts_tilt + cur_dir_bot + transmit_power_output,
      data=df_sync_hour_sum,
      main="Scatterplot all data",
      lower.panel = panel.cor,
      upper.panel = upper.panel)
# ggsave(filename = paste0("reports/figures/exploration/zuur/zuur_5_hoursday.jpg"), width = 16, height = 10)


# Check relationship noise and current speed
df_sync_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id, ts_noise_rec_median, cur_speed_bot_median) %>% 
  summarise() %>% 
  ggplot(aes(cur_speed_bot_median, ts_noise_rec_median)) +
  geom_point()
  
df_sync_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id, ts_noise_rec_median, cur_speed_bot_median) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_date)) +
  geom_path(aes(y= ts_noise_rec_median)) +
  geom_path(aes(y= cur_speed_bot_median*1000), colour = "red") +
  scale_y_continuous(sec.axis = sec_axis(~(./1000)))

df_sync_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, station_rec, ts_noise_rec_median, cur_speed_bot_median) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_date)) +
  geom_path(aes(y= ts_noise_rec_median)) +
  geom_path(aes(y= cur_speed_bot_median*1000), colour = "red") +
  scale_y_continuous(sec.axis = sec_axis(~(./1000))) +
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Daily noise (black) and median current speed (red)")
# Important relationship!! For most stations: exactly same pattern
# Run model with each! (never together)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, receiver_id, ts_noise_rec, cur_speed_bot) %>% 
  summarise() %>% 
  ggplot(aes(cur_speed_bot, ts_noise_rec)) +
  geom_point(size = 0.1)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, receiver_id, ts_noise_rec, cur_speed_bot) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_hour)) +
  geom_path(aes(y= ts_noise_rec), size = 0.1) +
  geom_path(aes(y= cur_speed_bot*1000), colour = "red", size = 0.1) +
  scale_y_continuous(sec.axis = sec_axis(~(./1000)))

df_sync_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, station_rec, ts_noise_rec, cur_speed_bot) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_hour)) +
  geom_path(aes(y= ts_noise_rec), size = 0.1) +
  geom_path(aes(y= cur_speed_bot*1000), colour = "red", alpha = 0.7, size = 0.1) +
  scale_y_continuous(sec.axis = sec_axis(~(./1000))) +
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Hourly noise (black) and median current speed (red)")
# Important relationship!! For most stations: exactly same pattern
# Run model with each! (never together)

# Check relationship noise and wind speed
df_sync_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id, ts_noise_rec_median, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(wind_speed_median, ts_noise_rec_median)) +
  geom_point()

df_model_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id, ts_noise_rec_median, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(cut(wind_speed_median, breaks = 20), ts_noise_rec_median)) +
  geom_boxplot()

df_model_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, receiver_id, ts_noise_rec, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(cut(wind_speed_median, breaks = 20), ts_noise_rec)) +
  geom_boxplot()

df_model_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id, ts_noise_rec_median, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(cut(ts_noise_rec_median, breaks = 20), wind_speed_median )) +
  geom_boxplot()

df_sync_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, receiver_id, ts_noise_rec_median, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_date)) +
  geom_path(aes(y= ts_noise_rec_median)) +
  geom_path(aes(y= wind_speed_median*50), colour = "red") +
  scale_y_continuous(sec.axis = sec_axis(~(./50)))

df_sync_day %>% 
  filter(distance != 0) %>% 
  group_by(datetime_date, station_rec, ts_noise_rec_median, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_date)) +
  geom_path(aes(y= ts_noise_rec_median)) +
  geom_path(aes(y= wind_speed_median*50), colour = "red") +
  scale_y_continuous(sec.axis = sec_axis(~(./50))) +
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Daily noise (black) and median wind speed (red)")
# Some relationship (if you remove storm, acceptable)


df_sync_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, receiver_id, ts_noise_rec, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(wind_speed_median, ts_noise_rec)) +
  geom_point(size = 0.1)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, receiver_id, ts_noise_rec, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_hour)) +
  geom_path(aes(y= ts_noise_rec), size = 0.1) +
  geom_path(aes(y= wind_speed_median*50), colour = "red", size = 0.1) +
  scale_y_continuous(sec.axis = sec_axis(~(./50)))

df_sync_hour %>% 
  filter(distance != 0) %>% 
  group_by(datetime_hour, station_rec, ts_noise_rec, wind_speed_median) %>% 
  summarise() %>% 
  ggplot(aes(x= datetime_hour)) +
  geom_path(aes(y= ts_noise_rec), size = 0.1) +
  geom_path(aes(y= wind_speed_median*50), colour = "red", alpha = 0.7, size = 0.1) +
  scale_y_continuous(sec.axis = sec_axis(~(./50))) +
  facet_geo(~station_rec, grid = mygrid) +
  labs(x= "", y = "", title = "Hourly noise (black) and median current speed (red)")
# Not really relationship

# Wind direction and speed
df_env_day %>% 
  ggplot(aes(wind_dir_median, wind_speed_median)) +
  geom_point()
df_env_hour %>% 
  ggplot(aes(wind_dir_median, wind_speed_median)) +
  geom_point()
# Not really a relationship, but winds are less frequent in some directions

# Current direction and speed
df_env_hour %>% 
  ggplot(aes(cur_dir_bot, cur_speed_bot)) +
  geom_point()
# Obvious relationship: tides -> azimuth should be okay?

# Temperature and days since
df_sync_day %>% 
  ggplot(aes(days_since_deploy, ts_temp_median)) +
  geom_point()


#### Zuur: Step 6: Relationship ####
# Difficulty: need to take into account distance as well as other factors
# Distance
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(distance, perc_det)) +
  geom_point()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  ggplot(aes(distance, perc_det)) +
  geom_point()

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(dist_class2 = as.factor(cut(distance, breaks = seq(100, 1600, by = 100)))) %>% 
  ggplot(aes(dist_class2, perc_det)) +
  geom_boxplot()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(dist_class2 = as.factor(cut(distance, breaks = seq(100, 1600, by = 100)))) %>% 
  ggplot(aes(dist_class2, perc_det)) +
  geom_boxplot()

p1 <- df_sync_day %>%
  filter(distance != 0) %>%
  ggplot() +
  theme_minimal() +
  geom_boxplot(aes(x = dist_class, y = perc_det), fill = "indianred3")+
  labs(x= "", y = "", title = "Daily detection percentage")

p2 <- df_sync_hour %>%
  filter(distance != 0) %>%
  ggplot() +
  theme_minimal() +
  geom_boxplot(aes(x = dist_class, y = perc_det), fill = "indianred3")+
  labs(x= "", y = "", title = "Hourly detection percentage")

p3 <- df_sync_day %>%
  filter(distance != 0) %>%
  ggplot(aes(fill = factor(det_bin), x = dist_class)) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_bar(position = "fill", stat = 'count') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c("rosybrown1", "indianred3"), name = '') +
  labs(x= "", y = "", title = "Percentage of days with a detection")

p4 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot(aes(fill = factor(det_bin), x = dist_class)) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_bar(position = "fill", stat = 'count') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c("rosybrown1", "indianred3"), name = '') +
  labs(x= "", y = "", title = "Percentage of hours with a detection")
ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_distance.jpg"), width = 16, height = 10)


# Relation with distance for low and high power output
p1 <- df_sync_day %>%
  filter(distance != 0) %>%
  ggplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_boxplot(aes(x = dist_class, y = perc_det, colour = transmit_power_output), fill = "indianred3")+
  labs(x= "", y = "", title = "Daily detection percentage")

p2 <- df_sync_hour %>%
  filter(distance != 0) %>%
  ggplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_boxplot(aes(x = dist_class, y = perc_det, colour = transmit_power_output), fill = "indianred3")+
  labs(x= "", y = "", title = "Hourly detection percentage")

p3 <- df_sync_day %>%
  filter(distance != 0) %>%
  ggplot(aes(fill = factor(det_bin), x = transmit_power_output , colour = transmit_power_output)) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_bar(position = "fill", stat = 'count') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c("rosybrown1", "indianred3"), name = '') +
  facet_wrap(~dist_class, nrow = 1) + theme(panel.spacing = unit(0, "lines")) +
  labs(x= "", y = "", title = "Percentage of days with a detection")

p4 <- df_sync_hour %>% 
  filter(distance != 0) %>%
  ggplot(aes(fill = factor(det_bin), x = transmit_power_output , colour = transmit_power_output)) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_bar(position = "fill", stat = 'count') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c("rosybrown1", "indianred3"), name = '')+
  facet_wrap(~dist_class, nrow = 1) + theme(panel.spacing = unit(0, "lines")) + 
  labs(x= "", y = "", title = "Percentage of hours with a detection")
ggarrange(p1, p2, p3, p4)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_distance_lohi.jpg"), width = 16, height = 10)


# Noise
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(ts_noise_rec_median, perc_det)) +
  geom_point(size = 0.1) +
  facet_wrap(~dist_class)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(noise_class = as.factor(cut(ts_noise_rec_median, seq(100, 800, 100)))) %>% 
  ggplot(aes(noise_class, perc_det)) +
  theme_bw() +
  geom_boxplot() +
  facet_wrap(~dist_class, nrow = 2)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_noise_distance_day.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(noise_class = as.factor(cut(ts_noise_rec, breaks = 15))) %>% 
  ggplot(aes(noise_class, perc_det)) +
  theme_bw() +
  geom_boxplot() +
  facet_wrap(~dist_class)
facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_noise_distance_hour.jpg"), width = 16, height = 10)


df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(noise_class = as.factor(cut(ts_noise_rec_median, breaks = 15))) %>% 
  ggplot(aes(noise_class, perc_det)) +
  geom_boxplot()

df_sync_day %>%
  filter(distance != 0) %>%
  mutate(noise_class = as.factor(cut(ts_noise_rec_median, breaks = 15))) %>% 
  ggplot(aes(fill = factor(det_bin), x = noise_class)) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_bar(position = "fill", stat = 'count') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c("rosybrown1", "indianred3"), name = '') +
  labs(x= "", y = "", title = "Percentage of days with a detection") +
  facet_wrap(~dist_class)

df_sync_hour %>%
  filter(distance != 0) %>%
  mutate(noise_class = as.factor(cut(ts_noise_rec, breaks = 15))) %>% 
  ggplot(aes(fill = factor(det_bin), x = noise_class)) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_bar(position = "fill", stat = 'count') +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(values = c("rosybrown1", "indianred3"), name = '') +
  labs(x= "", y = "", title = "Percentage of days with a detection") +
  facet_wrap(~dist_class)

# Wind speed: interaction
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(wind_speed_median, perc_det)) +
  geom_point(size = 0.1) +
  facet_wrap(~dist_class)


df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_speed_median, breaks = 15))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_wind_distance_day.jpg"), width = 16, height = 10)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_speed_median, breaks = 15))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_speed_median, breaks = 15))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_wind_distance_hour.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_speed_median, breaks = 15))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot()


# Wind dir: no interaction. Hardly an effect
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(wind_azimuth, perc_det)) +
  geom_point(size = 0.1) +
  facet_wrap(~dist_class)


df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_azimuth, breaks = 10))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_windaz_distance_day.jpg"), width = 16, height = 10)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_azimuth, breaks = 10))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_azimuth, breaks = 10))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_windaz_distance_hour.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(wind_class = as.factor(cut(wind_azimuth, breaks = 10))) %>% 
  ggplot(aes(wind_class, perc_det)) +
  geom_boxplot()

# current dir: might be an interaction
df_sync_hour %>% 
  filter(distance != 0) %>% 
  ggplot(aes(cur_azimuth, perc_det)) +
  geom_point(size = 0.1) +
  facet_wrap(~dist_class)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  filter(!is.na(cur_azimuth)) %>% 
  mutate(cur_azimuth_class = as.factor(cut(cur_azimuth, breaks = seq(-0, 180, 30)))) %>% 
  ggplot(aes(cur_azimuth_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_curaz_distance_hour.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  filter(!is.na(cur_azimuth)) %>% 
  mutate(cur_azimuth_class = as.factor(cut(cur_azimuth, breaks = seq(-0, 180, 30)))) %>% 
  ggplot(aes(cur_azimuth_class, perc_det)) +
  geom_boxplot()


# current speed: no real interaction
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(cur_speed_bot_median, perc_det)) +
  geom_point(size = 0.1) +
  facet_wrap(~dist_class)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(cur_speed_class = as.factor(cut(cur_speed_bot_median, breaks = 10))) %>% 
  ggplot(aes(cur_speed_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_cur_distance_day.jpg"), width = 16, height = 10)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(cur_speed_class = as.factor(cut(cur_speed_bot_median, breaks = 10))) %>% 
  ggplot(aes(cur_speed_class, perc_det)) +
  geom_boxplot()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(cur_speed_class = as.factor(cut(cur_speed_bot, breaks = 10))) %>% 
  ggplot(aes(cur_speed_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_cur_distance_hour.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(cur_speed_class = as.factor(cut(cur_speed_bot, breaks = 10))) %>% 
  ggplot(aes(cur_speed_class, perc_det)) +
  geom_boxplot()


# Temperature: no interaction. Hardly a relationship?
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(ts_temp_median, perc_det)) +
  geom_point(size = 0.1)
df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(temp_class = as.factor(cut(ts_temp_median, breaks = 10))) %>% 
  ggplot(aes(temp_class, perc_det)) +
  geom_boxplot()
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_temperature_day.jpg"), width = 16, height = 10)
df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(temp_class = as.factor(cut(ts_temp_median, breaks = 10))) %>% 
  ggplot(aes(temp_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)



df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(temp_class = as.factor(cut(ts_temp_median, breaks = 10))) %>% 
  ggplot(aes(temp_class, perc_det)) +
  geom_boxplot()

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(temp_class = as.factor(cut(ts_temp, breaks = 10))) %>% 
  ggplot(aes(temp_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(cur_speed_class = as.factor(cut(cur_speed_bot, breaks = 10))) %>% 
  ggplot(aes(cur_speed_class, perc_det)) +
  geom_boxplot()


# Tilt

df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(recedf_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(tilt_class = as.factor(cut(ts_tilt_median, breaks = 10))) %>% 
  ggplot(aes(tilt_class, perc_det)) +
  geom_boxplot()iver_id, ts_tilt_median)) +
  geom_boxplot()
# ggsave(filename = paste0("reports/figures/exploration/independent_var/tilt_receiver.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(tilt_class = as.factor(cut(ts_tilt, breaks = 10))) %>% 
  ggplot(aes(tilt_class, perc_det)) +
  geom_boxplot()
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_tilt_hour.jpg"), width = 16, height = 10)


df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(tilt_class = as.factor(cut(ts_tilt_median, breaks = 10))) %>% 
  ggplot(aes(tilt_class, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(tilt_class = as.factor(cut(ts_tilt_median, breaks = 10))) %>% 
  ggplot(aes(tilt_class, perc_det)) +
  geom_boxplot()
# Days since
df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(days_clas = as.factor(cut(days_since_deploy, breaks = 15))) %>% 
  ggplot(aes(days_clas, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(days_clas = as.factor(cut(days_since_deploy, breaks = 15))) %>% 
  ggplot(aes(days_clas, perc_det)) +
  geom_boxplot()
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_days.jpg"), width = 16, height = 10)

df_sync_hour %>% 
  filter(distance != 0) %>% 
  mutate(days_clas = as.factor(cut(days_since_deploy, breaks = 15))) %>% 
  ggplot(aes(days_clas, perc_det)) +
  geom_boxplot()
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_days_hour.jpg"), width = 16, height = 10)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(days_clas = as.factor(cut(days_since_deploy, breaks = 15))) %>% 
  ggplot(aes(days_clas, perc_det)) +
  geom_boxplot()

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(days_clas = as.factor(cut(days_since_deploy, breaks = 15))) %>% 
  ggplot(aes(days_clas, perc_det)) +
  geom_boxplot() +
  facet_wrap(~cut(ts_temp_median, breaks = 5))

# receiver
df_sync_day %>% 
  filter(distance != 0) %>% 
  ggplot(aes(receiver_id, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_receiver_distance_day.jpg"), width = 16, height = 10)

df_sync_day %>% 
  filter(distance != 0) %>% 
  mutate(tilt_class = as.factor(cut(ts_tilt_median, breaks = 10))) %>% 
  ggplot(aes(receiver_id , perc_det)) +
  geom_boxplot() +
  facet_wrap(~tilt_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/tilt_receiver_percentageday.jpg"), width = 16, height = 10)


df_sync_hour %>% 
  filter(distance != 0) %>% 
  ggplot(aes(receiver_id, perc_det)) +
  geom_boxplot() +
  facet_wrap(~dist_class)
# ggsave(filename = paste0("reports/figures/exploration/relationships/relation_receiver_distance_hour.jpg"), width = 16, height = 10)


#### Step 7: Interactions ####
# Expect: yes
# For sure distance with noise

#### Step 8: Autocorrelation ####
# Autocorrelation in independent variables will make autocorrelation in dependent variable impossible to see
# Only autocorrelation that could be inherent to station/receiver is biofouling / technical failure
