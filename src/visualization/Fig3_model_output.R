
##############################
# Plot binomial model output #
##############################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(patchwork)

#### Get data ####
df_model_day <- read_csv("data/interim/df_model_day.csv")
df_model_hour <- read_csv("data/interim/df_model_hour.csv")

#### Settings ####
zero_threshold = 0.05
k = 2

cols = c(
  "Low" = "#2b8cbe",
  "High" = "#8856a7"
)

#### Make functions ####
# Standardize
MyStd2 <- function(x, x_ref) {(x - mean(x_ref))/sd(x_ref)}
# Standardize parallel to standardization used in model
MyStd2_inv <- function(std, x_ref) {sd(x_ref)*std + mean(x_ref)}

#### Set model as selected ####
model_formula_day <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st * transmit_power_output +
  distance.st * ts_noise_rec_median.st

glm_day <- glm(model_formula_day, family = "binomial", data = df_model_day)

model_formula_hour <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st *  transmit_power_output +
  distance.st * ts_noise_rec.st 

glm_hour <- glm(model_formula_hour, family = "binomial", data = df_model_hour)

#### Prediction pbinom: hour ####
newdata_hour <- expand.grid(
  distance = seq(125,1125, by = 50),
  ts_noise_rec = c(min(df_model_hour$ts_noise_rec), median(df_model_hour$ts_noise_rec), max(df_model_hour$ts_noise_rec)),
  transmit_power_output = c("High", "Low"))

# Formatting
newdata_hour <- newdata_hour %>% 
  mutate( # Standardize
    distance.st = MyStd2(distance, df_model_hour$distance),
    ts_noise_rec.st = MyStd2(ts_noise_rec, df_model_hour$ts_noise_rec))

newdata_hour <- newdata_hour %>% 
  mutate(model_pred = as.numeric(predict(glm_hour, newdata= newdata_hour, type = "response")))


#### Prediction pbinom: day ####
newdata_day <- expand.grid(
  distance = seq(125,1125, by = 50),
  ts_noise_rec_median = c(min(df_model_day$ts_noise_rec_median), 
                          median(df_model_day$ts_noise_rec_median),
                          max(df_model_day$ts_noise_rec_median)),
  transmit_power_output = c("High", "Low"),
  n_value = c(0, 10, 20, 60))

# Formatting
newdata_day <- newdata_day %>% 
  mutate(# Standardize
    distance.st = MyStd2(distance, df_model_day$distance),
    ts_noise_rec_median.st = MyStd2(ts_noise_rec_median, df_model_day$ts_noise_rec_median))

newdata_day <- newdata_day %>% 
  mutate(model_pred = as.numeric(predict(glm_day, newdata= newdata_day, type = "response")))

#### Prediction & classification P hour ####
# Prediction
df_model_hour <- df_model_hour %>% 
  # Set pi and pi zero
  mutate(model_pred = as.numeric(predict(glm_hour, newdata= df_model_hour, type = "response"))) %>% 
  mutate(model_pred_zero = ifelse(model_pred < zero_threshold, 0, (model_pred - zero_threshold)/(1- zero_threshold))) %>%
  # Calculate pcum
  mutate(cum_prob = pbinom(k-1, maxi, prob = model_pred_zero, lower.tail = F))

# Classification
df_model_hour <- df_model_hour %>%
  mutate(obs_P = ifelse(n_det <= k, 0,1),
         pred_P = ifelse(cum_prob < 0.5, 0, 1))


#### Prediction & classification P day ####
# Prediction
df_model_day <- df_model_day %>% 
  # Set pi and pi zero
  mutate(model_pred = as.numeric(predict(glm_day, newdata= df_model_day, type = "response"))) %>% 
  mutate(model_pred_zero = ifelse(model_pred < zero_threshold, 0, (model_pred - zero_threshold)/(1- zero_threshold))) %>%
  # Calculate pcum
  mutate(cum_prob = pbinom(k-1, maxi, prob = model_pred_zero, lower.tail = F))

# Classification
df_model_day <- df_model_day %>%
  mutate(obs_P = ifelse(n_det <= k, 0,1),
         pred_P = ifelse(cum_prob < 0.5, 0, 1))


#### Plots of binomial model prediction ####
phour = newdata_hour %>% 
  mutate(transmit_power_output = factor(transmit_power_output, levels = c("Low", "High"))) %>% 
  group_by(distance, transmit_power_output) %>% 
  summarise(pred_min = min(model_pred),
            pred_median = median(model_pred),
            pred_max = max(model_pred)) %>% 
  ggplot(aes(x = distance)) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        # legend.position = c(0.9, 0.9),
        legend.position = "none",
        axis.text= element_blank()
        # legend.key.size = unit(1, 'cm'),
        # legend.text = element_text(size=10)
        ) +
  geom_line(aes(y = pred_median, colour = transmit_power_output)) +
  geom_ribbon(aes(ymin = pred_min, ymax = pred_max,
                  fill = transmit_power_output),
              alpha = 0.6) +
  scale_x_continuous(expand = c(0,0), limits = c(100,1100),
                     breaks = seq(200, 1000, by = 200)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  # labs(x = "", y = expression(paste(pi["hour"]))) +
  labs(x = "", y ="") +
  guides(fill = guide_legend(label.position = "left"))


pday = newdata_day %>% 
  mutate(transmit_power_output = factor(transmit_power_output, levels = c("Low", "High"))) %>% 
  group_by(distance, transmit_power_output) %>% 
  summarise(pred_min = min(model_pred),
            pred_median = median(model_pred),
            pred_max = max(model_pred)) %>% 
  ggplot(aes(x = distance)) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.text= element_blank(),
        legend.position = "none") +
  geom_line(aes(y = pred_median, colour = transmit_power_output)) +
  geom_ribbon(aes(ymin = pred_min, ymax = pred_max,
                  fill = transmit_power_output),
              alpha = 0.6) +
  scale_x_continuous(expand = c(0,0), limits = c(100,1100),
                     breaks = seq(200, 1000, by = 200)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  labs(x = "", y ="")
  # labs(x = "", y = expression(paste(pi["day"])))
  


#### Plot classifications ####
phigh_day = df_model_day %>% 
  filter(transmit_power_output == "High") %>% 
  gather(type, P, obs_P:pred_P) %>% 
  mutate(dist_class = cut(distance, breaks = seq(100, 1100, by = 100))) %>%
  # mutate(dist_class = factor(dist_class, labels = paste(seq(100, 1000, by = 100), seq(200, 1100, by = 100), sep = "-"))) %>% 
  mutate(dist_class = factor(dist_class, labels = seq(200, 1100, by = 100))) %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text= element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +
  geom_bar(aes(x = type, fill = as.factor(P) , alpha = as.factor(type)), 
            position = "fill", colour = "black") +
  scale_fill_manual(values = c("1" = "#8856a7", "0" = "white")) +
  scale_x_discrete(labels = c("Obs", "Pred"), expand = c(0,0), position = "top") +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_manual(values = cols) +
  scale_alpha_manual(values = c(0.9,0.5)) +
  facet_wrap(~dist_class, nrow = 1) +
  labs(x = "", y ="")

plow_day = df_model_day %>% 
  filter(transmit_power_output == "Low") %>% 
  gather(type, P, obs_P:pred_P) %>% 
  mutate(dist_class = cut(distance, breaks = seq(100, 1100, by = 100))) %>%
  # mutate(dist_class = factor(dist_class, labels = paste(seq(100, 1000, by = 100), seq(200, 1100, by = 100), sep = "-"))) %>% 
  mutate(dist_class = factor(dist_class, labels = seq(200, 1100, by = 100))) %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text= element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +
  geom_bar(aes(x = type, fill = as.factor(P) , alpha = as.factor(type)), 
           position = "fill", colour = "black") +
  scale_fill_manual(values = c("1" = "#2b8cbe", "0" = "white")) +
  scale_x_discrete(labels = c("Obs", "Pred"), expand = c(0,0), position = "top") +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_manual(values = cols) +
  scale_alpha_manual(values = c(0.9,0.5)) +
  facet_wrap(~dist_class, nrow = 1) +
  labs(x = "", y ="")
  

phigh_hour = df_model_hour %>% 
  filter(transmit_power_output == "High") %>% 
  gather(type, P, obs_P:pred_P) %>% 
  mutate(dist_class = cut(distance, breaks = seq(100, 1100, by = 100))) %>%
  # mutate(dist_class = factor(dist_class, labels = paste(seq(100, 1000, by = 100), seq(200, 1100, by = 100), sep = "-"))) %>% 
  mutate(dist_class = factor(dist_class, labels = seq(200, 1100, by = 100))) %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text= element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +
  geom_bar(aes(x = type, fill = as.factor(P) , alpha = as.factor(type)), 
           position = "fill", colour = "black") +
  scale_fill_manual(values = c("1" = "#8856a7", "0" = "white")) +
  scale_x_discrete(labels = c("Obs", "Pred"), expand = c(0,0), position = "top") +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_manual(values = cols) +
  scale_alpha_manual(values = c(0.9,0.5)) +
  facet_wrap(~dist_class, nrow = 1) +
  labs(x = "", y ="")

plow_hour = df_model_hour %>% 
  filter(transmit_power_output == "Low") %>% 
  gather(type, P, obs_P:pred_P) %>% 
  mutate(dist_class = cut(distance, breaks = seq(100, 1100, by = 100))) %>%
  # mutate(dist_class = factor(dist_class, labels = paste(seq(100, 1000, by = 100), seq(200, 1100, by = 100), sep = "-"))) %>% 
  mutate(dist_class = factor(dist_class, labels = seq(200, 1100, by = 100))) %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text= element_blank(),
        strip.text = element_blank(),
        legend.position = "none") +
  geom_bar(aes(x = type, fill = as.factor(P) , alpha = as.factor(type)), 
           position = "fill", colour = "black") +
  scale_fill_manual(values = c("1" = "#2b8cbe", "0" = "white")) +
  scale_x_discrete(labels = c("Obs", "Pred"), expand = c(0,0), position = "top") +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_manual(values = cols) +
  scale_alpha_manual(values = c(0.9,0.5)) +
  facet_wrap(~dist_class, nrow = 1) +
  labs(x = "", y ="")

#### Combine plots ####
ptot = (phour | plow_hour | phigh_hour) / (pday | plow_day | phigh_day)

#### Save ####
ggsave(filename = "reports/figures/Fig3_model.jpg", plot = ptot,
       scale = 1, dpi = 600, width = 40, height = 16, units = "cm")
