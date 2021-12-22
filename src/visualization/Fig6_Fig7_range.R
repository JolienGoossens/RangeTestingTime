
##############################
# Plot range for different n #
##############################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(ggpubr)
library(patchwork)

#### Get data ####
df_model_day <- read_csv("data/interim/df_model_day.csv")
df_model_hour <- read_csv("data/interim/df_model_hour.csv")

#### Settings ####
zero_threshold = 0.05
k = 2

#### Plot settings ####
label_hour = c(
  '0' = expression(paste(pi["hour"])),
  '5' = expression(paste("P"["hour"],"(n=5)")),
  '10' = expression(paste("P"["hour"],"(n=10)")),
  '20' = expression(paste("P"["hour"],"(n=20)")))

label_day = c(
  '0' = expression(paste(pi["day"])),
  '10' = expression(paste("P"["day"],"(n=10)")),
  '20' = expression(paste("P"["day"],"(n=20)")),
  '60' = expression(paste("P"["day"],"(n=60)")))

col_hour = c(
  '0' = "gray10",
  '5' = "#615C54",
  '10' = "#948D81",
  '20' = "#D9CEBD"
)

col_day = c(
  '0' = "gray10",
  '10' = "#615C54",
  '20' = "#948D81",
  '60' = "#D9CEBD"
)


#### Make functions ####
MyStd2 <- function(x, x_ref) {(x - mean(x_ref))/sd(x_ref)}
MyStd2_inv <- function(std, x_ref) {sd(x_ref)*std + mean(x_ref)}

# Function for root finding
findInt <- function(model, value, temp_res, transmit_value) {
  if (temp_res == "hour") {
    function(x) {
      predict(model, 
              data.frame(
                distance.st=x, 
                transmit_power_output = transmit_value,
                ts_noise_rec.st = unique(newdata_hour_cdf$ts_noise_rec.st)),
              type="response") - value
    }}
  else if (temp_res == "day") {
    function(x) {
      predict(model, 
              data.frame(
                distance.st=x, 
                transmit_power_output = transmit_value,
                ts_noise_rec_median.st = unique(newdata_day_cdf$ts_noise_rec_median.st)),
              type="response") - value
    }}
}


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

#### Dataset prediction pbinom: hour ####
newdata_hour_cdf <- expand.grid(
  distance = seq(0,1100, by = 50),
  ts_noise_rec = median(df_model_hour$ts_noise_rec),
  transmit_power_output = c("High", "Low"),
  n_value = c(0, 5, 10, 20))

# Formatting
newdata_hour_cdf <- newdata_hour_cdf %>% 
  mutate( # Standardize
    distance.st = MyStd2(distance, df_model_hour$distance),
    ts_noise_rec.st = MyStd2(ts_noise_rec, df_model_hour$ts_noise_rec))

newdata_hour_cdf <- newdata_hour_cdf %>% 
  # Set pi and pi zero
  mutate(model_pred = as.numeric(predict(glm_hour, newdata= newdata_hour_cdf, type = "response"))) %>% 
  mutate(model_pred_zero = ifelse(model_pred < zero_threshold, 0, (model_pred - zero_threshold)/(1- zero_threshold))) %>%
  mutate(model_pred_zero = ifelse(n_value == 0, model_pred, model_pred_zero)) %>% 
  # Calculate pcum
  mutate(cum_prob = ifelse(n_value == 0, model_pred, 
                           pbinom(k-1, n_value, prob = model_pred_zero, lower.tail = F))) %>% 
  # Set n as a factor
  mutate(n_value = as.factor(n_value))


#### Dataset prediction pbinom: day ####
newdata_day_cdf <- expand.grid(
  distance = seq(0,1100, by = 50),
  ts_noise_rec_median = median(df_model_day$ts_noise_rec_median),
  transmit_power_output = c("High", "Low"),
  n_value = c(0, 10, 20, 60))

# Formatting
newdata_day_cdf <- newdata_day_cdf %>% 
  mutate(# Standardize
    distance.st = MyStd2(distance, df_model_day$distance),
    ts_noise_rec_median.st = MyStd2(ts_noise_rec_median, df_model_day$ts_noise_rec_median))

newdata_day_cdf <- newdata_day_cdf %>% 
  # Set pi and pi zero
  mutate(model_pred = as.numeric(predict(glm_day, newdata= newdata_day_cdf, type = "response"))) %>% 
  mutate(model_pred_zero = ifelse(model_pred < zero_threshold, 0, (model_pred - zero_threshold)/(1- zero_threshold))) %>%
  mutate(model_pred_zero = ifelse(n_value == 0, model_pred, model_pred_zero)) %>% 
  # Calculate pcum
  mutate(cum_prob = ifelse(n_value == 0, model_pred, 
                           pbinom(k-1, n_value, prob = model_pred_zero, lower.tail = F))) %>%
  # Set n as a factor
  mutate(n_value = as.factor(n_value))


#### Calculate D50  ####
# Make data frames
dfd50_hour <- expand.grid(
  n_value = c(5, 10, 20),
  transmit_power_output = c("High", "Low"))
dfd50_day <- expand.grid(
  n_value = c(10, 20, 60),
  transmit_power_output = c("High", "Low"))

# Calculate D50 hour
list_dfd50_hour <- lapply(unique(dfd50_hour$transmit_power_output), function(transmit_value){
  list_transmit = lapply(unique(dfd50_hour$n_value), function(n){
    df_temp = dfd50_hour %>% 
      filter(transmit_power_output == transmit_value & n_value == n) %>% 
      mutate(d50.st = uniroot(findInt(glm_hour, .5, "hour", transmit_value), range(newdata_hour_cdf$distance.st))$root) %>% 
      mutate(d50 = MyStd2_inv(d50.st, df_model_hour$distance)) %>% 
      mutate(model_predzero = uniroot(function(p) pbinom(k-1, n_value, p, lower.tail = F) - 0.5, c(0, 1), 
                                      tol = .Machine$double.eps)$root) %>% 
      mutate(model_pred = (1- zero_threshold)*model_predzero + zero_threshold) %>% 
      mutate(d50cum.st = uniroot(findInt(glm_hour, model_pred, "hour", transmit_value), range(newdata_hour_cdf$distance.st))$root) %>% 
      mutate(d50cum = MyStd2_inv(d50cum.st, df_model_hour$distance))
  })
  df_transmit = plyr::ldply(list_transmit)
  return(df_transmit)
})
dfd50_hour <- rbind(plyr::ldply(list_dfd50_hour))

# Add D50 of pi
dfd50_hour <- dfd50_hour %>% 
  group_by(transmit_power_output) %>% 
  summarise(d50 = unique(d50),
            d50cum = unique(d50)) %>% 
  mutate(n_value = 0) %>% 
  full_join(dfd50_hour)

dfd50_hour <- dfd50_hour %>% 
  mutate(n_value = as.factor(n_value))

# Calculate D50 day
list_dfd50_day <- lapply(unique(dfd50_day$transmit_power_output), function(transmit_value){
  list_transmit = lapply(unique(dfd50_day$n_value), function(n){
    df_temp = dfd50_day %>% 
      filter(transmit_power_output == transmit_value & n_value == n) %>% 
      mutate(d50.st = uniroot(findInt(glm_day, .5, "day", transmit_value), 
                              range(newdata_day_cdf$distance.st))$root) %>% 
      mutate(d50 = MyStd2_inv(d50.st, df_model_day$distance)) %>% 
      mutate(model_predzero = uniroot(function(p) pbinom(k-1, n_value, p, lower.tail = F) - 0.5, c(0, 1), 
                                      tol = .Machine$double.eps)$root) %>% 
      mutate(model_pred = (1- zero_threshold)*model_predzero + zero_threshold) %>% 
      mutate(d50cum.st = uniroot(findInt(glm_day, model_pred, "day", transmit_value), 
                                 range(newdata_day_cdf$distance.st))$root) %>% 
      mutate(d50cum = MyStd2_inv(d50cum.st, df_model_day$distance))
  })
  df_transmit = plyr::ldply(list_transmit)
  return(df_transmit)
})
dfd50_day <- rbind(plyr::ldply(list_dfd50_day))

# Add D50 of pi
dfd50_day <- dfd50_day %>% 
  group_by(transmit_power_output) %>% 
  summarise(d50 = unique(d50),
            d50cum = unique(d50)) %>% 
  mutate(n_value = 0) %>% 
  full_join(dfd50_day)

dfd50_day <- dfd50_day %>% 
  mutate(n_value = as.factor(n_value))

#### Plot prediction pbinom: hour ####
phour = newdata_hour_cdf %>% 
  filter(transmit_power_output == "High") %>% 
  ggplot(aes(x=distance)) +
  theme_bw() +
  theme(legend.text = element_text(hjust = 0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_tile(data = data.frame(prob =  seq(0,1, by = 0.025)),
            aes(x = 550, y = prob, fill = prob), width = 1100) +
  scale_fill_gradient2(midpoint = 0.5, low ="#348499", mid = "gray95", high = "#E6301C") +
  geom_hline(yintercept = 0.5, colour = "white", size = 1) +
  geom_vline(xintercept = seq(200, 1000, 200), alpha= 0.2) +
  geom_path(aes(y = cum_prob, colour = n_value, group = n_value), size = 1, alpha = 0.7) +
  scale_x_continuous(breaks = seq(200, 1000, 200), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(labels = label_hour, values = col_hour) +
  guides(colour =guide_legend(title=NULL), fill = "none") +
  labs(x = "", y = "Predicted probability")

#### Plot prediction pbinom: day ####
pday = newdata_day_cdf %>% 
  filter(transmit_power_output == 'High') %>% 
  ggplot(aes(x=distance)) +
  theme_bw() +
  theme(legend.text = element_text(hjust = 0)) +
  geom_tile(data = data.frame(prob =  seq(0,1, by = 0.025)),
            aes(x = 550, y = prob, fill = prob), width = 1100) +
  scale_fill_gradient2(midpoint = 0.5, low ="#348499", mid = "gray95", high = "#E6301C") +
  geom_hline(yintercept = 0.5, colour = "white", size = 1) +
  geom_vline(xintercept = seq(200, 1000, 200), alpha= 0.2) +
  geom_path(aes(y = cum_prob, colour = n_value, group = n_value), size = 1, alpha = 0.7) +
  scale_x_continuous(breaks = seq(200, 1000, 200), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_manual(labels = label_day, values = col_day) +
  guides(colour =guide_legend(title=NULL), fill = "none") +
  labs(x = "Distance (m)", y = "Predicted probability")


#### Plot circle prediction pbinom: hour ####
pcirc_hour = newdata_hour_cdf %>% 
  filter(transmit_power_output == "High") %>% 
  ggplot() +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        strip.text = element_blank()) +
  geom_tile(aes(1, distance, fill = cum_prob)) +
  geom_hline(data = filter(dfd50_hour, transmit_power_output == "High"), aes(yintercept = d50cum),
             colour = "white", size = 1) +
  geom_hline(yintercept = seq(200, 1000, by = 200), alpha = 0.2) +
  geom_text(data = data.frame(x =1, y = seq(200,1000, by = 200)),
            aes(x, y, label = y), alpha = 0.4) +
  geom_text(data = filter(dfd50_hour, transmit_power_output == "High"),
            aes(0.65, d50cum, label = paste0(round(d50cum, digits = 0), " m")),
            colour = "white", nudge_y = 275, fontface = "bold") +
  scale_fill_gradient2(midpoint = 0.5, low ="#348499", mid = "gray95", high = "#E6301C") +
  coord_polar() +
  facet_wrap(~n_value,nrow = 1)


#### Plot circle prediction pbinom: day ####
pcirc_day = newdata_day_cdf %>% 
  filter(transmit_power_output == "High") %>% 
  ggplot() +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = 'none',
        strip.text = element_blank()) +
  geom_tile(aes(1, distance, fill = cum_prob)) +
  geom_hline(data = filter(dfd50_day, transmit_power_output == "High"), aes(yintercept = d50cum),
             colour = "white", size = 1) +
  geom_hline(yintercept = seq(200, 1000, by = 200), alpha = 0.2) +
  geom_text(data = data.frame(x =1, y = seq(200,1000, by = 200)),
            aes(x, y, label = y), alpha = 0.4) +
  geom_text(data = filter(dfd50_day, transmit_power_output == "High"),
            aes(0.65, d50cum, label = paste0(round(d50cum, digits = 0), " m")),
            colour = "white", nudge_y = 275, fontface = "bold") +
  scale_fill_gradient2(midpoint = 0.5, low ="#348499", mid = "gray95", high = "#E6301C") +
  coord_polar() +
  facet_wrap(~n_value,nrow = 1)

#### Combine ####
ptot_hour = ggarrange(pcirc_hour, phour, nrow = 2)
ptot_day = ggarrange(pcirc_day, pday, nrow = 2)
ptot_circ = ggarrange(pcirc_hour, pcirc_day, nrow = 2)
ptot_path = ggarrange(phour, pday, nrow = 2)

#### Save ####
ggsave(filename = "reports/figures/Fig6_circtot.jpg", plot = ptot_circ,
       scale = 1, dpi = 600, width = 28, height = 16, units = "cm", bg = "white")
ggsave(filename = "reports/figures/Fig6_ptot_path.jpg", plot = ptot_path,
       scale = 1, dpi = 600, width = 23, height = 10, units = "cm", bg = "white")
