
############################################
#  Model validation for hourly resolution  #
############################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(ggpubr)
Sys.setlocale("LC_ALL", "English")

#### Get data ####
df_model_hour <- read_csv("data/interim/df_model_hour.csv")


#### Set model as selected ####
model_formula_hour <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st *  transmit_power_output +
  distance.st * ts_noise_rec.st 

glm_hour <- glm(model_formula_hour, family = "binomial", data = df_model_hour)

#### Summary information model ####
summary(glm_hour)
drop1(glm_hour, test = "LRT")
car::vif(glm_hour)

#### Plot residuals ####
df_model_hour <- df_model_hour %>% 
  mutate(
    model_fit = as.numeric(glm_hour$fitted.values),
    model_resid_dev = as.numeric(resid(glm_hour, type = "deviance")),
    model_resid_pear = as.numeric(resid(glm_hour, type = "pearson")))%>% 
  as.data.frame() 

p1 = df_model_hour %>% 
  ggplot(aes(cut(model_fit, breaks = 10), model_resid_dev)) +
  geom_boxplot()
p2 = df_model_hour %>% 
  ggplot(aes(cut(model_fit, breaks = 10), model_resid_pear)) +
  geom_boxplot()
ggarrange(p1, p2, nrow = 2)
ggsave(filename = paste0("reports/figures/model/validation/residuals_boxplot_hour.jpg"), width = 10, height = 7)

# Resid over time
df_model_hour %>% 
  mutate(datetime_hour = lubridate::parse_date_time(datetime_hour, orders = "ymd HMS"),
         datetime_month = lubridate::month(datetime_hour)) %>% 
  ggplot(aes(x = as.factor(datetime_month), y = model_resid_pear)) +
  geom_boxplot()

#### Check autocorrelation #### 
lapply(unique(df_model_hour$receiver_id), function(rec_id){
  df_acf = df_model_hour %>% filter(receiver_id == rec_id)
  acf(df_acf$model_resid_dev)
})
# Still some autocorrelation structure related to tides (however: rarely under threshold value)
