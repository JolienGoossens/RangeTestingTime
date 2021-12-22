
###########################################
#  Model validation for daily resolution  #
###########################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(ggpubr)
Sys.setlocale("LC_ALL", "English")

#### Get data ####
df_model_day <- read_csv("data/interim/df_model_day.csv")

#### Set model as selected ####
model_formula_day <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st * transmit_power_output +
  distance.st * ts_noise_rec_median.st

glm_day <- glm(model_formula_day, family = "binomial", data = df_model_day)

#### Summary information model ####
summary(glm_day)
drop1(glm_day, test = "LRT")
car::vif(glm_day)

#### Plot residuals ####
df_model_day <- df_model_day %>% 
  mutate(
    model_fit = as.numeric(glm_day$fitted.values),
    model_resid_dev = as.numeric(resid(glm_day, type = "deviance")),
    model_resid_pear = as.numeric(resid(glm_day, type = "pearson")))%>% 
  as.data.frame() 

# Fit vs resid
p1 = df_model_day %>% 
  ggplot(aes(cut(model_fit, breaks = 10), model_resid_dev)) +
  geom_boxplot()
p2 = df_model_day %>% 
  ggplot(aes(cut(model_fit, breaks = 10), model_resid_pear)) +
  geom_boxplot()
ggarrange(p1, p2, nrow = 2)
ggsave(filename = paste0("reports/figures/model/validation/residuals_boxplot_day.jpg"), width = 10, height = 7)

# Resid over time
df_model_day %>% 
  mutate(datetime_date = lubridate::parse_date_time(datetime_date, orders = "ymd"),
         datetime_month = lubridate::month(datetime_date)) %>% 
  ggplot(aes(x = as.factor(datetime_month), y = model_resid_pear)) +
  geom_boxplot()

#### Check autocorrelation #### 
lapply(unique(df_model_day$receiver_id), function(rec_id){
  df_acf = df_model_day %>% filter(receiver_id == rec_id)
  acf(df_acf$model_resid_dev)
})
# Some autocorrelation structure related to macrotides, but low enough to be ignored
