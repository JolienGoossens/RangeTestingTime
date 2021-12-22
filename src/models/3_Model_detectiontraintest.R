
########################################################################
#  Assess range assessment scenarios with training and test data sets  #
########################################################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(pROC)
library(lubridate)
Sys.setlocale("LC_ALL", "English")

#### Get data ####
df_model_day <- read_csv("data/interim/df_model_day.csv")
df_model_hour <- read_csv("data/interim/df_model_hour.csv")
deploy <- read_csv("data/interim/deploy.csv")
df_dist <- read_csv("data/interim/df_dist.csv")

date_Lowtohigh = date("2020-06-16")

#### Formatting + reduce columns datasets ####
df_model_hour <- df_model_hour %>% 
  mutate(datetime_date = lubridate::parse_date_time(date(datetime_hour), orders = "ymd"))

deploy <- deploy %>%
  mutate(location_type = ifelse(turbine %in% c("DE89", "CD78", "BC89"), "sand", "turbine")) %>% 
  select(receiver_id, station_name, transmit_power_output, built_in_tag_id, location_type)
  
df_dist <- df_dist %>% select(receiver_id, transmitter, distance)

#### Model formulae ####
model_formula_hour <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2)+
  distance.st +
  distance.st *  transmit_power_output +
  distance.st * ts_noise_rec.st 

model_formula_hour_nopower <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2) +
  distance.st +
  distance.st * ts_noise_rec.st 

model_formula_day <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2)+
  distance.st +
  distance.st * transmit_power_output +
  distance.st * ts_noise_rec_median.st

model_formula_day_nopower <- cbind(n_det, maxi-n_det) ~ 
  I(distance.st^2)+
  distance.st +
  distance.st * ts_noise_rec_median.st

#### Make general functions ####
MyStd2 <- function(x, x_ref) {(x - mean(x_ref))/sd(x_ref)}

make_subset_train <- function(df_model, include_range = FALSE, station_filter = NULL, deploy = NULL) {
  df_model_train = df_model
  if (!is.null(station_filter)) {
    df_model_train = df_model_train %>% filter(datetime_date > date_Lowtohigh)
    transid = deploy %>% filter(station_name %in% station_filter) %>% distinct(built_in_tag_id) %>% pull()
    df_model_train = df_model_train %>%  filter(transmitter %in% transid)
    if (include_range == TRUE) {
      df_model_range = df_model %>% filter(datetime_date < date_Lowtohigh)
      df_model_train = rbind(df_model_range, df_model_train)
    }
  } else if (include_range == TRUE) {
    df_model_train = df_model %>% filter(datetime_date < date_Lowtohigh)
  }
  return(df_model_train)
}

make_subset_test <- function(df_model, station_filter = NULL, deploy = NULL) {
  df_model_test = df_model %>%  filter(datetime_date > date_Lowtohigh)
  if (!is.null(station_filter)) {
    transid = deploy %>% filter(station_name %in% station_filter) %>% distinct(built_in_tag_id) %>% pull()
    df_model_test = df_model_test %>% filter(!transmitter %in% transid)
  }
  return(df_model_test)
}

make_subset_range_train <- function(df_model, station_filter = NULL, deploy = NULL, n_days = 32) {
  df_model_train = df_model
  df_model_train = df_model %>% 
    filter(datetime_date < date_Lowtohigh) %>% 
    filter(datetime_date >= (date_Lowtohigh - days(1 + n_days)))
  transid = deploy %>% filter(station_name %in% station_filter) %>% distinct(built_in_tag_id) %>% pull()
  recid = deploy %>% filter(station_name %in% station_filter) %>% distinct(receiver_id) %>% pull()
  df_model_train = df_model_train %>%  filter(transmitter %in% transid & receiver_id %in% recid)
  return(df_model_train)
}

predict_binary <- function(df_test, zero_threshold = 0) {
  df_test <- df_test %>%
    mutate(pred_binary = ifelse(pred < zero_threshold, 0, (pred - zero_threshold)/(1- zero_threshold))) %>%
    mutate(
      pred_bin_1 = as.integer(ifelse(pbinom(0, maxi, pred_binary, lower.tail = F) > 0.5, 1, 0)),
      pred_bin_2 = as.integer(ifelse(pbinom(1, maxi, pred_binary, lower.tail = F) > 0.5, 1, 0))) %>%
    mutate(
      det_bin_1 = as.integer(det_bin),
      det_bin_2 = as.integer(ifelse(n_det >= 2, 1, 0))) %>%
    as.data.frame()
  return(df_test)
}

quant_bin_all = function(df_test) {
  quant_bin1_01 = df_test %>% 
    predict_binary() %>% 
    filter(pred_bin_1 == 1 & det_bin_1 ==0)  %>% 
    summarise(quantile = scales::percent(seq(0, 1, by = 0.05)),
              pred = quantile(pred,seq(0, 1, by = 0.05))) %>% 
    pull(pred)
  
  quant_bin1_11 = df_test %>% 
    predict_binary() %>% 
    filter(pred_bin_1 == 1 & det_bin_1 ==1)  %>% 
    summarise(quantile = scales::percent(seq(0, 1, by = 0.05)),
              pred = quantile(pred,seq(0, 1, by = 0.05))) %>% 
    pull(pred)
  
  quant_bin2_01 = df_test %>% 
    predict_binary() %>% 
    filter(pred_bin_2 == 1 & det_bin_2 ==0)  %>% 
    summarise(quantile = scales::percent(seq(0, 1, by = 0.05)),
              pred = quantile(pred,seq(0, 1, by = 0.05))) %>% 
    pull(pred)
  
  quant_bin2_11 = df_test %>% 
    predict_binary() %>% 
    filter(pred_bin_2 == 1 & det_bin_2 ==1)  %>% 
    summarise(quantile = scales::percent(seq(0, 1, by = 0.05)),
              pred = quantile(pred,seq(0, 1, by = 0.05))) %>% 
    pull(pred)
  
  return(cbind(quant_bin1_01, quant_bin1_11, quant_bin2_01, quant_bin2_11))
  
}

calc_modeleval <- function(pred, perc_det, pred_bin, det, brier_reference = NULL) {
  resid = pred - perc_det
  rmse = sqrt(sum(resid^2)/length(resid))
  
  TP= table(pred_bin, det)[2,2]
  TN = table(pred_bin, det)[1,1]
  FP = table(pred_bin, det)[2,1]
  FN = table(pred_bin, det)[1,2]
  
  sensitivity = TP/(TP+FN)
  specificity = TN/(TN+FP) # most important!
  accuracy = (TN + TP)/(TN + TP + FN + FP)
  AUC = auc(roc(det, pred_bin, levels= c(0,1), direction = "<"))
  brier = mean((pred_bin - det)^2)
  if (is.null(brier_reference)) {
    return(c(RMSE = rmse, Sensitivity= sensitivity, Specificity= specificity, Accuracy= accuracy, AUC = AUC, Brier = brier))
  } else {
    brier_skill = 1 - (brier/brier_reference)
    return(c(RMSE = rmse, Sensitivity= sensitivity, Specificity= specificity, Accuracy= accuracy, AUC = AUC, Brier = brier, BSS = brier_skill))
  }
  
}

calc_modeleval_min2 = function(df_test, brier_reference = NULL) {
  calc_modeleval(df_test$pred, df_test$perc_det, df_test$pred_bin_2, df_test$det_bin_2, brier_reference)
}

calc_modeleval_all2 = function(df_test, brier_reference = NULL) {
  rbind(bin1 = calc_modeleval(df_test$pred, df_test$perc_det, 
                              df_test$pred_bin_1, df_test$det_bin_1, brier_reference),
        bin2 = calc_modeleval(df_test$pred, df_test$perc_det, 
                              df_test$pred_bin_2, df_test$det_bin_2, brier_reference))
}

runmodel_calceval <- function(df_model, formula_model, zero_threshold,
                              include_range, station_filter = NULL, deploy = NULL, 
                              brier_reference = NULL) {
  df_train = make_subset_train(df_model, include_range, station_filter, deploy)
  df_test = make_subset_test(df_model, station_filter, deploy)
  
  # Make glm based on training dataset
  glm_train = glm(formula_model, family = "binomial", data = df_train)
  
  
  # Calculate predictions for test data set
  df_test$pred = predict(glm_train, newdata = df_test, type = "response") # individual predictions
  df_test = predict_binary(df_test, zero_threshold)# binary predictions
  
  # Model evaluation
  calc_modeleval_min2(df_test, brier_reference)
}

runmodel_calceval_range <- function(df_model, formula_model, zero_threshold,
                                    station_filter = NULL, deploy = NULL, n_days = 32,
                                    brier_reference = NULL) {
  df_train = make_subset_range_train(df_model, station_filter, deploy, n_days)
  df_test = make_subset_test(df_model, deploy = deploy)
  
  # Make glm based on training dataset
  glm_train = glm(formula_model, family = "binomial", data = df_train)
  
  
  # Calculate predictions for test data set
  df_test$pred = predict(glm_train, newdata = df_test, type = "response") # individual predictions
  df_test = predict_binary(df_test, zero_threshold)# binary predictions
  
  # Model evaluation
  calc_modeleval_min2(df_test, brier_reference)
}



#### Run on full datasets: day ####
df_train = df_model_day
df_test = df_model_day
formula_model = model_formula_day

# Make glm based on training dataset
glm_train = glm(formula_model, family = "binomial", data = df_train)

# Calculate predictions for test data set
df_test$pred = predict(glm_train, df_test, type='response')
df_test = predict_binary(df_test) # binary predictions

# Confusion matrix
table(Predicted = df_test$pred_bin_1, Observed = df_test$det_bin_1) # too many false positives!
table(Predicted = predict_binary(df_test, zero_threshold = 0.05)$pred_bin_2, 
      Observed = predict_binary(df_test, zero_threshold = 0.05)$det_bin_2)

# Find a fitting threshold prediction value to equal to zero
quant_bin_all(df_test)

calc_modeleval_all2(predict_binary(df_test, zero_threshold = 0))
calc_modeleval_all2(predict_binary(df_test, zero_threshold = 0.05))

# Final model
df_test = predict_binary(df_test, zero_threshold = 0.05)
brier_ref_day = calc_modeleval_min2(df_test)["Brier"]

#### Visualize output day ####
df_test %>% 
  mutate(distance100 = cut(distance, seq(100,1100,100))) %>% 
  select(distance100, pred, perc_det, pred_binary) %>% 
  gather(key = 'perc', value = 'value', pred:pred_binary) %>% 
  ggplot(aes(x = distance100, y = value)) +
  theme_bw() +
  geom_boxplot(aes(fill = perc)) +
  labs(x = "Distance (m)", y ="")

df_test %>% 
  mutate(distance100 = cut(distance, seq(100,1100,100))) %>% 
  select(distance100, pred_bin_2, det_bin_2) %>% 
  gather(key = 'perc', value = 'value', pred_bin_2:det_bin_2) %>% 
  ggplot(aes(x = perc)) +
  theme_minimal() +
  geom_bar(aes(fill = factor(value)), stat = "count", position = 'fill') +
  facet_wrap(~distance100, nrow = 1)



#### Run on full datasets: hour ####
df_train = df_model_hour
df_test = df_model_hour
formula_model = model_formula_hour

# Make glm based on training dataset
glm_train = glm(formula_model, family = "binomial", data = df_train)

# Calculate predictions for test data set
df_test$pred = predict(glm_train, newdata = df_test, type = "response") # individual predictions
df_test = predict_binary(df_test) # binary predictions

# Confusion matrix
table(Predicted = df_test$pred_bin_1, Observed = df_test$det_bin_1) # too many false positives!
table(Predicted = df_test$pred_bin_2, Observed = df_test$det_bin_2) # too many false positives!

# Find a fitting threshold prediction value to equal to zero
quant_bin_all(df_test)

calc_modeleval_all2(predict_binary(df_test, zero_threshold = 0))
calc_modeleval_all2(predict_binary(df_test, zero_threshold = 0.05))

# Final model
df_test = predict_binary(df_test, zero_threshold = 0.05)
brier_ref_hour = calc_modeleval_min2(df_test)["Brier"]



#### Visualize output hour ####
df_test %>% 
  mutate(distance100 = cut(distance, seq(100,1100,100))) %>% 
  select(distance100, pred, perc_det, pred_binary) %>% 
  gather(key = 'perc', value = 'value', pred:pred_binary) %>% 
  ggplot(aes(x = distance100, y = value)) +
  theme_bw() +
  geom_boxplot(aes(fill = perc)) +
  labs(x = "Distance (m)", y ="")

df_test %>% 
  mutate(distance100 = cut(distance, seq(100,1100,100))) %>% 
  select(distance100, pred_bin_2, det_bin_2) %>% 
  gather(key = 'perc', value = 'value', pred_bin_2:det_bin_2) %>% 
  ggplot(aes(x = perc)) +
  theme_minimal() +
  geom_bar(aes(fill = factor(value)), stat = "count", position = 'fill') +
  facet_wrap(~distance100, nrow = 1)


#### Remove unnecessary columns ####
df_model_day <- df_model_day %>% select(-distance, -ts_noise_rec_median)
df_model_hour <- df_model_hour %>% select(-distance, -ts_noise_rec)

#### Run training and tests : 1 tag ####
list_output_1tag <- lapply(unique(deploy$station_name), function(station_1) {
  print(paste0("Starting model for ", station_1))
  transmitter_1 = deploy %>% filter(station_name == station_1) %>% pull(built_in_tag_id)
  n_300 = df_dist %>% filter(transmitter == transmitter_1 & distance < 300) %>% nrow()
  n_500 = df_dist %>% filter(transmitter == transmitter_1 & distance < 500) %>% nrow()
  n1100 = df_dist %>% filter(transmitter == transmitter_1 & distance < 1100) %>% nrow()
  
  
  df_model_sum <- data.frame(
    station_name = station_1,
    transmitter = transmitter_1,
    n_300 = n_300,
    n_500 = n_500,
    n1100 = n1100,
    location_type = deploy %>% filter(station_name == station_1) %>% pull(location_type))
  
  
  list_out_day <- lapply(c(TRUE, FALSE), function(TF) {
    if(TF == TRUE) {formula_model = model_formula_day} else {formula_model = model_formula_day_nopower}
    out_day = runmodel_calceval(df_model_day, 
                                formula_model = formula_model,
                                zero_threshold = 0.05, 
                                include_range = TF, station_filter = station_1, deploy = deploy,
                                brier_reference = brier_ref_day)
    out_day = cbind(df_model_sum, 
                    cbind(include_range = TF, include_power = TF, temp_res = "day"), 
                    as.data.frame(t(out_day)))
  })
  df_out_day = plyr::ldply(list_out_day)
  
  list_out_hour <- lapply(c(TRUE, FALSE), function(TF) {
    if(TF == TRUE) {formula_model = model_formula_hour} else {formula_model = model_formula_hour_nopower}
    out_hour = runmodel_calceval(df_model_hour,
                                 formula_model = formula_model,
                                  zero_threshold = 0.05, 
                                include_range = TF, station_filter = station_1, deploy = deploy,
                                brier_reference = brier_ref_hour)
    out_hour = cbind(df_model_sum, 
                    cbind(include_range = TF, include_power = TF, temp_res = "hour"), 
                    as.data.frame(t(out_hour)))
  })
  df_out_hour = plyr::ldply(list_out_hour)
  
  out_1tag = rbind(df_out_day, df_out_hour)
  
  return(out_1tag)
})
df_output_1tag <- plyr::ldply(list_output_1tag)
# data.table::fwrite(df_output_1tag, "data/interim/temp_traintest/df_output_1tag.csv", row.names = F)

#### Run training and tests : 2 tags ####
list_output_2tag <- lapply(1:length(combn(deploy$station_name, m= 2)[1,]), function(stationcomb_i) {
  station_1 = combn(deploy$station_name, m= 2)[1,stationcomb_i]
  station_2 = combn(deploy$station_name, m= 2)[2,stationcomb_i]
  
  print(paste0("Starting model for ", station_1, " and ", station_2))
  transmitter_1 = deploy %>% filter(station_name == station_1) %>% pull(built_in_tag_id)
  transmitter_2 = deploy %>% filter(station_name == station_2) %>% pull(built_in_tag_id)
  location_type_1 = deploy %>% filter(station_name == station_1) %>% pull(location_type)
  location_type_2 = deploy %>% filter(station_name == station_2) %>% pull(location_type)
  
  n_300 = df_dist %>% filter(transmitter %in% c(transmitter_1, transmitter_2) & distance < 300) %>% nrow()
  
  n_500 = df_dist %>% filter(transmitter %in% c(transmitter_1, transmitter_2) & distance < 500) %>% nrow()
  n1100 = df_dist %>% filter(transmitter %in% c(transmitter_1, transmitter_2) & distance < 1100) %>% nrow()
  
  df_model_sum <- data.frame(n_300 = n_300, n_500 = n_500, n1100 = n1100)
  df_model_sum <- df_model_sum %>% mutate(
    station_name = list(c(station_1, station_2)),
    transmitter = list(c(transmitter_1, transmitter_2)),
    location_type = list(c(location_type_1, location_type_2))) %>% 
    select(station_name, transmitter, n_300, n_500, n1100, location_type)
  
  
  list_out_day <- lapply(c(TRUE, FALSE), function(TF) {
    if(TF == TRUE) {formula_model = model_formula_day} else {formula_model = model_formula_day_nopower}
    out_day = runmodel_calceval(df_model_day, 
                                formula_model = formula_model,
                                zero_threshold = 0.05, 
                                include_range = TF, station_filter = c(station_1, station_2), deploy = deploy,
                                brier_reference = brier_ref_day)
    out_day = cbind(df_model_sum, 
                    cbind(include_range = TF, include_power = TF, temp_res = "day"), 
                    as.data.frame(t(out_day)))
  })
  df_out_day = plyr::ldply(list_out_day)
  
  list_out_hour <- lapply(c(TRUE, FALSE), function(TF) {
    if(TF == TRUE) {formula_model = model_formula_hour} else {formula_model = model_formula_hour_nopower}
    out_hour = runmodel_calceval(df_model_hour,
                                 formula_model = formula_model,
                                 zero_threshold = 0.05, 
                                 include_range = TF, station_filter = c(station_1, station_2), deploy = deploy,
                                 brier_reference = brier_ref_hour)
    out_hour = cbind(df_model_sum, 
                     cbind(include_range = TF, include_power = TF, temp_res = "hour"), 
                     as.data.frame(t(out_hour)))
  })
  df_out_hour = plyr::ldply(list_out_hour)
  
  
  out_2tag = rbind(df_out_day, df_out_hour)
  return(out_2tag)
})

df_output_2tag <- plyr::ldply(list_output_2tag)
# data.table::fwrite(df_output_2tag, "data/interim/temp_traintest/df_output_2tag.csv", row.names = F)

#### Run training and tests : 3 tags ####
list_output_3tag <- lapply(1:length(combn(deploy$station_name, m = 3)[1,]), function(stationcomb_i) {
  station_1 = combn(deploy$station_name, m= 3)[1,stationcomb_i]
  station_2 = combn(deploy$station_name, m= 3)[2,stationcomb_i]
  station_3 = combn(deploy$station_name, m= 3)[3,stationcomb_i]
  station_vec = c(station_1, station_2, station_3)
  
  print(paste0("Starting model for ", station_1, ", ", station_2, " and ", station_3))
  transmitter_1 = deploy %>% filter(station_name == station_1) %>% pull(built_in_tag_id)
  transmitter_2 = deploy %>% filter(station_name == station_2) %>% pull(built_in_tag_id)
  transmitter_3 = deploy %>% filter(station_name == station_3) %>% pull(built_in_tag_id)
  
  location_type_1 = deploy %>% filter(station_name == station_1) %>% pull(location_type)
  location_type_2 = deploy %>% filter(station_name == station_2) %>% pull(location_type)
  location_type_3 = deploy %>% filter(station_name == station_3) %>% pull(location_type)
  
  n_300 = df_dist %>% filter(transmitter %in% c(transmitter_1, transmitter_2, transmitter_3) & distance < 300) %>% nrow()
  n_500 = df_dist %>% filter(transmitter %in% c(transmitter_1, transmitter_2, transmitter_3) & distance < 500) %>% nrow()
  n1100 = df_dist %>% filter(transmitter %in% c(transmitter_1, transmitter_2, transmitter_3) & distance < 1100) %>% nrow()
  
  df_model_sum <- data.frame(n_300 = n_300, n_500 = n_500, n1100 = n1100)
  df_model_sum <- df_model_sum %>% mutate(
    station_name = list(c(station_1, station_2, station_3)),
    transmitter = list(c(transmitter_1, transmitter_2, transmitter_3)),
    location_type = list(c(location_type_1, location_type_2, location_type_3))) %>% 
    select(station_name, transmitter, n_300, n_500, n1100, location_type)
  
  
  list_out_day <- lapply(c(TRUE, FALSE), function(TF) {
    if(TF == TRUE) {formula_model = model_formula_day} else {formula_model = model_formula_day_nopower}
    out_day = runmodel_calceval(df_model_day, 
                                formula_model = formula_model,
                                zero_threshold = 0.05, 
                                include_range = TF, station_filter = station_vec, deploy = deploy,
                                brier_reference = brier_ref_day)
    out_day = cbind(df_model_sum, 
                    cbind(include_range = TF, include_power = TF, temp_res = "day"), 
                    as.data.frame(t(out_day)))
  })
  df_out_day = plyr::ldply(list_out_day)
  
  list_out_hour <- lapply(c(TRUE, FALSE), function(TF) {
    if(TF == TRUE) {formula_model = model_formula_hour} else {formula_model = model_formula_hour_nopower}
    out_hour = runmodel_calceval(df_model_hour,
                                 formula_model = formula_model,
                                 zero_threshold = 0.05, 
                                 include_range = TF, station_filter = station_vec, deploy = deploy,
                                 brier_reference = brier_ref_hour)
    out_hour = cbind(df_model_sum, 
                     cbind(include_range = TF, include_power = TF, temp_res = "hour"), 
                     as.data.frame(t(out_hour)))
  })
  df_out_hour = plyr::ldply(list_out_hour)
  
  
  out_3tag = rbind(df_out_day, df_out_hour)
  return(out_3tag)
})

df_output_3tag <- plyr::ldply(list_output_3tag)
# data.table::fwrite(df_output_3tag, "data/interim/temp_traintest/df_output_3tag.csv", row.names = F)

#### Run training and tests : range subsets ####
range_array_EW <- c("JJ_A9_1", "JJ_B9_1", "JJ_B9_6", "JJ_B9_5", "JJ_C9_1", 
                    "JJ_D9_1", "JJ_D9_6", "JJ_D9_5", "JJ_E9_1")
range_array_NS <- c("JJ_B10_1", "JJ_B9_4", "JJ_B9_5", "JJ_B9_6", 
                    "JJ_BC89", "JJ_C8_3", "JJ_C8_2", "JJ_C8_1")
range_array_full <- unique(deploy$station_name)

list_output_range <- lapply(c("EW", "NS", "full"), function(range_sub){
  range_array = get(paste0("range_array_", range_sub))
  
  list_output_range_sub <- lapply(c(8,16,24,32), function(n_days_sub) {
    print(paste("Starting model for", range_sub, "with", n_days_sub, "days", sep = " "))
    out_day_range <- runmodel_calceval_range(df_model_day, model_formula_day, zero_threshold = 0.05, 
                                                station_filter = range_array,
                                                deploy = deploy, n_days = n_days_sub, 
                                                brier_reference = brier_ref_day)
    out_day_range <- cbind(data.frame(station_name = NA,transmitter = NA, n_300 = NA, n_500 = NA,n1100 = NA ,
                                         location_type = NA,
                                         include_range = range_sub, range_days =  n_days_sub, 
                                         include_power = TRUE, temp_res = "day"), 
                              as.data.frame(t(out_day_range)))
    
    out_hour_range <- runmodel_calceval_range(df_model_hour, model_formula_hour, zero_threshold = 0.05, 
                                                 station_filter = range_array,
                                                 deploy = deploy, n_days = n_days_sub, 
                                                 brier_reference = brier_ref_hour)
    out_hour_range <- cbind(data.frame(station_name = NA,transmitter = NA, n_300 = NA,n_500 = NA,n1100 = NA ,
                                          location_type = NA,
                                          include_range = range_sub, range_days =  n_days_sub, 
                                          include_power = TRUE, temp_res = "hour"), 
                               as.data.frame(t(out_hour_range)))
    df_output_range <- rbind (out_day_range, out_hour_range)
    return(df_output_range)
  })
  df_output_range_sub <- plyr::ldply(list_output_range_sub)
  return(df_output_range_sub)
})

df_output_range <- plyr::ldply(list_output_range)
# data.table::fwrite(df_output_range, "data/interim/temp_traintest/df_output_range.csv", row.names = F)

#### Add NA column to tag df ####
df_output_1tag$station_name = as.list(as.character(df_output_1tag$station_name))
df_output_1tag = df_output_1tag %>% 
  mutate(station_name = as.list(as.character(station_name)),
         transmitter = as.list(as.character(transmitter)),
         location_type = as.list(as.character(location_type)))

df_output_tag = rbind(df_output_1tag, df_output_2tag, df_output_3tag)

df_output_tag = df_output_tag %>% 
  mutate(range_days = NA) %>%
  relocate(range_days, .after = include_range)

#### Save all datasets ####
df_output <- rbind(df_output_range, df_output_tag)
data.table::fwrite(df_output, "data/interim/df_output.csv", row.names = F)
