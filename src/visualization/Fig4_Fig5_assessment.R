
##################################
# Plot classification assessment #
##################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages ####
library(tidyverse)
library(ggpubr)

#### Read data ####
df_out <- read_csv("data/interim/df_output.csv")

#### Set colours ####
cols = c(
  "Range test" = "#de425b" ,
  "Reference tag" =  "#afe0dc",
  "Range + Reference" = "#155666"
)

#### Formatting ####
df_out <- df_out %>%
  # Change n_stations variable
  mutate(n_stations = str_count(station_name, "JJ")) %>% 
  mutate(n_stations = ifelse(is.na(n_stations), include_range, as.character(n_stations))) %>% 
 
  mutate(n_stations = factor(ifelse(n_stations == "full", "Full", n_stations), 
                             levels = c("EW", "NS", "Full", "1", "2", "3"))) %>% 
  
  # Rename BSS
  rename(BSS = BSS.Brier) %>% 
  
  # Change include_range variable
  mutate(include_range = ifelse(include_range == TRUE, "Range + Reference", 
                                ifelse(include_range == FALSE, "Reference tag", "Range test"))) %>% 
  mutate(include_range = factor(include_range,
                                levels = c("Range test", "Reference tag","Range + Reference"))) 

#### Create long format for plotting ####
df_out_long <- gather(df_out, key = "metric", value = "value", 
                      RMSE, Specificity, AUC, BSS)

#### Final plots: compare metrics range test vs no range test ####
p1 <- df_out_long %>% 
  filter(temp_res == "hour") %>% 
  filter(metric %in% c("RMSE", "Specificity", "AUC", "BSS")) %>%
  mutate(metric = factor(metric, levels = c("RMSE", "Specificity", "AUC", "BSS"))) %>% 
  ggplot(aes(x = as.factor(n_stations), y = value, fill = include_range)) +
  geom_boxplot() +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(n.breaks = 4) +
  facet_grid(metric~include_range, scales = "free", space = "free_x") +
  labs(y = "", x = "", title =expression(paste("P"["hour"])))

p2 <- df_out_long %>% 
  filter(temp_res == "day") %>% 
  filter(metric %in% c("RMSE", "Specificity", "AUC", "BSS")) %>%
  mutate(metric = factor(metric, levels = c("RMSE", "Specificity", "AUC", "BSS"))) %>% 
  ggplot(aes(x = as.factor(n_stations), y = value, fill = include_range)) +
  geom_boxplot() +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(n.breaks = 3) +
  facet_grid(metric~include_range, scales = "free", space = "free_x") +
  labs(y = "", x = "", title =expression(paste("P"["day"])))

# Combine
ptot = ggarrange(p1, p2, nrow = 1)

#### Save ####
ggsave(filename = "reports/figures/Fig4_classificationbox.jpg", plot = ptot,
       scale = 1, dpi = 600, width = 23, height = 15, units = "cm")


#### Final plots: relationships metrics #### 
df_out <- df_out %>% 
  mutate(include_range = factor(include_range, levels = c("Reference tag","Range + Reference", "Range test")))

p1 <- df_out %>% 
  arrange(include_range) %>% 
  mutate(temp_res = ifelse(temp_res == "day", "Day", "Hour")) %>% 
  mutate(temp_res = factor(temp_res, levels = c("Hour", "Day"))) %>% 
  ggplot(aes(Specificity, AUC)) +
  geom_point(aes(fill = as.factor(include_range)), shape = 21) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  scale_x_continuous(breaks = seq(0.6, 0.9, 0.1)) +
  facet_grid(metric~include_range, scales = "free", space = "free_x") +
  # scale_fill_manual(values = c("#A3CCFF", "#225699", "#E69E1C")) +
  scale_fill_manual(values = cols) +
  facet_grid(~as.factor(temp_res), scales = "free_x", space = "free_x")

p2 <- df_out %>% 
  arrange(include_range) %>% 
  mutate(temp_res = ifelse(temp_res == "day", "Day", "Hour")) %>% 
  mutate(temp_res = factor(temp_res, levels = c("Hour", "Day"))) %>% 
  ggplot(aes(RMSE, AUC)) +
  geom_point(aes(fill = as.factor(include_range)), shape = 21) +
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(0.12, 0.28, 0.02)) +
  facet_grid(~as.factor(temp_res), scales = "free_x", space = "free_x")

p3 <- df_out %>% 
  arrange(include_range) %>% 
  mutate(temp_res = ifelse(temp_res == "day", "Day", "Hour")) %>% 
  mutate(temp_res = factor(temp_res, levels = c("Hour", "Day"))) %>% 
  ggplot(aes(Specificity, BSS)) +
  geom_point(aes(fill = as.factor(include_range)), shape = 21) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(0.6, 0.9, 0.1)) +
  facet_grid(~as.factor(temp_res), scales = "free_x", space = "free_x")

p4 <- df_out %>% 
  arrange(include_range) %>% 
  mutate(temp_res = ifelse(temp_res == "day", "Day", "Hour")) %>% 
  mutate(temp_res = factor(temp_res, levels = c("Hour", "Day"))) %>% 
  ggplot(aes(RMSE, BSS)) +
  geom_point(aes(fill = as.factor(include_range)), shape = 21) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(0.12, 0.28, 0.02)) +
  facet_grid(~as.factor(temp_res), scales = "free_x", space = "free_x")

# Combine
ptot = ggarrange(p1, p2, p3, p4, align = "v")

#### Save ####
ggsave(filename = "reports/figures/Fig5_classificationpoint.jpg", plot = ptot,
       scale = 1, dpi = 600, width = 23, height = 10, units = "cm")
