
################################
# Plot concept of cumulative P #
################################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1

#### Load packages
library(tidyverse)

#### Settings ####
zero_threshold = 0.05

cols = c(
  '5' = "#615C54",
  '60' = "#D9CEBD"
)



#### Make plot dataframe ####
test_df <- expand.grid(
  n = c(5, 60),
  k = c(1,2,3),
  p = seq(0, 1, by = 0.01)
) %>% as.data.frame()  %>% 
  mutate(p_adjust = ifelse(p < zero_threshold, 0, (p - zero_threshold)/(1- zero_threshold))) %>% 
  mutate(p_cum_adjust = pbinom(k-1, n, p_adjust, lower.tail = F)) %>% 
  mutate(p_cum = pbinom(k-1, n, p, lower.tail = F)) %>% 
  mutate(n = as.character(n))

  
#### Make plot ####
p1 = test_df %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.margin = margin(10,10,10,10)) +
  geom_path(aes(x = p, y = p_cum, 
                colour = as.factor(n), linetype = as.factor(k)),
            size = 1.5) +
  geom_path(aes(x = p, y = p), colour = "gray10") +
  scale_colour_manual(values = cols, name ='n = ') +
  scale_x_continuous(expand= c(0,0)) +
  scale_y_continuous(expand= c(0,0)) +
  scale_linetype(name = 'k = ')+
  labs(x = expression(paste("Individual detection probability ", pi)) ,
       y = "Cumulative probability P")

p2 = test_df %>% 
  ggplot() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(10,10,10,10)) +
  geom_path(aes(x = p, y = p_cum_adjust, 
                colour = as.factor(n), linetype = as.factor(k)),
            size = 1.5) +
  geom_path(aes(x = p, y = p_adjust)) +
  scale_colour_manual(values = cols, name ='n = ') +
  scale_x_continuous(expand= c(0,0)) +
  scale_y_continuous(expand= c(0,0)) +
  labs(x = expression(paste("Individual detection probability ", pi^"0")) ,
       y = "Cumulative probability P")
ptot = ggpubr::ggarrange(p1, p2, nrow = 2, heights = c(1,0.8))

#### Save ####
ggsave(filename = "reports/figures/Fig1_cumprob.png", plot = ptot, scale = 1, dpi = 600, width = 22, height = 14, units = "cm")
