# Visualization Reflective functioning 

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(ggpubr)

source(here::here('code', 'read-data.R'))

df <- read_elc(path = '2023-08-11-SAFE-U-complete-data.sav')

df$V_Bindung <- as.integer(df$V_AAI_Class_sec_insec == 'sicher')
df$M_Bindung <- as.integer(df$M_AAI_Class_sec_insec == 'secure')


dd <- df %>%
  mutate(
    v_rf_diff = V_RF_PDI - V_RF_AAI,
    m_rf_diff = M_RF_PDI - M_RF_AAI,
    v_rf_abs = abs(v_rf_diff),
    m_rf_abs = abs(m_rf_diff)
  ) %>%
  select(Dem_ID,
         v_rf_abs, v_rf_diff, V_RF_PDI, V_RF_AAI,
         m_rf_abs, m_rf_diff, M_RF_PDI, M_RF_AAI) 


# Plot 1 -----------------------------------------------------------------------

d1 <- dd %>%
  mutate(change = v_rf_diff) %>%
  select(Dem_ID, V_RF_PDI, V_RF_AAI, change) %>%
  tidyr::pivot_longer(cols = c('V_RF_PDI', 'V_RF_AAI')) %>%
  mutate(time = ifelse(name == 'V_RF_AAI', 'AAI', 'PDI'),
         gender = 'father')
d2 <- dd %>%
  mutate(change = m_rf_diff) %>%
  select(Dem_ID, M_RF_PDI, M_RF_AAI, change) %>%
  tidyr::pivot_longer(cols = c('M_RF_PDI', 'M_RF_AAI')) %>%
  mutate(time = ifelse(name == 'M_RF_AAI', 'AAI', 'PDI'),
         gender = 'mother')
dpl <- rbind(d1, d2) %>% rename(RF = value)

pl1 <- dpl %>%
  ggplot(aes(time, RF)) +
  geom_boxplot(width = 0.2, color = 'lightgrey') +
  geom_line(aes(color = change, group = Dem_ID), alpha = 0.7) + 
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.7) +
  scale_color_gradient2(midpoint = 0, 
                        low = "blue", 
                        mid = "white", 
                        high = "red") +
  scale_y_continuous(limits = c(0, 8.5), breaks = seq(0,8,2)) +
  theme_tufte() +
  geom_rangeframe(sides = 'b') + 
  scale_x_discrete(expand = c(0.1, 0.1)) +
  facet_grid(.~gender) + theme(
    strip.background = element_rect(color = "black", 
                                    fill = NA)
  ) +
  labs(color = 'change\nover time')
  
ggsave(here::here('results', 'plot-rf-1.tiff'),
       pl1,
       width = 4, height = 5,
       bg = 'white')




# Plot 2 -----------------------------------------------------------------------

pl2 <- dpl %>%
  ggplot(aes(RF)) +
  geom_histogram(color = 'black', fill = 'white') +
  geom_density(aes(y = after_stat(scaled))) +
  scale_y_continuous(limits = c(0,10)) +
  theme_tufte() +
  geom_rangeframe(sides = 'b') + 
  scale_x_continuous(limits = c(0,8)) +
  facet_grid(time~gender) + theme(
    strip.background = element_rect(color = "black", 
                                    fill = NA)
  ) +
  labs(color = 'change\nover time')

ggsave(here::here('results', 'plot-rf-2.tiff'),
       pl2,
       width = 6, height = 5,
       bg = 'white')



# Plot 3 -----------------------------------------------------------------------

pl3 <- dpl %>%
  ggplot(aes(time, RF)) +
  geom_violin() +
  geom_boxplot(color = 'black', fill = 'white') +
  scale_y_continuous(limits = c(0,10)) +
  theme_tufte() +
  geom_rangeframe(sides = 'b') + 
  #scale_x_continuous(limits = c(0,8)) +
  facet_grid(.~gender) + theme(
    strip.background = element_rect(color = "black", 
                                    fill = NA)
  ) +
  labs(color = 'change\nover time')

ggsave(here::here('results', 'plot-rf-2.tiff'),
       pl2,
       width = 6, height = 5,
       bg = 'white')
