# Figures

library(ggplot2)
library(ggpubr)
library(ggthemes)
library(dplyr)
library(tidyr)
library(jtools)
library(patchwork)


# Source Functions -------------------------------------------------------------
funs <- list.files(
  here::here('code/functions'),
  full.names = TRUE
)
lapply(funs, source)

# Data Prep --------------------------------------------------------------------
df <- read_elc(path = '2023-11-01-SAFE-U-complete.sav')

df$V_Bindung <- as.integer(df$V_AAI_Class_sec_insec == 'sicher')
df$M_Bindung <- as.integer(df$M_AAI_Class_sec_insec == 'secure')

dfm <- df[, c('V_Sensitivity', 'M_Sensitivity', 
              'V_Bindung', 'M_Bindung',
              'V_RF_PDI', 'M_RF_PDI', 
              'V_RF_AAI', 'M_RF_AAI')]

# FIGURES ----------------------------------------------------------------------
discrete_colors <- c("#faaf90", "#7777DD", "#f2f2bb", "#f194b8")
ordinal_colors <- c("#fdfd96", "#77dd77", "#84b6f4", "#fdcae1", "#ff6961")
binary_colors <- c("grey", "#faaf90", "#7777DD")

theme_pmu <- function(
    base_size = 12, 
    base_family = "serif"
) {
  
  require(ggplot2)
  
  theme_minimal(
    base_size = base_size, base_family = base_family
  ) %+replace%
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.position = "bottom",
          plot.title = element_text(size = rel(1.2), hjust = 0, vjust = 2),
          strip.text = element_text(size = rel(1), hjust = 0.5, vjust = 0.7))
}

### RF -------------------------------------------------------------------------

d1 <- dfm %>%
  mutate(id = 1:40) %>%
  select(id, V_RF_AAI, M_RF_AAI) %>%
  rename(mother = M_RF_AAI,
         father = V_RF_AAI) %>%
  mutate(
    direction = factor(case_when(
      father > (mother + 1) ~ 1,
      mother > (father + 1) ~ 2,
      .default = 3
    ))
  ) %>%
  pivot_longer(cols = c(father, mother)) %>%
  mutate(time = 'A) Actor-Partner effects for RF at T1.') 

d2 <- dfm %>%
  mutate(id = 1:40) %>%
  select(id, V_RF_PDI, M_RF_PDI) %>%
  rename(mother = M_RF_PDI,
         father = V_RF_PDI) %>%
  mutate(
    direction = factor(case_when(
      father > (mother + 1) ~ 1,
      mother > (father + 1) ~ 2,
      .default = 3
    ))
  ) %>%
  pivot_longer(cols = c(father, mother)) %>%
  mutate(time = 'B) Actor-Partner effects for RF at T2.') 





p1 <- rbind(d1, d2) %>%
  ggplot(
    aes(name, value)
    ) +
  geom_line(aes(group = id), 
            alpha = 0.7, 
            color = "darkgrey") +
    geom_violin(width = 0.3, 
                aes(fill = name),
                alpha = 0.7) +
    geom_boxplot(width = 0.1) +
    theme_pmu() +
    geom_rangeframe(sides = 'b') +
    xlab('') +  
    ylab('Reflective functioning') +
    scale_x_discrete(expand = c(0.1, 0.1)) +
    scale_y_continuous(limits = c(0, 8),
                       breaks = seq(0,8,2)) +
    theme(legend.position = 'none') +
    scale_color_manual(values = binary_colors[2:3]) +
    scale_fill_manual(values = binary_colors[2:3]) +
    facet_wrap(. ~ time)




d3 <- dfm %>%
  mutate(id = 1:40) %>%
  select(id, V_RF_AAI, V_RF_PDI) %>%
  rename(T1 = V_RF_AAI,
         T2 = V_RF_PDI) %>%
  mutate(
    direction = factor(case_when(
      T1 > (T2 + 1) ~ 1,
      T2 > (T1 + 1) ~ 2,
      .default = 3
    ))
  ) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  mutate(time = 'C) RF at T1 and T2 for Fathers.') 

d4 <- dfm %>%
  mutate(id = 1:40) %>%
  select(id, M_RF_AAI, M_RF_PDI) %>%
  rename(T1 = M_RF_AAI,
         T2 = M_RF_PDI) %>%
  mutate(
    direction = factor(case_when(
      T1 > (T2 + 1) ~ 1,
      T2 > (T1 + 1) ~ 2,
      .default = 3
    ))
  ) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  mutate(time = 'D) RF at T1 and T2 for Mothers.') 

p2 <- rbind(d3, d4) %>%
  ggplot(
    aes(name, value)
  ) +
  geom_line(aes(group = id), 
            alpha = 0.7, 
            color = "darkgrey") +
  geom_violin(width = 0.3, 
              aes(fill = time),
              alpha = 0.7) +
  geom_boxplot(width = 0.1) +
  theme_pmu() +
  geom_rangeframe(sides = 'b') +
  xlab('') +  
  ylab('Reflective functioning') +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0,8,2)) +
  theme(legend.position = 'none') +
  scale_color_manual(values = binary_colors[2:3]) +
  scale_fill_manual(values = binary_colors[2:3]) +
  facet_wrap(. ~ time)


plt1 <- ggarrange(
  p1, p2,
  nrow = 2
)


ggsave(filename = here::here('results/figures', 'RF.tiff'),
       plt1, width = 6, height = 8, bg = 'white')


######### Dropouts #############################################################

dropouts <- haven::read_sav(
  file = here::here('data', 'T1_01-45_SozDemAAI.sav')
) %>%
  mutate(
    Dropout = factor(Dropout,
                     levels = 1:2,
                     labels = c('not a dropout',
                                'dropout')),
    `annual income` = factor(Dem_Jahreseinkommen,
                             levels = 1:4,
                             labels = c('< 32,000€',
                                        '32,000 - 48,000€',
                                        '48,000 - 67,000€',
                                        '> 67,000€')),
    gender = factor(Dem_Geschlecht, 
                 levels = 1:2,
                 labels = c('female', 'male')),
    education = factor(Dem_Bildung,
                       levels = 1:5,
                       labels = c('No formal degree',
                                  'Lower secondary school diploma',
                                  'Apprenticeship',
                                  'Higher secondary diploma\n(A-levels)',
                                  'University degree'
                       )),
    `attachment style` = factor(AAI_sec_insec,
                                levels = 0:1,
                                labels = c('insecure', 'secure'))
  ) %>%
  rename(
    age = Dem_Alter,
    dropout = Dropout
  )

p_atta <- dropouts %>%
  ggplot(
    aes(x = `attachment style`, group = dropout, fill = dropout)
  ) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(breaks = c(0, 10, 20, 30),
                     labels = c(0, 5, 10, 20)) +
  xlab('Attachment style') +
  ylab('Number of participants') +
  theme_pmu() +
  scale_fill_manual(values = binary_colors[2:3]) +
  facet_wrap(.~gender) +
  theme(legend.title = element_blank())


p_rf <- dropouts %>%
  ggplot(
    aes(x = dropout, y = RF, group = dropout, fill = dropout)
  ) +
  geom_boxplot(
    alpha = 0.4, outlier.shape = NA, show.legend = FALSE
  ) +
  geom_point(
    position = position_jitter(width = 0.15), color = 'black', show.legend = FALSE
    ) +
  scale_fill_manual(values = binary_colors[2:3]) +
  facet_wrap(.~gender)+
  theme_pmu() +
  ylab('Reflective functioning (T1)') +
  xlab('') +
  theme(legend.title = element_blank())


p_age <- dropouts %>%
  ggplot(
    aes(x = dropout, y = age, group = dropout, fill = dropout)
  ) +
  geom_boxplot(
    alpha = 0.4, outlier.shape = NA, show.legend = FALSE
  ) +
  geom_point(
    position = position_jitter(width = 0.15), color = 'black', show.legend = FALSE
    ) +
  scale_fill_manual(values = binary_colors[2:3]) +
  facet_wrap(.~gender)+
  theme_pmu() +
  ylab('Participant age (years)') +
  xlab('') +
 theme(legend.title = element_blank())

p_edu <- dropouts %>%
  ggplot(
    aes(x = education, group = dropout, fill = dropout)
  ) +
  geom_bar(position = position_dodge()) +
  facet_wrap(.~gender) +
  xlab('Highest education') +
  ylab('Number of participants') +
  theme_pmu() +
  scale_fill_manual(values = binary_colors[2:3]) +
  theme(legend.title = element_blank()) +
  coord_flip()
  

p_inc <- dropouts %>%
  ggplot(
    aes(x = `annual income`, group = dropout, fill = dropout)
  ) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(breaks = c(0, 10, 20, 30),
                     labels = c(0, 5, 10, 20)) +
  xlab('Combined annual income') +
  ylab('Number of couples') +
  scale_fill_manual(values = binary_colors[2:3]) +
  theme_pmu() +
  theme(legend.title = element_blank()) +
  coord_flip()





p1 <- (
  (p_age | p_rf | p_atta) /
  (p_inc + p_edu + plot_spacer())
  ) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")


ggsave(
  here::here('results/figures', 'dropouts.tiff'),
  p1, width = 14, height = 9, bg = 'white'
)
