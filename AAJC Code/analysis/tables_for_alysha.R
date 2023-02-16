library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(grid)
library(gridExtra)
library(dvmisc)
source("aajc_tools.R")

#############################
# 2010 Census Accuracy: NHP #
#############################



comparison10 <- read.csv('../../Transformed Data/2010/ES_MR_comparison_2010.csv')
comparison20 <- read.csv('../../Transformed Data/2020/ES_MR_comparison_2020.csv')

# ----
# 2010 
# ----
# get number of counties per group 
coverage_2010 <- comparison10 %>% group_by(RACE) %>% count(COVERAGE)

# total counties
total_counties_2010 <- coverage_2010 %>% group_by(RACE) %>% summarise(sum(n))
total_counties_2010 <- 3143

# percentage of counties calculation 
coverage_2010 <- coverage_2010 %>% mutate(percent_counties_2010 = (n/total_counties_2010)*100 ) %>%
  rename('num_counties_2010' = n)


# ----
# 2020 
# ----
# get number of counties per group 
coverage_2020 <- comparison20 %>% group_by(RACE) %>% count(COVERAGE)

# total counties
total_counties_2020 <- coverage_2020 %>% group_by(RACE) %>% summarise(sum(n))
total_counties_2020 <- 3143

# percentage of counties calculation 
coverage_2020 <- coverage_2020 %>% mutate(percent_counties_2020 = (n/total_counties_2020)*100 ) %>%
  rename('num_counties_2020' = n)


# merge 2010 and 2020 
coverage <- coverage_2010 %>% left_join(coverage_2020, by = c('RACE', 'COVERAGE'))

# save to file 
write.csv(coverage, '../../Transformed Data/data for viz_alysha/census_accuracy_by_county_coverage_for_gauge_chart.csv')


########
# NHPI #
########

coverage_bar_nhpi_a <- ggplot(coverage%>%filter(RACE == 'NHPI_A'), aes(x=COVERAGE, y=percent_counties_2010)) +
  geom_bar(stat = 'identity', fill = '#6c2e65')+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(aes(label=round(percent_counties_2010,1) ), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle = "2010 Census Accuracy: NHPI Alone") + 
  ylab("Percentage of Counties") + 
  xlab("")

coverage_bar_nhpi_aic <- ggplot(coverage%>%filter(RACE == 'NHPI_AIC'), aes(x=COVERAGE, y=percent_counties_2010)) +
  geom_bar(stat = 'identity', fill = '#6c2e65')+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(aes(label=round(percent_counties_2010,1) ), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle = "2010 Census Accuracy: NHPI Alone or In Combo") + 
  ylab("Percentage of Counties") + 
  xlab("")

coverage_bar_nhpi_a_2020 <- ggplot(coverage%>%filter(RACE == 'NHPI_A'), aes(x=COVERAGE, y=percent_counties_2020)) +
  geom_bar(stat = 'identity', fill = '#6c2e65')+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(aes(label=round(percent_counties_2020,1) ), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle = "2020 Census Accuracy: NHPI Alone") + 
  ylab("") + 
  xlab("")

coverage_bar_nhpi_aic_2020 <- ggplot(coverage%>%filter(RACE == 'NHPI_AIC'), aes(x=COVERAGE, y=percent_counties_2020)) +
  geom_bar(stat = 'identity', fill = '#6c2e65')+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(aes(label=round(percent_counties_2020,1) ), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle = "2020 Census Accuracy: NHPI Alone or In Combo") + 
  ylab("") + 
  xlab("")


coverage_bar_NHPI <- grid.arrange(coverage_bar_nhpi_a, coverage_bar_nhpi_a_2020, coverage_bar_nhpi_aic, coverage_bar_nhpi_aic_2020)
ggsave(filename =  '../../AAJC Vis/census_accuracy_by_county_NHPI_BAR.png', plot = coverage_bar_NHPI, height = 8, width = 8)



######
# AA #
######

coverage_bar_aa_aic_2010 <- ggplot(coverage%>%filter(RACE == 'A_AIC'), aes(x=COVERAGE, y=percent_counties_2010)) +
  geom_bar(stat = 'identity', fill = '#6c2e65')+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(aes(label=round(percent_counties_2010,1) ), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle = "2010 Census Accuracy: Asian Alone or In Combo") + 
  ylab("Percentage of Counties") + 
  xlab("")


coverage_bar_aa_aic_2020 <- ggplot(coverage%>%filter(RACE == 'A_AIC'), aes(x=COVERAGE, y=percent_counties_2020)) +
  geom_bar(stat = 'identity', fill = '#6c2e65')+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_text(aes(label=round(percent_counties_2020,1) ), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle = "2020 Census Accuracy: Asian Alone or In Combo") + 
  ylab("") + 
  xlab("")


coverage_bar_AA <- grid.arrange(coverage_bar_aa_aic_2010, coverage_bar_aa_aic_2020, nrow=1)
ggsave(filename =  '../../AAJC Vis/census_accuracy_by_county_AA_BAR.png', plot = coverage_bar_AA, height = 4, width = 8)



