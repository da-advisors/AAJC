library(tidyverse)
library(tidyr)
library(dplyr)


county_agegrps <- read.csv("../../Transformed Data/2010/ES_MR_AGEGRP_comparison_2010.csv")


# ==========
# AGE GROUP   
# ==========

# ------
#  creating 65+ age group
# ------

# age groups 14 - 18 --> 65+

over65_cty <- county_agegrps %>% filter(AGEGRP >= 14) %>% group_by(STNAME, CTYNAME, RACE) %>%
  summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM,   # numeric diff
         EOC = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),   # percent difference/error of closure (EOC)
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))

# double check age groups only include 14 - 18
over65_cty_check <- county_agegrps %>% filter(AGEGRP >= 14) %>% group_by(STATE, CTYNAME, RACE, AGEGRP) %>%
  summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM,   # numeric diff
         EOC = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),   # percent difference/error of closure (EOC)
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))

unique(over65_cty_check$AGEGRP)

# add 65+ label 
over65_cty$AGE <- c("65+")

# export csv file
write.csv(over65_cty,"../../Transformed Data/data for viz_alysha/over65_cty_2010.csv", row.names = FALSE)

