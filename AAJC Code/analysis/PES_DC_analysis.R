library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(grid)
library(gridExtra)
library(dvmisc)
source("aajc_tools.R")



# ==================================
# Analysis and data exploration of 
# 2010 population estimates compared 
# to 2010 Decennial Census Results
# ==================================

# ========
# get data 
# ========

analytical10 <- read.csv("../../Transformed Data/2010/ES_MR_comparison_2010.csv")
analytical00 <- read.csv("../../Transformed Data/2000/ES_MR_comparison_2000.csv")
analytical20 <- read.csv("../../Transformed Data/2020/ES_MR_comparison_2020.csv")

# ==================================
# Cleaning step - for 2020 only
# ==================================

# NHPI is recorded as "NA" in the RACE_GROUP column 
# R is reading them in as NA values 

analytical$RACE_GROUP[is.na(analytical$RACE_GROUP)] <- "NA"


# ==================================
# Getting summaries of percent diff.
# ==================================


# inspecting the PERCENT_DIFF column 
summary(analytical$NUMERIC_DIFF)

# A list of our race values (AA, AAC, NHPI, NHPIC)
# variables <- unique(analytical$RACE_GROUP)
variables <- c("AAC", "NAC")
# MR_comparisons <- c("PES_alone_MR_alone", "MR_alone_DC_alone", "PES_alone_MR_combo", "MR_combo_DC_combo")

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))


# get % summaries for each race group
for (i in variables){
  dummy <- analytical %>%
    filter(RACE_GROUP == i)
           # get rid of MR 
           # ,COMPARISON %!in% MR_comparisons)
  
  cat(" --------\n", i, "\n", "--------\n")
  # get percent breakdown summaries
  # percent_breakdown(dummy$PERCENT_DIFF)

  # Get overestimate, underestimate, and equal estimate summaries
  # num rows where % diff is negative (underestimated pop)
  # under <- nrow(dummy[dummy$PERCENT_DIFF < 0, ])
  # # num rows where % diff is positive (overestiamted pop)
  # over <- nrow(dummy[dummy$PERCENT_DIFF > 0, ])
  # # num rows where % diff is 0 -- no percentage diff between PES & DC
  # equal <- nrow(dummy[dummy$PERCENT_DIFF == 0, ])
  # # total rows
  # tot <- nrow(dummy)
  # # DF for table
  # OOE <- data.frame(Population = c("underestimated", "overestiamted", "equal"),
  #                   Number_of_US_counties = c(under,over,equal),
  #                   Percentage_of_US_counties = c(under/tot *100, over/tot*100, equal/tot*100))
  # 
  # print(formattable(OOE,
  #             align = c("l", rep("r", NCOL(OOE) - 1))))

  # get quantiles
  # print(summary(dummy$PERCENT_DIFF))
  # print(table(quant_groups(dummy$PERCENT_DIFF, groups = 5)))
  # print(table(quant_groups(dummy$PERCENT_DIFF, groups = 4)))

  # Top 20 counties by numeric difference in population between Estimates & Census
  # dummy1 <- dummy %>%
  #   top_n(20, abs(NUMERIC_DIFF)) %>%
  #   select(STNAME, CTYNAME, NUMERIC_DIFF, PERCENT_DIFF) %>%
  #   arrange(desc(abs(NUMERIC_DIFF)))
  # 
  # print(dummy1)

  # Top 20 counties by % difference in population between Estimates & Census
  # NOT USING THIS BC HUNDREDS OF COUNTIES HAVE THE MAX % DIFF VALUE OF 200%
  # dummyp <- dummy %>%
  #   top_n(20, abs(PERCENT_DIFF)) %>%
  #   select(STNAME, CTYNAME, PERCENT_DIFF, NUMERIC_DIFF) %>%
  #   arrange(desc(abs(PERCENT_DIFF)))
  # 
  # print(dummyp)
  
  
  # dummyp <- dummy %>%
  #   top_n(20, abs(PERCENT_OF_COUNTY_DIFF)) % >%
  #   select(STNAME, CTYNAME, PERCENT_OF_COUNTY_DIFF) %>%
  #   arrange(desc(abs(PERCENT_OF_COUNTY_DIFF)))
  # 
  # print(dummyp)
  # 
  # Top 10 states
  # aggregate numeric diff by state
  # dummy2 <- dummy %>%
  #   group_by(STNAME) %>%
  #   summarise(NUMERIC_DIFF = sum(abs(NUMERIC_DIFF))) %>%
  #   top_n(10, NUMERIC_DIFF) %>%
  #   arrange(desc(NUMERIC_DIFF))
  # 
  # print(dummy2)
  # 
  # # Top 10 states
  # # aggregate % diff by state
  # dummy3 <- dummy %>%
  #   group_by(STNAME) %>%
  #   summarise(ESTIMATE = sum(ESTIMATE), CENSUS = sum(CENSUS),
  #             PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2)) %>%
  #   top_n(10, abs(PERCENT_DIFF)) %>%
  #   arrange(desc(abs(PERCENT_DIFF)))
  # 
  # print(dummy3)


  # remove the maximum (6000) for display - its a big outlier and makes it hard to see the distribution 
  # dummy <- dummy[dummy$PERCENT_DIFF != max(dummy$PERCENT_DIFF), ]
  # hist(dummy$PERCENT_DIFF, breaks=100)
  
#   histo <- dummy %>%
#     ggplot(aes(x = PERCENT_DIFF)) +
#     geom_histogram(aes(fill = PERCENT_DIFF>0), binwidth = 5) +
#     theme_minimal() +
#     labs(fill = "Population was greater\nthan estimated?\n(Census results > estimated results)",
#          title = i,
#          x = "% Difference") +
#     scale_x_continuous(breaks=seq(-200, max(dummy$PERCENT_DIFF), 50))+
#     theme(axis.text.x = element_text(angle = 45))
# 
#   print(histo)
}


# dummy3 <- analytical %>%
#   mutate(
#   # dummy column to store ESTIMATE values for subtraction for numeric_diff
#   lag_estim = ESTIMATE) %>%
#   # filling NA values - ESTIMATE has NAs because there is no API_combo data available 
#   fill(lag_estim) %>%
#   filter(# get rid of MR 
#          COMPARISON == "PES_alone_DC_combo") %>%
#   group_by(STNAME) %>%
#   summarise(ESTIMATE = sum(lag_estim), CENSUS = sum(CENSUS),
#             PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2)) %>%
#   top_n(10, abs(PERCENT_DIFF)) %>%
#   arrange(desc(abs(PERCENT_DIFF)))
# 
# print(dummy3)
# 
# analytical[is.na(analytical$PERCENT_DIFF), ]


# =====================================
# CALCULATIONS
# =====================================

# At the national-level, the 2000 Census came in [x] below the population estimates for the API race group
# =====================================

analytical <- read.csv("../Transformed Data/PES_DC_MR_comparison_2000.csv")

analytical[, c(4,5,6)] <- sapply(analytical[, c(4,5,6)], as.numeric)
# get total populations on a national level - ESTIMATES - API alone
estim_val <- analytical %>%
  filter(COMPARISON == 'PES_alone_DC_alone') %>%
  # group_by(CTYNAME) %>%
  summarise(ESTIMATE = sum(ESTIMATE), CENSUS = sum(as.integer(CENSUS)), MR = sum(as.integer(MR)))


# get total populations on a national level - CENSUS & MR - API alone or combo
cen_mr_val <- analytical %>%
  filter(COMPARISON == 'MR_combo_DC_combo') %>%
  summarise(CENSUS_combo = sum(CENSUS), MR_combo = sum(MR))

national_totals <- cbind(estim_val, cen_mr_val)

# percent difference Alone and in combo
(national_totals$ESTIMATE - national_totals$MR)/ ((national_totals$ESTIMATE + national_totals$MR)/2)
(national_totals$ESTIMATE - national_totals$MR_combo)/ ((national_totals$ESTIMATE + national_totals$MR_combo)/2)



# State level comparisons - [Top 10 and bottom 10 table]
# =====================================

# Mapping in PES_DC_vis.R in the analytical_state dataset for the mapping for AA alone and API and NA alone 

# =====
# 2010
# =====

# aggregate by state 
analytical10_states <- analytical10 %>% group_by(STNAME, RACE) %>%
  summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>% mutate(NUM_DIFF = MR - ESTIM, 
                                                         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
                                                         COVERAGE = case_when(
                                                           NUM_DIFF < 0 ~ 'undercount',
                                                           NUM_DIFF > 0 ~ 'overcount',
                                                           NUM_DIFF == 0 ~ 'equal'
                                                         )) 

# write.csv(analytical10_states, "../../Transformed Data/2010/state_level_comparisons_2010_non_hispanic.csv")

# TOP and Bottom 10 tables - tables made in word 
analytical10_states %>% filter(RACE == "NHPI_AIC") %>% arrange(desc(abs(PERC_DIFF)))

# National Coverage by Race 
analytical10_states %>% filter(RACE %in% c('NHPI_A', 'NHPI_AIC')) %>% group_by(RACE) %>% summarise(ESTIM = sum(ESTIM), MR=sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM, 
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))


analytical10_states %>% filter(RACE %in% c('A_A', 'A_AIC')) %>% group_by(RACE) %>% summarise(ESTIM = sum(ESTIM), MR=sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM, 
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))

# =====
# 2020
# =====

# aggregate by state 
analytical20_states <- analytical20 %>% group_by(STNAME, RACE) %>%
  summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>% mutate(NUM_DIFF = MR - ESTIM, 
                                                         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
                                                         COVERAGE = case_when(
                                                           NUM_DIFF < 0 ~ 'undercount',
                                                           NUM_DIFF > 0 ~ 'overcount',
                                                           NUM_DIFF == 0 ~ 'equal'
                                                         )) 

write.csv(analytical20_states, "../../Transformed Data/2020/state_level_comparisons_2020.csv")

# TOP and Bottom 10 tables - tables made in word 
# analytical10_states %>% filter(RACE == "NHPI_AIC") %>% arrange(desc(abs(PERC_DIFF)))

# National Coverage by Race 
analytical20_states %>% filter(RACE %in% c('NHPI_A', 'NHPI_AIC')) %>% group_by(RACE) %>% summarise(ESTIM = sum(ESTIM), MR=sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM, 
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))

analytical20_states %>% filter(RACE %in% c('A_A', 'A_AIC')) %>% group_by(RACE) %>% summarise(ESTIM = sum(ESTIM), MR=sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM, 
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))

# =====
# 2000
# =====

# aggregate by state 
analytical00_states <- analytical00 %>% group_by(STNAME, RACE) %>%
  summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>% mutate(NUM_DIFF = MR - ESTIM, 
                                                         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),
                                                         COVERAGE = case_when(
                                                           NUM_DIFF < 0 ~ 'undercount',
                                                           NUM_DIFF > 0 ~ 'overcount',
                                                           NUM_DIFF == 0 ~ 'equal'
                                                         )) 

write.csv(analytical00_states, "../../Transformed Data/2000/state_level_comparisons_2000.csv")
  

# =====================================
# National level coverage for each category for both the 2010 and 2020 
# Census as well as the coverage for the 2000 Census for the API category
# =====================================

# # change NA to "NA" for NA populations 
# analytical10$RACE_GROUP[is.na(analytical10$RACE_GROUP)] <- "NA"
# analytical20$RACE_GROUP[is.na(analytical20$RACE_GROUP)] <- "NA"
# 
# race_groups <- unique(analytical10$RACE_GROUP)
# 
# # create dataframe for table
# nat_coverage <- data.frame(" " = c("AA", "AAC", "NA", "NAC", "API Alone", "API AIC"),
#                            "2000" = c("","","","",0,0),
#                            "2010" = c(0,0,0,0,"",""),
#                            "2020" = c(0,0,0,0,"",""), check.names = F)
#          
# for (i in race_groups) { 
#   # ----
#   # 2010 
#   # ----
#   
#   # loop through each race group to get over/undercounts
#   analytical <- analytical10 %>%
#     filter(RACE_GROUP == i & COMPARISON == "PES_MR") %>%
#     summarise(ESTIMATE = sum(ESTIMATE), MR = sum(MOD_RACE)) 
#   
#   # percent difference calculation
#   pdiff = ((analytical$MR - analytical$ESTIMATE) / ( (analytical$MR + analytical$ESTIMATE)/2 )) * 100
#   
#   if (pdiff < 0) {
#     # under count 
#     pdiff <- paste(as.character(round(pdiff, 2)), "% under count", sep = "")
#   }
#   else {
#     # over count 
#     pdiff <- paste(as.character(round(pdiff,2)), "% over count", sep = "")
#   }
#   
#   # append to nat_coverage DF 
#   nat_coverage$`2010`[nat_coverage$` ` == i] <- pdiff
#   
#   
#   # ----
#   # 2020 
#   # ----
#   
#   # loop through each race group to get over/undercounts
#   analytical <- analytical20 %>%
#     filter(RACE_GROUP == i & COMPARISON == "PES_MR") %>%
#     summarise(ESTIMATE = sum(ESTIMATE), MR = sum(MR)) 
#   
#   # percent difference calculation
#   pdiff = ((analytical$MR - analytical$ESTIMATE) / ( (analytical$MR + analytical$ESTIMATE)/2 )) * 100
#   
#   if (pdiff < 0) {
#     # under count 
#     pdiff <- paste(as.character(round(pdiff, 2)), "% under count", sep = "")
#   }
#   else {
#     # over count 
#     pdiff <- paste(as.character(round(pdiff,2)), "% over count", sep = "")
#   }
#   
#   # append to nat_coverage DF 
#   nat_coverage$`2020`[nat_coverage$` ` == i] <- pdiff
# 
# 
# }
# 
# 
# # Change Race categories column to something more readable 
# nat_coverage$` ` <- c("Asian Alone", "Asian AIC", "NHPI Alone", "NHPI AIC","API Alone", "API AIC")
# 
# # Add API values for 2000
# nat_coverage$`2000`[nat_coverage$` `=="API Alone"] <- "1.27% under count"
# nat_coverage$`2000`[nat_coverage$` `=="API AIC"] <- "10.42% over count"
# 
# print(formattable(nat_coverage, align = c("l", "c", "c", "c"), caption = "National Coverage by Category"))
# 
#       


# =====================================
# Coverage by single year of age in 2010 for the A AIC population
# =====================================

# ESTIMATE DATA 
# ----------------------------------------
# Dataset: 
#   1) 2010 estimates alone or in combo (state)
#   2) 2010 estimates alone only (state)
# documentation:
#   1) https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/sc-est2010-alldata5.pdf
#   2) https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/sc-est2010-alldata6.pdf
# ----------------------------------------

estim_state_2010_AIC_raw <- read.csv('../../Raw Data/2010/sc-est2010-alldata5.csv') # Alone or in combination
estim_state_2010_A_raw <- read.csv('../../Raw Data/2010/sc-est2010-alldata6.csv')   # Alone only


# DATAFRAMES CREATED:
# ---------------------------------------
# 1) estim_state_2010 --> state level data for age groups. To be used later for
#   differentiation by state (map) and county (map and histogram) Section below
#
# 2) estim_nat_2010 --> national population data for age groups. Used in this
#     Coverage by single year of age in 2010 for the A AIC population Section 
# ---------------------------------------

# Using --> POPESTIMATE72010 7/1/2010 resident population estimate

# 1. 
# estim_state_2010
es2010_AIC <- estim_state_2010_AIC_raw %>% 
  select(STATE, STNAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE72010) %>%
  filter(RACE == 5) %>% # 5 = Native Hawaiian and Other Pacific Islander Alone or in Combination  
  filter(SEX == 0, ORIGIN == 0) %>% 
  group_by(STATE, STNAME, AGE) %>%
  summarise(AIC = sum(POPESTIMATE72010))

es2010_A <- estim_state_2010_A_raw %>% 
  select(STATE, STNAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE72010) %>%
  filter(RACE == 5) %>% # 5 = Native Hawaiian and Other Pacific Islander Alone
  filter(SEX == 0, ORIGIN == 0) %>% 
  group_by(STATE, STNAME, AGE) %>%
  summarise(A = sum(POPESTIMATE72010))

estim_state_2010 <- left_join(es2010_AIC, es2010_A, by=c("STATE","STNAME", "AGE")) %>%
  pivot_longer(cols = c('AIC', 'A'), names_to = "RACE", values_to = "ESTIMATE")


# 2.
# estim_nat_2010
estim_state_2010_AIC <- estim_state_2010_AIC_raw %>%
  select(STATE, STNAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE72010) %>%
  filter(RACE == 5) %>% # 5 = Native Hawaiian and Other Pacific Islander Alone or in Combination  
  filter(SEX == 0, ORIGIN == 0) %>% 
  group_by(AGE) %>%
  summarise(AIC = sum(POPESTIMATE72010)) # get the total population by each age 

estim_state_2010_A <- estim_state_2010_A_raw %>%
  select(STATE, STNAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE72010) %>%
  filter(RACE == 5) %>% # 5 = Native Hawaiian and Other Pacific Islander Alone
  filter(SEX == 0, ORIGIN == 0) %>% 
  group_by(AGE) %>%
  summarise(A = sum(POPESTIMATE72010)) # get the total population by each age 
  
estim_nat_2010 <- left_join(estim_state_2010_AIC, estim_state_2010_A) %>%
  pivot_longer(cols = c('AIC', 'A'), names_to = "RACE", values_to = "ESTIMATE")


# ---------------------------------------
# The modified Race Data groups age into 
# 5-year age groups. For consistency,
# will manually do the same with estimate data
# ---------------------------------------

# Grouping AGE column based on ranges 
# 1. 
# National level 
estim_nat_2010 <- estim_nat_2010 %>%
  mutate(AGEGRP = cut(AGE,
                      breaks = seq(-1,89, by = 5),
                      labels = seq(1,18))) %>%
  # aggregate by new AGEGRP col
  group_by(AGEGRP, RACE) %>%
  summarise(ESTIMATE = sum(ESTIMATE))

estim_nat_2010$AGEGRP <- as.factor(estim_nat_2010$AGEGRP)

# 2. 
# State level 
estim_state_2010 <- estim_state_2010 %>%
  mutate(AGEGRP = cut(AGE,
                      breaks = seq(-1,89, by = 5),
                      labels = seq(1,18))) %>%
  # aggregate by new AGEGRP col
  group_by(STATE, STNAME, AGEGRP, RACE) %>%
  summarise(ESTIMATE = sum(ESTIMATE))


# MODIFIED RACE DATA 
# ----------------------------------------
# Dataset:
#   1) Census Modified Race Data 2010 
# documentation: 
#   1) https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/mr2010.pdf
# ----------------------------------------


# read in datasets
# Alabama - Missouri 
mr_al_mo_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_mt_wy.csv")


# list of race groups which contain Asian + another race
keep_imprace_aic <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)
keep_imprace_nhpi <- c(9,12,14,15,18,20,21,23,24,25,27,28,29,30,31)

# Subset Relevant data 
# -------

# 1. 
# State level 
mr_al_mo_2010_STATE <- mr_al_mo_2010 %>%
  filter(IMPRACE == 5 | IMPRACE %in% keep_imprace_nhpi) %>% # get all Asian/nhpi race groups 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 5 ~ 'A', #alone
    IMPRACE %in% keep_imprace_nhpi ~ 'AIC' #alone or in combo
  )) %>%
  
  # get total populations by race group 
  group_by(STATE, STNAME, AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))

mr_mt_wy_2010_STATE <- mr_mt_wy_2010 %>%
  filter(IMPRACE == 5 | IMPRACE %in% keep_imprace_nhpi) %>% # get all Asian/nhpi race groups 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 5 ~ 'A',
    IMPRACE %in% keep_imprace_nhpi ~ 'AIC'
  )) %>%
  
  # get total populations by race group 
  group_by(STATE, STNAME, AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))


# 2. 
# National level 
mr_al_mo_2010 <- mr_al_mo_2010 %>%
  filter(IMPRACE == 5 | IMPRACE %in% keep_imprace_nhpi) %>% # get all Asian race groups 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 5 ~ 'A',
    IMPRACE %in% keep_imprace_nhpi ~ 'AIC'
  )) %>%
  
  # get total populations by race group 
  group_by(AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))

mr_mt_wy_2010 <- mr_mt_wy_2010 %>%
  filter(IMPRACE == 5 | IMPRACE %in% keep_imprace_nhpi) %>% # get all Asian race groups 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 5 ~ 'A',
    IMPRACE %in% keep_imprace_nhpi ~ 'AIC'
  )) %>%
  
  # get total populations by race group 
  group_by(AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))


# Add alone population (A) to AIC (to get alone or in combination)
# -------

# 1. 
# National level 
aic <- mr_al_mo_2010 %>%
  group_by(AGEGRP) %>%
  summarise(mr_aic = sum(MR))

aic2 <- mr_mt_wy_2010 %>%
  group_by(AGEGRP) %>%
  summarise(mr_aic = sum(MR))

  # Add AIC values back into df
mr_al_mo_2010$MR[mr_al_mo_2010$RACE_GROUP == "AIC" ] <- aic$mr_aic
mr_mt_wy_2010$MR[mr_mt_wy_2010$RACE_GROUP == "AIC" ] <- aic2$mr_aic

# Combine alabama-missouri & montana-wyoming datasets
combined_MR_2010 <- mr_al_mo_2010 %>% left_join(mr_mt_wy_2010, by = c("AGEGRP", "RACE_GROUP")) %>%
  # add their populations together since this is national coverage 
  mutate(MR = MR.x + MR.y) %>%
  select(AGEGRP, RACE = RACE_GROUP, MR)

combined_MR_2010$AGEGRP <- as.factor(combined_MR_2010$AGEGRP)

# 2. 
# State level 
aic <- mr_al_mo_2010_STATE %>%
  group_by(STATE,STNAME,AGEGRP) %>%
  summarise(mr_aic = sum(MR))

aic2 <- mr_mt_wy_2010_STATE %>%
  group_by(STATE,STNAME, AGEGRP) %>%
  summarise(mr_aic = sum(MR))

# Add AIC values back into df
mr_al_mo_2010_STATE$MR[mr_al_mo_2010_STATE$RACE_GROUP == "AIC" ] <- aic$mr_aic
mr_mt_wy_2010_STATE$MR[mr_mt_wy_2010_STATE$RACE_GROUP == "AIC" ] <- aic2$mr_aic

# Combine alabama-missouri & montana-wyoming datasets
combined_MR_2010_STATE <- rbind(mr_al_mo_2010_STATE, mr_mt_wy_2010_STATE) %>%
  rename(RACE = RACE_GROUP)

combined_MR_2010_STATE$AGEGRP <- as.factor(combined_MR_2010_STATE$AGEGRP)

# JOIN estimates and modified race data 
# -------

# 1. 
# National level
popBy_age_2010 <- estim_nat_2010 %>% left_join(combined_MR_2010, by = c("AGEGRP", "RACE"))

# Calculate Error of Closer (EOC) - % difference 
popBy_age_2010 <- popBy_age_2010 %>%
  mutate(EOC = round(( (MR - ESTIMATE) / ( (MR + ESTIMATE)/2 ) * 100)  ,2))


# 1. 
# State level
state_EOC_2010 <- estim_state_2010 %>% left_join(combined_MR_2010_STATE, by = c("STATE", "STNAME", "AGEGRP","RACE"))

# Calculate Error of Closer (EOC) - % difference 
state_EOC_2010 <- state_EOC_2010 %>%
  mutate(EOC = round(( (MR - ESTIMATE) / ( (MR + ESTIMATE)/2 ) * 100)  ,2))

# Write to csv for use with PES_DC_vis.R
write.csv(state_EOC_2010, "../../Transformed Data/2010/state_level_comparisons_2010_by_agegrp_NHPI.csv")

# --------------
# Visualization
# --------------

# 1. 
# Line Graph 
popBy_age_2010 %>% ggplot(aes(x = AGEGRP, y=EOC, group = RACE)) + 
  geom_line(aes(color = RACE), size = 1) + 
  scale_color_manual(values = c("#916a92", "#f4c78d")) + 
  theme_minimal() + 
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("Coverage by Age Group in 2010 for NHPI Alone and NHPI Alone and in\nCombination Populations")

# Line graph without grid lines
popBy_age_2010 %>% ggplot(aes(x = AGEGRP, y=EOC, group = RACE)) + 
  geom_line(aes(color = RACE), size = 1) + 
  scale_color_manual(values = c("#916a92", "#f4c78d")) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey")) + 
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("Coverage by Age Group in 2010 for NHPI Alone and NHPI Alone and in\nCombination Populations")

# 2. 
# Age Groups with the top Error of Closure (tables)
top5_agegrp_A <- popBy_age_2010 %>% filter(RACE == 'A') %>% arrange(desc(abs(EOC)))
top5_agegrp_A <- top5_agegrp_A[1:5, ]

top5_agegrp_AIC <- popBy_age_2010 %>% filter(RACE == 'AIC') %>% arrange(desc(abs(EOC)))
top5_agegrp_AIC <- top5_agegrp_AIC[1:6, ]

# display table 
#   note - it will display pixelated in R viewer. Use png from AAJC Vis foder
library("png")
tbl1 <- readPNG("./AAJC Vis/top5_agegrps_EOC_2010.png")
plot.new() 
rasterImage(tbl1, 0,0,1,1)

# ''''''''''''''''''''''
# Table created manually in Words 
# due to the need for sub columns 
# ''''''''''''''''''''''


# =====================================
# Coverage for the over and under 18 pop in 2010 for the A AIC population
# =====================================

# FOR NOW - Do over and under 19 due to age groupings in modified race data 

over_under_19 <- popBy_age_2010 %>% mutate(over_19 = case_when(
  AGEGRP %in% c(1,2,3,4) ~ 0, # 0 - under 19 
  AGEGRP %!in% c(1,2,3,4) ~ 1 # 1 - over 19
))

over_under_19 <- over_under_19 %>% group_by(RACE, over_19) %>%
  summarise(ESTIMATE = sum(ESTIMATE), MR = sum(MR)) %>%
  mutate(EOC = round(( (MR - ESTIMATE) / ( (MR + ESTIMATE)/2 ) * 100)  ,2))


# --------------
# Visualization
# --------------

# 1. 
# Table 

# ''''''''''''''''''''''
# Table created manually in Words 
# due to the need for sub columns 
# ''''''''''''''''''''''

# display table 
tbl2 <- readPNG("../AAJC Vis/over_under_19_tbl_2010.png")
plot.new() 
rasterImage(tbl2, 0,0,1,1)

# 2. 
# BAR CHART 
ggplot(over_under_19, aes(x = RACE, y = EOC, fill = as.factor(over_19))) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Coverage for NHPI Populations Over and Under 19") + 
  ylab("Error of Closure (%)") + 
  xlab("Race Category") +
  scale_fill_manual(labels = c("19 or under", "Over 19"), name = "Age Group", values = c("#916a92", "#f4c78d")) +
  scale_x_discrete(labels = c("Alone", "Alone or in Combination")) + 
  theme_minimal()
  
  






# =====================================
# Show differentiation by state (map) and county (map and histogram)
# =====================================

# Will be using 'state_EOC_2010' DF defined above

head(state_EOC_2010)

min(state_EOC_2010$EOC)
max(state_EOC_2010$EOC)

# sanity check - confirming what we have here is the same as what we have in the ES_MR transformed data file
check2010 <- read.csv("../Transformed Data/2010/ES_MR_comparison_2010.csv")

check2010 %>% filter(RACE %in% c("A_A", "A_AIC")) %>% group_by(STNAME, RACE) %>%
  summarise(ES = sum(ESTIM), MR = sum(MR))

state_EOC_2010 %>% group_by(STNAME, RACE) %>%
  summarise(ES = sum(ESTIMATE), MR = sum(MR))

blah <- as.integer(seq(-100, 70, length.out = 4))


# =====================================
# National level coverage for each category for both the 2010 and 2020 
# Census as well as the coverage for the 2000 Census for the API category
# =====================================


# 2010 #
# ------

# 1. 
# Estimates 
nat_estim_2010_aic <- estim_state_2010_AIC_raw %>%
  select(STATE, STNAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE72010) %>%
  filter(RACE == 4) %>% # 4 = Asian Alone or in Combination 
  filter(SEX == 0, ORIGIN == 0) %>% 
  summarise(AIC = sum(POPESTIMATE72010)) # get the total population by each age 

nat_estim_2010_a <- estim_state_2010_A_raw %>%
  select(STATE, STNAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE72010) %>%
  filter(RACE == 4) %>% # 4 = Asian Alone
  filter(SEX == 0, ORIGIN == 0) %>%
  summarise(A = sum(POPESTIMATE72010)) # get the total population by each age 


# 2. 
# Modified Race

# read in datasets
# Alabama - Missouri 
mr_al_mo_2010 <- read.csv("../Raw Data/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../Raw Data/modified_race_2010_mt_wy.csv")


# list of race groups which contain Asian + another race
keep_imprace_aic <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)


mr_al_mo_2010 <- mr_al_mo_2010 %>%
  filter(IMPRACE == 4 | IMPRACE %in% keep_imprace_aic) %>% # get all Asian race groups 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 4 ~ 'A',
    IMPRACE %in% keep_imprace_aic ~ 'AIC'
  )) %>%
  
  # get total populations by race group 
  group_by(RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))

mr_mt_wy_2010 <- mr_mt_wy_2010 %>%
  filter(IMPRACE == 4 | IMPRACE %in% keep_imprace_aic) %>% # get all Asian race groups 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 4 ~ 'A',
    IMPRACE %in% keep_imprace_aic ~ 'AIC'
  )) %>%
  
  # get total populations by race group 
  group_by(RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))


# Add alone population (A) to AIC (to get alone or in combination)
# -------
aic <- mr_al_mo_2010 %>%
  summarise(mr_aic = sum(MR))

aic2 <- mr_mt_wy_2010 %>%
  summarise(mr_aic = sum(MR))

# Add AIC values back into df
mr_al_mo_2010$MR[mr_al_mo_2010$RACE_GROUP == "AIC" ] <- aic$mr_aic
mr_mt_wy_2010$MR[mr_mt_wy_2010$RACE_GROUP == "AIC" ] <- aic2$mr_aic

# Combine alabama-missouri & montana-wyoming datasets
combined_MR_2010 <- mr_al_mo_2010 %>% left_join(mr_mt_wy_2010, by = c("RACE_GROUP")) %>%
  # add their populations together since this is national coverage 
  mutate(MR = MR.x + MR.y) %>%
  select(RACE = RACE_GROUP, MR)



# 2020 #
# ------

# ESTIMATE DATA 
# ----------------------------------------
# Dataset: 
#   1) 2020 estimates alone or in combo (state)
#   2) 2020 estimates alone only (state)
# documentation:
#   1) https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/sc-est2020-alldata5.pdf
#   2) https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/sc-est2020-alldata6.pdf
# ----------------------------------------

estim_state_2020_AIC_raw <- read.csv('../Raw Data/SC-EST2020-ALLDATA5.csv') # Alone or in combination
estim_state_2020_A_raw <- read.csv('../Raw Data/SC-EST2020-ALLDATA6.csv')   # Alone only


# 1.
# Estimates 
nat_estim_2020_aic <- estim_state_2020_AIC_raw %>%
  select(STATE, NAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE2020) %>% # POPESTIMATE2020 7/1/2020 resident population estimate
  filter(RACE == 4) %>% # 4 = Asian Alone or in Combination 
  filter(SEX == 0, ORIGIN == 0) %>% 
  summarise(AIC = sum(POPESTIMATE2020)) # get the total population 

nat_estim_2020_a <- estim_state_2020_A_raw %>%
  select(STATE, NAME, SEX, ORIGIN, RACE, AGE, POPESTIMATE2020) %>%
  filter(RACE == 4) %>% # 4 = Asian Alone
  filter(SEX == 0, ORIGIN == 0) %>%
  summarise(A = sum(POPESTIMATE2020)) # get the total population 

# 2. 
# Modifed Race 

mr_2020 <- read.csv("../Transformed Data/mr_county_2020.csv")

# get asian populations only 
mr_2020 <- mr_2020 %>% filter(race_group == "AA" | race_group == "AAC") %>%
  group_by(race_group) %>%
  summarise(MR = sum(population)) 


# 2000 #
# ------

estim_nat_2000 <- read.csv("../Transformed Data/county_estimates_2000_interpolated.csv") # estimate data 
mr_2000 <- read.csv("../Transformed Data/MR_county_2000_API.csv")

estim_nat_2000$api <- as.integer(estim_nat_2000$api)
nat_estim_2000 <- sum(estim_nat_2000$api)

mr_2000 <- mr_2000 %>% group_by(RACE) %>%
  summarise(MR = sum(MR))


# 3.
# Calculations 
calc_2010 <- c(0,0)
calc_2020 <- c(0,0)
calc_2000 <- c(0,0)

# 2010 
# alone
calc_2010[1] <- round(( (combined_MR_2010$MR[combined_MR_2010$RACE == "A"] - nat_estim_2010_a$A) / ( (combined_MR_2010$MR[combined_MR_2010$RACE == "A"] + nat_estim_2010_a$A)/2 ) * 100)  ,2)
# AIC 
calc_2010[2] <- round(( (combined_MR_2010$MR[combined_MR_2010$RACE == "AIC"] - nat_estim_2010_aic$AIC) / ( (combined_MR_2010$MR[combined_MR_2010$RACE == "AIC"] + nat_estim_2010_aic$AIC)/2 ) * 100)  ,2)

# 2020
# alone 
calc_2020[1] <- round(( (mr_2020$MR[mr_2020$race_group == "AA"] - nat_estim_2020_a$A) / ( (mr_2020$MR[mr_2020$race_group == "AA"] + nat_estim_2020_a$A)/2 ) * 100)  ,2)
# AIC  
calc_2020[2] <- round(( (mr_2020$MR[mr_2020$race_group == "AAC"] - nat_estim_2020_aic$AIC) / ( (mr_2020$MR[mr_2020$race_group == "AAC"] + nat_estim_2020_aic$AIC)/2 ) * 100)  ,2)

# 2000
# API alone
round(( (mr_2000$MR[mr_2000$RACE == "API_alone"] - nat_estim_2000) / ( (mr_2000$MR[mr_2000$RACE == "API_alone"] + nat_estim_2000)/2 ) * 100)  ,2)
round(( (mr_2000$MR[mr_2000$RACE == "API_combo"] - nat_estim_2000) / ( (mr_2000$MR[mr_2000$RACE == "API_combo"] + nat_estim_2000)/2 ) * 100)  ,2)



