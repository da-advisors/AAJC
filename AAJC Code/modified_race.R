library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(plotly)
source("AAJC_theme.R")

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')

######################
# MODIFIED RACE DATA # 
######################

# Link to datasets: 
# https://www.census.gov/programs-surveys/popest/technical-documentation/research/modified-race-data.html



#######
# 2000
#######

# File format is ASCII txt file. Could not load this in R correctly. No additional documentation on file layout ??





########
# 2010 #
########

# read in datasets
# Alabama - Missouri 
mr_al_mo_2010 <- read.csv("../Raw Data/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../Raw Data/modified_race_2010_mt_wy.csv")


# =========
# Data Prep
# =========

# keep only county geographic summary level
mr_al_mo_2010 <- mr_al_mo_2010[mr_al_mo_2010$SUMLEV == 50, ]
mr_mt_wy_2010 <- mr_mt_wy_2010[mr_mt_wy_2010$SUMLEV == 50, ]

# remove unecessary columns -> State and County FIPS
drop_cols = c(2, 3)
mr_al_mo_2010 <- mr_al_mo_2010[, -drop_cols]
mr_mt_wy_2010 <- mr_mt_wy_2010[, -drop_cols]


# ===============
# Get Race Groups
# ===============

# Identify which key values in the IMPRACE column are relevant

# 4 = Asian Alone 
# Modified Race (Asian & some other race) 
      # 8 = White and Asian
      # 11 = Black or African American and Asian
      # 13 = American Indian and Alaska Native and Asian
      # 15 = Asian and Native Hawaiian and Other Pacific Islander
      # 17 = White and Black or African American and Asian
      # 19 = White and American Indian and Alaska Native and Asian
      # 21 = White and Asian and Native Hawaiian and Other Pacific Islander
      # 22 = Black or African American and American Indian and Alaska Native and Asian
      # 24 = Black or African American and Asian and Native Hawaiian and Other Pacific Islander
      # 25 = American Indian and Alaska Native and Asian and Native Hawaiian and Other Pacific Islander
      # 26 = White and Black or African American and American Indian and Alaska Native and Asian
      # 28 = White and Black or African American and Asian and Native Hawaiian and Other Pacific Islander
      # 29 = White and American Indian and Alaska Native and Asian and Native Hawaiian and Other Pacific Islander
      # 30 = Black or African American and American Indian and Alaska Native and Asian and Native Hawaiian and Other Pacific Islander
      # 31 = White and Black or African American and American Indian and Alaska Native and Asian and Native Hawaiian and Other Pacific Islander

# 5 = NHPI alone 
# Modified Race (NHPI & some other race) 
      # 9 White and Native Hawaiian and Other Pacific Islander
      # 12 Black or African American and Native Hawaiian and Other Pacific Islander 
      # 14 American Indian and Alaska Native and Native Hawaiian and Other Pacific Islander 
      # 15 (already have it above)
      # 18 White and Black or African American and Native Hawaiian and Other Pacific Islander
      # 20 White and American Indian and Alaska Native and Native Hawaiian and Other Pacific Islander
      # 21 White and Asian and Native Hawaiian and Other Pacific Islander 
      # 23 (already have it above)
      # 24 (already have it above)
      # 25 (already have it above)
      # 27 White and Black or African American and American Indian and Alaska Native and Native Hawaiian and Other Pacific Islander 
      # 28 (already have it above)
      # 29 (already have it above)
      # 30 (already have it above)
      # 31 (already have it above)

keep_imprace <- c(4,5,8,9,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

# filter data to keep all rows where imprace has a value in keep_imprace
    # Removed age groups and sex columns so that we have a true total count
    # Can bring back these columns if we need age/sex breakdowns

# ALL_df_name will preserve the age group, hispanic origin, and sex info in case we need it
ALL_mr_al_mo_2010 <- mr_al_mo_2010[-1] %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME, CTYNAME, IMPRACE, SEX,ORIGIN,AGEGRP) %>%
  summarise(count = sum(RESPOP))

# JUST the population info
mr_al_mo_2010 <- mr_al_mo_2010[-1] %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME,CTYNAME, IMPRACE) %>%
  summarise(count = sum(RESPOP))

# ALL_df_name will preserve the age group, hispanic origin, and sex info in case we need it
ALL_mr_mt_wy_2010 <- mr_mt_wy_2010[-1] %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME, CTYNAME, IMPRACE, SEX,ORIGIN,AGEGRP) %>%
  summarise(count = sum(RESPOP))

# JUST the population info
mr_mt_wy_2010 <- mr_mt_wy_2010[-1] %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME, CTYNAME, IMPRACE) %>%
  summarise(count = sum(RESPOP))


# ===============================
# Create AA, AAC, NA, NAC columns
# ===============================

# list of values which contain Asian + another race
keep_imprace_asian <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)
# list of values which contain native hawaiian/pacific islander + another race
keep_imprce_nhpi <- c(9,12,14,15,18,20,21,23,24,25,27,28,29,30,31)

mr_al_mo_2010 <- mr_al_mo_2010 %>%
  mutate(race_group = case_when(
    IMPRACE == 4 ~ "AA", #Asian American Alone
    IMPRACE %in% keep_imprace_asian ~ "AAC", #Asian American alone or in combination 
    IMPRACE == 5 ~ "NA", #Native Hawaiian & Pacific Islander Alone
    IMPRACE %in% keep_imprce_nhpi ~ "NAC" #Native Hawaiian & Pacific Islander Alone or in combination 
  ))

mr_mt_wy_2010 <- mr_mt_wy_2010 %>%
  mutate(race_group = case_when(
    IMPRACE == 4 ~ "AA", #Asian American Alone
    IMPRACE %in% keep_imprace_asian ~ "AAC", #Asian American alone or in combination 
    IMPRACE == 5 ~ "NA", #Native Hawaiian & Pacific Islander Alone
    IMPRACE %in% keep_imprce_nhpi ~ "NAC" #Native Hawaiian & Pacific Islander Alone or in combination 
  ))


# ==============================
# Identify any race trends (EDA)
# ==============================

# BY STATE 
# Get top 5 race groups for each state (by num of residents)
# ---------------------------------------------------------
mr_al_mo_2010_top_5_race_groups <- mr_al_mo_2010 %>%
  group_by(STNAME, IMPRACE) %>%
  summarise(population = sum(count)) %>%
  # get top 5 race groups with most residents per state 
  group_by(STNAME) %>%
  slice_max(order_by = population, n = 5)

mr_mt_wy_2010_top_5_race_groups <- mr_mt_wy_2010 %>%
  group_by(STNAME, IMPRACE) %>%
  summarise(population = sum(count)) %>%
  # get top 5 race groups with most residents per state 
  group_by(STNAME) %>%
  slice_max(order_by = population, n = 5)

mr_2010_top_5_race_groups <- rbind(mr_al_mo_2010_top_5_race_groups, mr_mt_wy_2010_top_5_race_groups)

# frequency table of top 5 race groups
# -------------------------------------
# convert imprace to factor 
mr_2010_top_5_race_groups$IMPRACE <- as.factor(mr_2010_top_5_race_groups$IMPRACE)
table(mr_2010_top_5_race_groups$IMPRACE)

# top race groups - "A","NA","W + A","W + NA","B + A","B + NA","AI/AN + A","A + NA","W + B + A","W + AI/AN + A","W + A + NA"

# frequency distribution of top 5 race groups
mr_2010_top_5_race_groups %>%
  ggplot(aes(x=IMPRACE)) +
  geom_bar() + 
  scale_x_discrete(labels = c("A","NA","W + A","W + NA","B + A","B + NA","AI/AN + A","A + NA","W + B + A","W + AI/AN + A","W + A + NA")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Distribution of top 5 race groups for all states")

# frequency table of ALL race groups
# ----------------------------------
mr_2010 <- rbind(mr_al_mo_2010, mr_mt_wy_2010)
table(mr_2010$IMPRACE)

dist_race_groups <- mr_2010 %>%
  # remove asian alone so we can visualize the other race groups
  filter(IMPRACE != 4) %>%
  ggplot(aes(x=as.factor(IMPRACE), y=count, fill = as.factor(STNAME))) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(labels = c("NA","W + A","W + NA","B + A","B + NA","AI/AN + A", "AI/AN + NA", "A + NA", "W + B + A",
                              "W + B + NA", "W + AI/AN + A", "W + AI/AN + NA", "W + A + NA", "B + AI/AN + A", "B + AI/AN + NA",
                              "B + A + NA", "AI/AN + A + NA", "W + B + AI/AN + A", "W + B + AI/AN + NA", "W + B + A + NA",
                              "W + AI/NH + A + NA","B + AI/AN + A + NA", "W + B + AI/AN + A + NA")) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Distribution race groups for all states") 

ggplotly(dist_race_groups)

# Race groups with highest population - Asian alone --> White + Asian --> NA  --> Black + Asian --> White + NA --> Asian + NA
highest_top_5_rg <- mr_2010 %>%
  filter(IMPRACE %in% c(4, 8, 5, 9, 11, 15)) %>%
  group_by(STNAME, IMPRACE) %>%
  summarise(population = sum(count)) %>%
  arrange(desc(population)) %>%
  mutate(race_group = case_when(
    IMPRACE == 4 ~ "Asian alone",
    IMPRACE == 8 ~ "White and Asian",
    IMPRACE == 5 ~ "NHPI alone",
    IMPRACE == 9 ~ "White and NHPI",
    IMPRACE == 11 ~ "Black and Asian",
    IMPRACE == 15 ~ "Asian and NHPI"
    ))
  
# mr_2010_top_5_race_groups %>%
#   ggplot(aes(x = population, y = STNAME, fill = IMPRACE))+
#   geom_bar(stat = "identity")

# interactive bar plot
# imprace_state_bar <- rbind(mr_al_mo_2010, mr_mt_wy_2010) %>%
#   filter(STNAME != "California") %>%
#   ggplot(aes(x = count, y = as.factor(STNAME), fill = as.factor(IMPRACE))) +
#   geom_bar(stat = "identity")
# 
# ggplotly(imprace_state_bar)


# ================================================================
# Combining all race groups into 4 categories --> AA, AAC, NA, NAC 
# ================================================================

# calculating AAC and NAC 
# AAC = AA + AAC       
# NAC = NA + NAC 

mr_2010 <- mr_2010 %>%
  group_by(STNAME, CTYNAME, race_group) %>%
  summarise(population = sum(count))

combo_pop <- mr_2010 %>%
  mutate(race_group = case_when(
    race_group == "AA" ~ "AAC",
    race_group == "AAC" ~ "AAC",
    race_group == "NA" ~ "NAC",
    race_group == "NAC" ~ "NAC"
  )) %>%
  group_by(STNAME, CTYNAME, race_group) %>%
  summarise(combo_pop = sum(population))


# see all counties with missing race_groups ie. 0 population for a given race group
missing_rg <- anti_join(combo_pop[-4], mr_2010[-4]) %>%
  mutate(population = 0)
# add missing race group rows - set them to 0 for now
mr_2010 <- bind_rows(mr_2010, missing_rg) %>%
  group_by(STNAME, CTYNAME, race_group) 
  
# left join combination data with mr data
mr_2010 <- left_join(mr_2010, combo_pop, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'race_group' = 'race_group')) 

# replace combination population values with combo_pop values
mr_2010$population[mr_2010$race_group == "AAC" | mr_2010$race_group == "NAC" ] <- 
  mr_2010$combo_pop[mr_2010$race_group == "AAC" | mr_2010$race_group == "NAC"]

# drop combo_pop column
mr_2010 <- mr_2010[-5]



# ===============================================
# Merging modified race data with PES & DC Data
# ===============================================

# read data 
analytical <- read.csv("../Transformed Data/estimates_census_comparison_2010.csv")


# change "variable" column name and values in prep for a left join
colnames(analytical)[4] <- "race_group"
analytical <- analytical %>% mutate(race_group = case_when(
race_group == "AA_TOT"~ "AA",
race_group == "AAC_TOT" ~ "AAC",
race_group == "NA_TOT" ~ "NA",
race_group == "NAC_TOT" ~ "NAC"
))

analytical <- left_join(analytical, mr_2010, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'race_group' = 'race_group'))

analytical <- analytical %>% select(STNAME, CTYNAME, race_group, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF, population, ESTIMATE_PERC,CENSUS_PERC,PERCENT_OF_COUNTY_DIFF,
                      estim_TOT_POP, census_TOT_POP, flag, flag_desc,WKT) %>%
  rename(MOD_RACE=population, geometry = WKT)


# replace NA values in MOD_RACE with 0 
analytical$MOD_RACE[is.na(analytical$MOD_RACE)] <- 0

# sum(is.na(analytical$MOD_RACE))


# =========================
# Numeric and % differences
# =========================

analytical <- analytical %>%
  mutate(
    # Modified Race was ___ people higher(or lower) than census results
    NUMERIC_MR_DC = round(MOD_RACE - CENSUS, 2),
    # Modified Race were __% higher(or lower) than census results
    PERCENT_DIFF_MR_DC = round(((MOD_RACE - CENSUS) / ((MOD_RACE + CENSUS) / 2)) * 100 ,2),
    # Estimates were ___ people higher(or lower) than Modified Race results
    NUMERIC_PES_MR = round(ESTIMATE - MOD_RACE, 2),
    # Estimates were __% higher(or lower) than Modified Raceresults
    PERCENT_DIFF_MR_PES = round(((ESTIMATE - MOD_RACE) / ((ESTIMATE + MOD_RACE) / 2)) * 100 ,2)) %>%
  rename(NUMERIC_PES_DC = NUMERIC_DIFF, PERCENT_DIFF_PES_DC = PERCENT_DIFF, RACE_GROUP = race_group)%>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MOD_RACE, NUMERIC_PES_DC, PERCENT_DIFF_PES_DC,NUMERIC_MR_DC, PERCENT_DIFF_MR_DC, 
         NUMERIC_PES_MR, PERCENT_DIFF_MR_PES, ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, geometry)



# ==================
# Handling NA values
# ==================

# ISSUES: 
# 1. Cases in which both populations reported 0, should indicate a 0% change but is instead NA
# 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
#    a county but the census reported 0 population)

analytical <- analytical %>% 
  mutate(
    # If MR & census/estimate both = 0, then PERCENT_DIFF should be 0
    PERCENT_DIFF_MR_DC = replace(PERCENT_DIFF_MR_DC, MOD_RACE == 0 & CENSUS == 0, 0),
    PERCENT_DIFF_MR_PES = replace(PERCENT_DIFF_MR_PES, MOD_RACE == 0 & ESTIMATE == 0, 0)) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MOD_RACE, NUMERIC_PES_DC, PERCENT_DIFF_PES_DC,NUMERIC_MR_DC, PERCENT_DIFF_MR_DC, 
         NUMERIC_PES_MR, PERCENT_DIFF_MR_PES, ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, geometry)


# ============================
# Melt Dataframe - tidy format
# ============================
boxplot(analytical$PERCENT_DIFF_PES_DC, analytical$PERCENT_DIFF_MR_DC, analytical$PERCENT_DIFF_MR_PES)

num_diff <- analytical %>% 
  rename(PES_DC = NUMERIC_PES_DC, MR_DC = NUMERIC_MR_DC, PES_MR = NUMERIC_PES_MR) %>%
  pivot_longer(cols = c(PES_DC, MR_DC,PES_MR), 
               names_to = "COMPARISON", values_to = "NUMERIC_DIFF") %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MOD_RACE, COMPARISON, NUMERIC_DIFF, ESTIMATE_PERC, CENSUS_PERC, 
         PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, geometry)

perc_diff <- analytical %>% 
  rename(PES_DC = PERCENT_DIFF_PES_DC, MR_DC = PERCENT_DIFF_MR_DC, PES_MR = PERCENT_DIFF_MR_PES) %>%
  pivot_longer(cols = c(PES_DC, MR_DC,PES_MR), 
               names_to = "COMPARISON", values_to = "PERCENT_DIFF") %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MOD_RACE, COMPARISON, PERCENT_DIFF)

analytical <- left_join(num_diff, perc_diff, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'RACE_GROUP' = 'RACE_GROUP',"MOD_RACE"="MOD_RACE",
                                      "ESTIMATE" = "ESTIMATE", "CENSUS" = "CENSUS", "COMPARISON" = "COMPARISON")) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MOD_RACE, COMPARISON, NUMERIC_DIFF, PERCENT_DIFF,
         ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, geometry)


# write to csv
# st_write(analytical, "../Transformed Data/PES_DC_MR_comparison_2010.csv", layer_options = "GEOMETRY=AS_WKT")

# ==========
# Simple EDA
# ==========


# 1. % Diff boxplots by comparison 
analytical %>%
  ggplot(aes(x = COMPARISON, y = PERCENT_DIFF, fill = COMPARISON)) + 
  geom_boxplot()

# 2. See all % differences distributions for each comparison 
ggplot(analytical, aes(x=PERCENT_DIFF)) + 
  geom_histogram(data = analytical %>% filter(COMPARISON == "PES_DC"),
                 aes(color = "green"), fill =NA, alpha = .05,binwidth = 2) +
  geom_histogram(data = analytical %>% filter(COMPARISON == "PES_DC"),
                 color="green", fill =NA, alpha = .05,binwidth = 2) + 
  geom_histogram(data = analytical %>% filter(COMPARISON == "MR_DC"),
                 col = "red" , fill = NA, alpha = .05, binwidth = 2) +
  geom_histogram(data = analytical %>% filter(COMPARISON == "PES_MR"),
                 color = "blue", fill = NA, alpha = .05, binwidth = 2) +
  theme_minimal()  +
  scale_color_manual(values = c("PES vs DC" = "green", "MR vs. DC" = "red", "PES vs. MR" = "blue"), name = "Comparisons") +
  ggtitle("Percent Difference Distribution for all Comparisons") 


# 3. Get % difference distributions for EACH comparison BY race group
analytical %>%
  filter(COMPARISON == "PES_DC") %>%
  ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
  geom_histogram(binwidth = 3) + 
  theme_minimal() + 
  ggtitle("PES vs DC % difference by race group")

analytical %>%
  filter(COMPARISON == "MR_DC") %>%
  ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
  geom_histogram(binwidth = 3) + 
  theme_minimal() + 
  ggtitle("Modified Race vs DC % difference by race group")

analytical %>%
  filter(COMPARISON == "PES_MR") %>%
  ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
  geom_histogram(binwidth = 3) + 
  theme_minimal() + 
  ggtitle("PES vs Modified Race % difference by race group")
