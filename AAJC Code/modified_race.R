library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(plotly)
library(sf)
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

# File format is ASCII txt file
# https://readr.tidyverse.org/reference/read_fwf.html#ref-examples
# https://groups.google.com/g/manipulatr/c/e2etwwOHUvw

# =====================
# Read fixed width file
# =====================

# read csv with column name information (done in Excel)
# documentation used to make column width csv file: (p. 15)
#   https://www2.census.gov/programs-surveys/popest/technical-documentation/methodology/modified-race-summary-file-method/mrsf2000.pdf
col_info <- read.csv("../Raw Data/mr-co-column-info.csv")

# Define column widths 
widths <- col_info$column_widths

# Define column names 
cols <- col_info$column_names  # col names set to "not API" if the race group does not contain Asian/NHPI


mr_2000 <- read_fwf("../Raw Data/mr-co.txt",
                    fwf_widths( widths,
                                # columns that do NOT contain Asian/NHPI populations named - "NA"
                                col_names = cols))



# ==========
# Clean Data
# ==========

# Drop columns that do NOT have Asian/NHPI population data
mr_2000 <- mr_2000 %>%
  select(-contains(c("not_api")))

# all population data is contained within columns 5 - 100
# Melt columns 5 - 100
colnames(mr_2000)[5]
colnames(mr_2000)[100]

mr_2000 <- mr_2000 %>%
  pivot_longer(cols = "HL-M-AA":"NHL-F-W_B_AINA_A_NHPI", names_to = "RACE_GROUP", values_to = "population") %>%
  rename(five_yr_age_grp = `Five-year age groups `, STNAME = `State postal abbreviation  `, CTYNAME = `County or county equivalent `,
         FIPS_ST = `FIPS state code `, FIPS_CTY = `FIPS county code `) %>%
  select(FIPS_ST,FIPS_CTY,STNAME,CTYNAME,five_yr_age_grp,RACE_GROUP, population)

# DOUBLE COUNTING ISSUE !! #
# --------------------------
# State records can be identified by blanks in the county or municipio code field.

# sanity check 
mr_2000 %>% filter(STNAME == "CA", CTYNAME == "California") 

mr_2000 %>% filter(STNAME == "CA", CTYNAME != "California") %>%
  group_by(five_yr_age_grp, RACE_GROUP) %>%
  summarise(pop = sum(population))

# remove state records where county is blank/NA to get ONLY county totals 
mr_2000 <- mr_2000 %>% drop_na(FIPS_CTY)


# create a copy
mr_2000_tidy <- mr_2000


# (takes a while to run)
mr_2000_tidy <- mr_2000_tidy %>%
  # Add 3 columns - ORIGIN, SEX, RACES
  mutate(ORIGIN = NA,
         SEX = NA,
         RACE = NA) %>%
  # seperate the data in RACE_GROUP 
  separate(col = RACE_GROUP,
           sep = "-",
           into = c("ORIGIN", "SEX", "RACE")) %>%
  select(STNAME, CTYNAME, AGEGRP = five_yr_age_grp, ORIGIN, SEX, RACE, population) %>%
  group_by(STNAME, CTYNAME, SEX, ORIGIN, AGEGRP,RACE)

# ** save mr_2000_tidy now to preserve race groups, origin, age, and gender 

# consolidate origin, sex, age
mr_2000_tidy <- mr_2000_tidy %>% group_by(STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(population))


# -----------------------
# consolidate race groups into --> AA, A_AIC, NHPIa, NHPI_AIC
# -----------------------

# AA - asian alone 
# NHPIA - NHPI alone 

# lowercase capital "A"s so it does not interfere with A (asian alone) race category  
mr_2000_tidy$RACE <- str_replace_all(mr_2000_tidy$RACE, "AINA", "aINa")
mr_2000_tidy$RACE <- str_replace_all(mr_2000_tidy$RACE, "NHPIA", "NHPIa")

# sanity check 
grepl("A", mr_2000_tidy$RACE[12])


# create 2 new columns - 1 if RACE contians A (asian) and 1 if RACE contains NHPI
mr_2000_tidy <- mr_2000_tidy %>% mutate(Asian = case_when(
  grepl("A", RACE) ~ T
),
NHPI = case_when(
  grepl("NHPI", RACE) ~ T
))


# rename race categories as needed 
mr_2000_tidy <- mr_2000_tidy %>% mutate(RACE = case_when(
  RACE == "AA" ~ "AA",
  RACE == "NHPIa" ~ "NHPIa",
  # choosing a random race group to set as our (alone or in combination race groups) 
  RACE == "A_NHPI" ~ "A_AIC",
  RACE == "aINa_A" ~ "NHPI_AIC"
))
  

# sum populations of all race groups for each county
# (takes a long time to run)
for (st in unique(mr_2000_tidy$STNAME)) {
  
  for (cty in unique(mr_2000_tidy$CTYNAME)){
    
    # 1. 
    # dummy DF so we can sum all values that contain Asian race 
    test <- mr_2000_tidy %>% filter(STNAME == st, CTYNAME == cty, Asian == T)
    asian_aic <- sum(test$MR)
    
    # dummy DF so we can sum all values that contain NHPI race 
    test <- mr_2000_tidy %>% filter(STNAME == st, CTYNAME == cty, NHPI == T)
    nhpi_aic <- sum(test$MR)
    
    # 2.
    # get the alone or in combo values back into original DF
    mr_2000_tidy$MR[mr_2000_tidy$STNAME == st & mr_2000_tidy$CTYNAME == cty & mr_2000_tidy$RACE == "A_AIC"] <- asian_aic
    
    mr_2000_tidy$MR[mr_2000_tidy$STNAME == st & mr_2000_tidy$CTYNAME == cty & mr_2000_tidy$RACE == "NHPI_AIC"] <- nhpi_aic
  }
  
}

# drop rows where RACE is NA
mr_2000_tidy <- mr_2000_tidy %>% drop_na(RACE) %>%
  select(-Asian, -NHPI)

# fix race column values 
mr_2000_tidy <- mr_2000_tidy %>% mutate(RACE = case_when(
  RACE == "A_AIC" ~ "A_AIC",
  RACE == "AA" ~ "A_A",
  RACE == "NHPIa" ~ "NHPI_A",
  RACE == "NHPI_AIC" ~"NHPI_AIC"
  
))

# ------------
# writing to csv - 
#   all four race categories {A alone, A AIC, NHPI alone, NHPI AIC}
#-------------

write.csv(mr_2000_tidy, "../Transformed Data/MR_county_2000_4_race_categories.csv")


# combining race into API alone or API combo 

mr_2000_api <- mr_2000_tidy

mr_2000_api <- mr_2000_api %>% mutate( RACE = case_when(
  RACE == "A_AIC" ~ "API_combo",
  RACE == "A_A" ~ "API_alone",
  RACE == "NHPI_A" ~ "API_alone",
  RACE == "NHPI_AIC" ~"API_combo"
))  %>%
  group_by(STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(MR))

# ------------
# writing to csv - 
#   API alone and API combo only 
#-------------

write.csv(mr_2000_api, "../Transformed Data/MR_county_2000_API.csv")


# Old 2000's code below 

# . 
# .
# .

# # ======================
# # export cleaned MR data 
# # ======================
# 
# # Because this dataset is very large we need to save into a few (10) different csv's
# # Each csv will have about roughly 500K rows
# 
# # split the data into seperate DFs by state 
# mr_2000_split <- split(mr_2000_tidy, f = mr_2000_tidy$STNAME)
# 
# # remove Puerto Rico
# mr_2000_split <- mr_2000_split[-40]
# 
# # rbind some states together 
# AK_CO <- bind_rows(mr_2000_split[1:6])
# CT_IA <- bind_rows(mr_2000_split[7:13])
# ID_KS <- bind_rows(mr_2000_split[14:17])
# KY_MI <- bind_rows(mr_2000_split[18:23])
# MN_MS <- bind_rows(mr_2000_split[24:26])
# MT_NM <- bind_rows(mr_2000_split[27:33])
# NV_OR <- bind_rows(mr_2000_split[34:38])
# PA_TN <- bind_rows(mr_2000_split[39:43])
# TX_UT <- bind_rows(mr_2000_split[44:45])
# VA_WY <- bind_rows(mr_2000_split[46:51])
# 
# # put the DFs back into a list
# DF_list <- list(AK_CO, CT_IA, ID_KS, KY_MI, MN_MS, MT_NM, NV_OR, PA_TN, TX_UT, VA_WY)
# DF_names <- list("AK_CO", "CT_IA", "ID_KS", "KY_MI", "MN_MS", "MT_NM", "NV_OR", "PA_TN", "TX_UT", "VA_WY")
# 
# # loop through list of DFs and export as csv 
# for (i in 1:length(DF_names)) {
#   write_csv( DF_list[[i]], paste0("../Transformed Data/mr_county_2000/", DF_names[i], "_mr_2000.csv") )
# }
# 
# # zip our new mr_county_2000 folder with all the csvs
# zip(zipfile = '../Transformed Data/mr_county_2000', files = "../Transformed Data/mr_county_2000")
# 
# 
# # ===============================================================
# # Transform Data for merge with PES & DC numeric & perc diff data
# # ===============================================================
# 
# # Replace AINA values with IN
# mr_2000_tidy$RACE <- str_replace_all(mr_2000_tidy$RACE, "AINA", "IN")
# 
# # Create a new RACE_GROUP column with our desired race groups --> API_alone API_combo 
# # TAKES VERY LONG TO RUN 
# mr_2000_tidy <- mr_2000_tidy %>%
#   mutate(RACE_GROUP = case_when(
#     RACE == "A" | RACE == "NHPI" ~ "API_alone",
#     RACE != "A" | RACE != "NHPI" ~ "API_combo"
#   )) %>%
#   # Get the TOTAL populations by RACE GROUP - we are adding people of all SEX, ORIGIN, & AGEGRP together here 
#   group_by(STNAME,CTYNAME, RACE_GROUP) %>%
#   summarise(MR = sum(population))
# 
# copy_mr_2000_tidy <- mr_2000_tidy
# 
# # get all combination values in a sep DF
# combos <- copy_mr_2000_tidy %>%
#   group_by(STNAME, CTYNAME) %>%
#   summarise(combo = sum(MR))
# 
# # Join combo values to mr_2000_tidy
# copy_mr_2000_tidy <- copy_mr_2000_tidy %>%
#   left_join(combos, by = c("STNAME", "CTYNAME")) %>% 
#   # insert combination values where RACE_GROUP = API_combo
#   mutate(MR = case_when(
#     RACE_GROUP == "API_alone" ~ MR,
#     RACE_GROUP == "API_combo" ~ combo
#   )) %>%
#   select(STNAME, CTYNAME, RACE_GROUP, MR)
# 
# # export as csv
# write_csv(copy_mr_2000_tidy, "../Transformed Data/MR_county_2000.csv")
# 
# # state populations are stored in  ctyname = state name 
# # mr_2000_tidy %>%
# #   filter(STNAME == "WY" & CTYNAME != "Wyoming") %>%
# #   group_by(RACE_GROUP) %>%
# #   summarise(n = sum(MR))
# 
# 
# # ===============================================
# # Merging modified race data with PES & DC Data
# # ===============================================
# 
# analytical <- read.csv("../Transformed Data/PES_DC_comparison_2000.csv")
# 
# # edited the STNAMES in MR_county_2000.csv in excel
# mr_2000_tidy <- read.csv("../Transformed Data/MR_county_2000.csv")
# 
# analytical <- analytical %>%
#   left_join(mr_2000_tidy, by = c("STNAME", "CTYNAME", "RACE_GROUP")) %>%
#   select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, NUMERIC_DIFF, PERCENT_DIFF, CENSUS_tot_pop, geometry = WKT)
# 
# # =====================
# # Adding MR comparisons
# # =====================
# 
# # pivot wider the COMPARISONS column so we can add our MR comparisons 
# analytical$comparison2 <- analytical$COMPARISON
# analytical <- analytical %>% mutate(comparison2 = paste(comparison2, " mr")) %>% distinct()
# 
# analytical <-  analytical %>%
#     
#     pivot_wider(names_from = COMPARISON, values_from = NUMERIC_DIFF) %>%
#     rename(PES_alone_DC_alone__N = `PES (alone) - DC (alone)`, PES_alone_DC_combo__N = `PES (alone) - DC (combo)`) %>%
#     pivot_wider(names_from = comparison2, values_from = PERCENT_DIFF) %>%
#     rename(PES_alone_DC_alone__P = `PES (alone) - DC (alone)  mr`, PES_alone_DC_combo__P = `PES (alone) - DC (combo)  mr`) %>%
#     
#     # because estimates col has NAs, we need a dummy col. for calculations
#     mutate(lag_estim = ESTIMATE) %>%
#     fill(lag_estim) %>%
#   
#     # Compute numeric & percent diffs
#     mutate(
#       PES_alone_MR_alone__N = case_when(
#         RACE_GROUP == "API_alone" ~ round(lag_estim - MR, 2)),
#       PES_alone_MR_combo__N = case_when(
#         RACE_GROUP == "API_combo" ~round(lag_estim - MR, 2)),
#       MR_alone_DC_alone__N = case_when(
#         RACE_GROUP == "API_alone" ~ round(MR - CENSUS, 2)),
#       MR_combo_DC_combo__N = case_when(
#         RACE_GROUP == "API_combo" ~round(MR - CENSUS, 2)),
#       PES_alone_MR_alone__P = case_when(
#         RACE_GROUP == "API_alone" ~ round(((lag_estim - MR) / ((lag_estim + MR) / 2)) * 100 ,2)),
#       PES_alone_MR_combo__P = case_when(
#         RACE_GROUP == "API_combo" ~ round(((lag_estim - MR) / ((lag_estim + MR) / 2)) * 100 ,2)),
#       MR_alone_DC_alone__P = case_when(
#         RACE_GROUP == "API_alone" ~ round(((MR - CENSUS) / ((MR + CENSUS) / 2)) * 100 ,2)),
#       MR_combo_DC_combo__P = case_when(
#         RACE_GROUP == "API_combo" ~ round(((MR - CENSUS) / ((MR + CENSUS) / 2)) * 100 ,2))) %>%
#   
#   select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, PES_alone_DC_alone__N, PES_alone_DC_combo__N, PES_alone_MR_alone__N, PES_alone_MR_combo__N,
#          MR_alone_DC_alone__N, MR_combo_DC_combo__N,
#          PES_alone_DC_alone__P, PES_alone_DC_combo__P, PES_alone_MR_alone__P, PES_alone_MR_combo__P,
#          MR_alone_DC_alone__P, MR_combo_DC_combo__P, CENSUS_tot_pop, geometry) %>%
#   
#   pivot_longer(PES_alone_DC_alone__N:MR_combo_DC_combo__P, names_to = c("COMPARISON", ".value"), names_sep = "__") %>%
#   select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, NUMERIC_DIFF = N, PERCENT_DIFF = P, CENSUS_tot_pop, geometry) %>%
#   filter(!if_all(c(NUMERIC_DIFF, PERCENT_DIFF), is.na))
#   
# 
# # ==================
# # Handling NA values
# # ==================
# 
# # ISSUES: 
# # 1. Cases in which both populations reported 0, should indicate a 0% change but is instead NA
# # 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
# #    a county but the census reported 0 population)
# 
# analytical <- analytical %>% 
#   mutate(
#     # If MR & census/estimate or census/MR both = 0, then PERCENT_DIFF should be 0
#     PERCENT_DIFF = replace(PERCENT_DIFF, MR == 0 & CENSUS == 0, 0),
#     PERCENT_DIFF = replace(PERCENT_DIFF, ESTIMATE == 0 & CENSUS == 0, 0) ) %>%
#   select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR,COMPARISON, NUMERIC_DIFF, PERCENT_DIFF, CENSUS_tot_pop, geometry)
# 
# # check for NAs
# analytical[is.na(analytical$PERCENT_DIFF),]
# 
# st_write(analytical, "../Transformed Data/PES_DC_MR_comparison_2000.csv", layer_options = "GEOMETRY=AS_WKT")
# 
# # change comparison values
# analytical <- analytical %>%
#   mutate(COMPARISON2 = case_when(
#     COMPARISON == "PES_alone_DC_alone" ~ "PES (alone) - DC (alone)",
#     COMPARISON == "PES_alone_MR_alone" ~ "PES (alone) - MR (alone)",
#     COMPARISON == "MR_alone_DC_alone" ~ "MR (alone) - DC (alone)",
#     COMPARISON == "PES_alone_DC_combo" ~ "PES (alone) - DC (combo)",
#     COMPARISON == "PES_alone_MR_combo" ~ "PES (alone) - MR (combo)",
#     COMPARISON == "MR_combo_DC_combo" ~ "MR (combo) - DC (combo)"
#   )) %>%
#   select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, COMPARISON2, NUMERIC_DIFF, PERCENT_DIFF, CENSUS_tot_pop, geometry)
# 
# 
# # ==========
# # Simple EDA
# # ==========
# 
# 
# # 1. % Diff boxplots by comparison 
# analytical %>%
#   ggplot(aes(x = COMPARISON2, y = PERCENT_DIFF, fill = COMPARISON2)) + 
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# # 2. See all % differences distributions for each comparison 
# ggplot(analytical, aes(x=PERCENT_DIFF)) + 
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_alone_DC_combo"),
#                  aes(color = "green"), fill =NA, alpha = .05,binwidth = 2) +
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_alone_DC_combo"),
#                  color="green", fill =NA, alpha = .05,binwidth = 2) + 
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_alone_MR_combo"),
#                  col = "red" , fill = NA, alpha = .05, binwidth = 2) +
#   geom_histogram(data = analytical %>% filter(COMPARISON == "MR_combo_DC_combo"),
#                  color = "blue", fill = NA, alpha = .05, binwidth = 2) +
#   theme_minimal()  +
#   scale_color_manual(values = c("PES vs DC" = "green", "PES vs. MR" = "red", "MR vs. DC" = "blue"), name = "Comparisons") +
#   ggtitle("Percent Difference Distribution for all Comparisons (API alone AND in combo populations)") 
# 
# 
# # 3. Get % difference distributions for EACH comparison BY race group
# a1 <- analytical %>%
#   filter(COMPARISON == "MR_combo_DC_combo") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("PES alone vs DC alone - % difference by race group")
# 
# analytical %>%
#   filter(RACE_GROUP == "API_alone") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth=3) + 
#   theme_minimal() + 
#   ggtitle("PES alone vs DC alone - % difference by race group")


# ===================================================================================================================================================


########
# 2010 #
########

# read in datasets
# Alabama - Missouri 
mr_al_mo_2010 <- read.csv("../Raw Data/2010/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../Raw Data/2010/modified_race_2010_mt_wy.csv")

# documentation: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/mr2010.pdf

# =========
# Data Prep
# =========

# keep only county geographic summary level
mr_al_mo_2010 <- mr_al_mo_2010[mr_al_mo_2010$SUMLEV == 50, ]
mr_mt_wy_2010 <- mr_mt_wy_2010[mr_mt_wy_2010$SUMLEV == 50, ]

# remove unecessary columns -> sumlev
drop_cols = 1
mr_al_mo_2010 <- mr_al_mo_2010[, -drop_cols]
mr_mt_wy_2010 <- mr_mt_wy_2010[, -drop_cols]


# ===============
# Get Race Groups
# ===============

# Identify which key values in the IMPRACE column are relevant

# Asian Race Groups
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

# NHPI Race Groups
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

# JUST the population info
mr_al_mo_2010 <- mr_al_mo_2010 %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME,CTYNAME, IMPRACE) %>%
  summarise(count = sum(RESPOP))

# JUST the population info
mr_mt_wy_2010 <- mr_mt_wy_2010 %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME, CTYNAME, IMPRACE) %>%
  summarise(count = sum(RESPOP))


# ===============================
# Create AA, AAC, NA, NAC groups
# ===============================

# list of values which contain Asian + another race
keep_imprace_asian <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)

# -----------------
# ASIAN populations 
# -----------------

# 1. 
# Create a DF for asian imprace values only 
mr_al_mo_2010_ASIAN <- mr_al_mo_2010 %>% filter(IMPRACE == 4 | IMPRACE  %in% keep_imprace_asian) %>%
  mutate(RACE = case_when(IMPRACE == 4 ~ "A_A", # Asian Alone
    IMPRACE %in% keep_imprace_asian ~ "A_AIC")) %>% # Asian alone or in combo
  group_by(STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(count)) # get total Asian alone or in combo value 

mr_mt_wy_2010_ASIAN <- mr_mt_wy_2010 %>% filter(IMPRACE == 4 | IMPRACE  %in% keep_imprace_asian) %>%
  mutate(RACE = case_when(IMPRACE == 4 ~ "A_A", # Asian Alone
                          IMPRACE %in% keep_imprace_asian ~ "A_AIC")) %>% # Asian alone or in combo
  group_by(STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(count)) # get total Asian alone or in combo value 

# 2.
# Add alone and AIC values to get alone or in combination 
aic <- mr_al_mo_2010_ASIAN %>% group_by(STNAME, CTYNAME) %>% summarise(aic = sum(MR))
aic$RACE <- "A_AIC"

aic2 <- mr_mt_wy_2010_ASIAN %>% group_by(STNAME, CTYNAME) %>% summarise(aic = sum(MR))
aic2$RACE <- "A_AIC"

# check if counties match 
identical(mr_al_mo_2010_ASIAN$CTYNAME[mr_al_mo_2010_ASIAN$RACE == "A_AIC"], aic$CTYNAME)
identical(mr_mt_wy_2010_ASIAN$CTYNAME[mr_mt_wy_2010_ASIAN$RACE == "A_AIC"], aic2$CTYNAME)

# get all the counties that exist in aic but not in the mr_al_mo_2010_ASIAN DF 
#   this is because some counties do not have any data for Asian race groups other than Asian alone 
missing_counties <- aic$CTYNAME[!(aic$CTYNAME %in% mr_al_mo_2010_ASIAN$CTYNAME[mr_al_mo_2010_ASIAN$RACE == "A_AIC"])]
missing_counties2 <- aic2$CTYNAME[!(aic2$CTYNAME %in% mr_mt_wy_2010_ASIAN$CTYNAME[mr_mt_wy_2010_ASIAN$RACE == "A_AIC"])]

# for each county missing a row where RACE = "A_AIC" 
for (cty in missing_counties){
    # copy the RACE = A_A
    missing_row <- mr_al_mo_2010_ASIAN[mr_al_mo_2010_ASIAN$CTYNAME == cty, ]
    # change the RACE value 
    missing_row$RACE <- "A_AIC"
    # insert the new row back into the DF
    mr_al_mo_2010_ASIAN <- rbind(mr_al_mo_2010_ASIAN, missing_row)
}

# for each county missing a row where RACE = "A_AIC" 
for (cty in missing_counties2){
  # copy the RACE = A_A
  missing_row <- mr_mt_wy_2010_ASIAN[mr_mt_wy_2010_ASIAN$CTYNAME == cty, ]
  # change the RACE value 
  missing_row$RACE <- "A_AIC"
  # insert the new row back into the DF
  mr_mt_wy_2010_ASIAN <- rbind(mr_mt_wy_2010_ASIAN, missing_row)
}

# join the AIC values DF to the mr_al_mo_2010_ASIAN DF
mr_al_mo_2010_ASIAN <- mr_al_mo_2010_ASIAN %>% group_by(STNAME, CTYNAME, RACE) %>%
  left_join(aic, by = c("STNAME", "CTYNAME", "RACE"))

mr_mt_wy_2010_ASIAN <- mr_mt_wy_2010_ASIAN %>% group_by(STNAME, CTYNAME, RACE) %>%
  left_join(aic2, by = c("STNAME", "CTYNAME", "RACE"))

# check for NAs
mr_al_mo_2010_ASIAN[mr_al_mo_2010_ASIAN$RACE != "A_A" & is.na(mr_al_mo_2010_ASIAN$aic), ]
mr_mt_wy_2010_ASIAN[mr_mt_wy_2010_ASIAN$RACE != "A_A" & is.na(mr_mt_wy_2010_ASIAN$aic), ]

# replace MR values where RACE = alone or in combo with the aic value 
mr_al_mo_2010_ASIAN <- mr_al_mo_2010_ASIAN %>% mutate(MR = case_when(RACE == "A_A" ~ MR, RACE == "A_AIC" ~ aic)) %>%
  select(-aic)

mr_mt_wy_2010_ASIAN <- mr_mt_wy_2010_ASIAN %>% mutate(MR = case_when(RACE == "A_A" ~ MR, RACE == "A_AIC" ~ aic)) %>%
  select(-aic)

# -----------------
# NHPI populations 
# -----------------

# list of values which contain native hawaiian/pacific islander + another race
keep_imprce_nhpi <- c(9,12,14,15,18,20,21,23,24,25,27,28,29,30,31)

# 1. 
# Create a DF for NHPI imprace values only 
mr_al_mo_2010_NHPI <- mr_al_mo_2010 %>% filter(IMPRACE == 5 | IMPRACE  %in% keep_imprce_nhpi) %>%
  mutate(RACE = case_when(IMPRACE == 5 ~ "NHPI_A", # Asian Alone
                          IMPRACE %in% keep_imprce_nhpi ~ "NHPI_AIC")) %>% # Asian alone or in combo
  group_by(STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(count)) # get total Asian alone or in combo value 

mr_mt_wy_2010_NHPI <- mr_mt_wy_2010 %>% filter(IMPRACE == 5 | IMPRACE  %in% keep_imprce_nhpi) %>%
  mutate(RACE = case_when(IMPRACE == 5 ~ "NHPI_A", # Asian Alone
                          IMPRACE %in% keep_imprce_nhpi ~ "NHPI_AIC")) %>% # Asian alone or in combo
  group_by(STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(count)) # get total Asian alone or in combo value 

# 2.
# Add alone and AIC values to get alone or in combination 
aic <- mr_al_mo_2010_NHPI %>% group_by(STNAME, CTYNAME) %>% summarise(aic = sum(MR))
aic$RACE <- "NHPI_AIC"

aic2 <- mr_mt_wy_2010_NHPI %>% group_by(STNAME, CTYNAME) %>% summarise(aic = sum(MR))
aic2$RACE <- "NHPI_AIC"

# check if counties match 
identical(mr_al_mo_2010_NHPI$CTYNAME[mr_al_mo_2010_NHPI$RACE == "NHPI_AIC"], aic$CTYNAME)
identical(mr_mt_wy_2010_NHPI$CTYNAME[mr_mt_wy_2010_NHPI$RACE == "NHPI_AIC"], aic2$CTYNAME)

# get all the counties that exist in aic but not in the mr_al_mo_2010_ASIAN DF 
#   this is because some counties do not have any data for Asian race groups other than Asian alone 
missing_counties <- aic$CTYNAME[!(aic$CTYNAME %in% mr_al_mo_2010_NHPI$CTYNAME[mr_al_mo_2010_NHPI$RACE == "NHPI_AIC"])]
missing_counties2 <- aic2$CTYNAME[!(aic2$CTYNAME %in% mr_mt_wy_2010_NHPI$CTYNAME[mr_mt_wy_2010_NHPI$RACE == "NHPI_AIC"])]


# for each county missing a row where RACE = "NHPI_AIC" 
for (cty in missing_counties){
  # copy the RACE = NHPI_A
  missing_row <- mr_al_mo_2010_NHPI[mr_al_mo_2010_NHPI$CTYNAME == cty, ]
  # change the RACE value 
  missing_row$RACE <- "NHPI_AIC"
  # insert the new row back into the DF
  mr_al_mo_2010_NHPI <- rbind(mr_al_mo_2010_NHPI, missing_row)
}

# for each county missing a row where RACE = "NHPI_AIC" 
for (cty in missing_counties2){
  # copy the RACE = NHPI_A
  missing_row <- mr_mt_wy_2010_NHPI[mr_mt_wy_2010_NHPI$CTYNAME == cty, ]
  # change the RACE value 
  missing_row$RACE <- "NHPI_AIC"
  # insert the new row back into the DF
  mr_mt_wy_2010_NHPI <- rbind(mr_mt_wy_2010_NHPI, missing_row)
}

# join the AIC values DF to the mr_al_mo_2010_ASIAN DF
mr_al_mo_2010_NHPI <- mr_al_mo_2010_NHPI %>% group_by(STNAME, CTYNAME, RACE) %>%
  left_join(aic, by = c("STNAME", "CTYNAME", "RACE"))

mr_mt_wy_2010_NHPI <- mr_mt_wy_2010_NHPI %>% group_by(STNAME, CTYNAME, RACE) %>%
  left_join(aic2, by = c("STNAME", "CTYNAME", "RACE"))

# check for NAs
mr_al_mo_2010_NHPI[mr_al_mo_2010_NHPI$RACE != "NHPI_A" & is.na(mr_al_mo_2010_NHPI$aic), ]
mr_mt_wy_2010_NHPI[mr_mt_wy_2010_NHPI$RACE != "NHPI_A" & is.na(mr_mt_wy_2010_NHPI$aic), ]

# replace MR values where RACE = alone or in combo with the aic value 
mr_al_mo_2010_NHPI <- mr_al_mo_2010_NHPI %>% mutate(MR = case_when(RACE == "NHPI_A" ~ MR, RACE == "NHPI_AIC" ~ aic)) %>%
  select(-aic)

mr_mt_wy_2010_NHPI <- mr_mt_wy_2010_NHPI %>% mutate(MR = case_when(RACE == "NHPI_A" ~ MR, RACE == "NHPI_AIC" ~ aic)) %>%
  select(-aic)

# There are counties with only AIC race categories and no "Asian alone" or "NHPI alone" category
# Those will be left alone, not adding a row for alone

# -------------
# Join all DFs
# --------------

mr_2010_tidy <- rbind(mr_al_mo_2010_ASIAN, mr_al_mo_2010_NHPI, mr_mt_wy_2010_ASIAN, mr_mt_wy_2010_NHPI %>% ungroup()) %>%
  arrange(STNAME, CTYNAME)

# write to csv 
write.csv(mr_2010_tidy, "../Transformed Data/2010/MR_county_2010.csv")

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


# ===================================================================================================================================================


########
# 2020 #
########

# read in datasets
mr_2020 <- read.csv("../Raw Data/mrf_2020.csv")


# ===============
# Get Race Groups
# ===============

# Identify which key values in the IMPRACE column are relevant
# (relevant keys determined above in 2010 section)

keep_imprace <- c(4,5,8,9,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

# filter data to keep all rows where imprace has a value in keep_imprace

# ALL_df_name will preserve the hispanic origin info in case we need it
ALL_mr_2020 <- mr_2020 %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(GEOID,IMPRACE,ORIGIN) %>%
  summarise(count = sum(RESPOP))

# JUST the population info
mr_2020 <- mr_2020 %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(GEOID, IMPRACE) %>%
  summarise(count = sum(RESPOP))

# ======================================
# Create AA, AAC, NA, NAC race_group column
# ======================================

# list of values which contain Asian + another race
keep_imprace_asian <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)
# list of values which contain native hawaiian/pacific islander + another race
keep_imprce_nhpi <- c(9,12,14,15,18,20,21,23,24,25,27,28,29,30,31)

mr_2020 <- mr_2020 %>%
  mutate(race_group = case_when(
    IMPRACE == 4 ~ "AA", #Asian American Alone
    IMPRACE %in% keep_imprace_asian ~ "AAC", #Asian American alone or in combination 
    IMPRACE == 5 ~ "NA", #Native Hawaiian & Pacific Islander Alone
    IMPRACE %in% keep_imprce_nhpi ~ "NAC" #Native Hawaiian & Pacific Islander Alone or in combination 
  ))


# ===============================
# Add city and state names 
# ===============================



# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

tw <- get_decennial(
  geography = "county",
  variables = "P1_006N",  # total population in this instance 
  year = 2020)

# split up the NAME column and keep GEOID for merging
tw <- tw %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)") %>%
  select(GEOID, CTYNAME, STNAME)

str(tw)
mr_2020$GEOID <- as.character(mr_2020$GEOID)
mr_2020 <- as.data.frame(mr_2020)

# Get all GEOIDs that are 4 numbers long and add a leading 0 to it 
# This is so that the left join will work with census data 
mr_2020$GEOID[nchar(mr_2020$GEOID) == 4]<- paste0('0', mr_2020$GEOID[nchar(mr_2020$GEOID) == 4])

# Merge FIPS code from census data to MR 2020 data 
mr_2020 <- mr_2020 %>% left_join(tw, by = c("GEOID")) %>%
  select(GEOID, STNAME, CTYNAME, IMPRACE, race_group, MR = count)



# ==============================
# Identify any race trends (EDA)
# ==============================

# BY STATE 
# Get top 5 race groups for each state (by num of residents)
# ---------------------------------------------------------
mr_2020_top_5_race_groups <- mr_2020 %>%
  group_by(STNAME, IMPRACE) %>%
  summarise(population = sum(MR)) %>%
  # get top 5 race groups with most residents per state 
  group_by(STNAME) %>%
  slice_max(order_by = population, n = 5)



# frequency table of top 5 race groups
# -------------------------------------
# convert imprace to factor 
mr_2020_top_5_race_groups$IMPRACE <- as.factor(mr_2010_top_5_race_groups$IMPRACE)
table(mr_2020_top_5_race_groups$IMPRACE)

# top race groups - "A","NA","W + A","W + NA","B + A","B + NA","AI/AN + A","A + NA","W + B + A","W + AI/AN + A","W + A + NA"

# frequency distribution of top 5 race groups
mr_2020_top_5_race_groups %>%
  ggplot(aes(x=IMPRACE)) +
  geom_bar() + 
  # scale_x_discrete(labels = c("A","NA","W + A","W + NA","B + A","B + NA","AI/AN + A","A + NA","W + B + A","W + AI/AN + A","W + A + NA")) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Distribution of top 5 race groups for all states")


dist_race_groups <- mr_2020 %>%
  # remove asian alone so we can visualize the other race groups
  # filter(IMPRACE != 4) %>%
  ggplot(aes(x=as.factor(IMPRACE), y=MR, fill = as.factor(STNAME))) +
  geom_bar(stat = "identity") + 
  # scale_x_discrete(labels = c("NA","W + A","W + NA","B + A","B + NA","AI/AN + A", "AI/AN + NA", "A + NA", "W + B + A",
  #                             "W + B + NA", "W + AI/AN + A", "W + AI/AN + NA", "W + A + NA", "B + AI/AN + A", "B + AI/AN + NA",
  #                             "B + A + NA", "AI/AN + A + NA", "W + B + AI/AN + A", "W + B + AI/AN + NA", "W + B + A + NA",
  #                             "W + AI/NH + A + NA","B + AI/AN + A + NA", "W + B + AI/AN + A + NA")) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Distribution race groups for all states") 

ggplotly(dist_race_groups)

# Race groups with highest population - Asian alone --> White + Asian --> NA  --> Black + Asian --> Asian + NA --> White + NA
highest_top_5_rg <- mr_2020 %>%
  filter(IMPRACE %in% c(4, 8, 5, 11, 15, 9)) %>%
  group_by(STNAME, IMPRACE) %>%
  summarise(population = sum(MR)) %>%
  arrange(desc(population)) %>%
  mutate(race_group = case_when(
    IMPRACE == 4 ~ "Asian alone",
    IMPRACE == 8 ~ "White and Asian",
    IMPRACE == 5 ~ "NHPI alone",
    IMPRACE == 9 ~ "White and NHPI",
    IMPRACE == 11 ~ "Black and Asian",
    IMPRACE == 15 ~ "Asian and NHPI"
  ))


# ================================================================
# Combining all race groups into 4 categories --> AA, AAC, NA, NAC 
# ================================================================

# calculating AAC and NAC 
# AAC = AA + AAC       
# NAC = NA + NAC 

mr_2020 <- mr_2020 %>%
  group_by(STNAME, CTYNAME, race_group) %>%
  summarise(population = sum(MR))

# Create new DF that contains ONLY AIC populations
combo_pop <- mr_2020 %>%
  mutate(race_group = case_when(
    race_group == "AA" ~ "AAC", # if alone, set to AAC (alone + AIC = AIC)
    race_group == "AAC" ~ "AAC",
    race_group == "NA" ~ "NAC",
    race_group == "NAC" ~ "NAC"
  )) %>%
  group_by(STNAME, CTYNAME, race_group) %>%
  summarise(combo_pop = sum(population))


# see all counties with missing race_groups ie. 0 population for a given race group
missing_rg <- anti_join(combo_pop[-4], mr_2020[-4]) %>%
  mutate(population = 0)

# left join combination data with mr data
mr_2020 <- left_join(mr_2020, combo_pop, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'race_group' = 'race_group')) 

# replace combination population values with combo_pop values
mr_2020$population[mr_2020$race_group == "AAC" | mr_2020$race_group == "NAC" ] <- 
  mr_2020$combo_pop[mr_2020$race_group == "AAC" | mr_2020$race_group == "NAC"]

# drop combo_pop column
mr_2020 <- mr_2020[-5]

write.csv(mr_2020, "../Transformed Data/mr_county_2020.csv")


# ===============================================
# Merging modified race data with PES & DC Data
# ===============================================

# read data 
analytical <- read.csv("../Transformed Data/PES_DC_comparison_2020.csv")

# change "variable" column name and values in prep for a left join
colnames(analytical)[4] <- "race_group"
analytical <- analytical %>% mutate(race_group = case_when(
  race_group == "AA_TOT"~ "AA",
  race_group == "AAC_TOT" ~ "AAC",
  race_group == "NA_TOT" ~ "NA",
  race_group == "NAC_TOT" ~ "NAC"
))


analytical <- left_join(analytical, mr_2020, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'race_group' = 'race_group')) %>%
  select(STNAME, CTYNAME, race_group, ESTIMATE, CENSUS, MR = population,NUMERIC_DIFF, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, flag, flag_desc,geometry = WKT)

# Check for NAs due to county naming inconsistencies
dummy <- analytical[is.na(analytical$MR), ] # 4 rows - New mexico dona ana county 

# Entering these 4 missing values 
analytical$MR[analytical$CTYNAME == "Dona Ana County"] <- 
  mr_2020$population[mr_2020$CTYNAME == "Doña Ana County"]


# =========================
# Numeric and % differences
# =========================

analytical <- analytical %>%
  mutate(
    # Modified Race was ___ people higher(or lower) than census results
    NUMERIC_MR_DC = round(MR - CENSUS, 2),
    # Modified Race were __% higher(or lower) than census results
    PERCENT_DIFF_MR_DC = round(((MR - CENSUS) / ((MR + CENSUS) / 2)) * 100 ,2),
    # Estimates were ___ people higher(or lower) than Modified Race results
    NUMERIC_PES_MR = round(ESTIMATE - MR, 2),
    # Estimates were __% higher(or lower) than Modified Raceresults
    PERCENT_DIFF_MR_PES = round(((ESTIMATE - MR) / ((ESTIMATE + MR) / 2)) * 100 ,2)) %>%
  rename(NUMERIC_PES_DC = NUMERIC_DIFF, PERCENT_DIFF_PES_DC = PERCENT_DIFF, RACE_GROUP = race_group)%>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, NUMERIC_PES_DC, PERCENT_DIFF_PES_DC,NUMERIC_MR_DC, PERCENT_DIFF_MR_DC, 
         NUMERIC_PES_MR, PERCENT_DIFF_MR_PES, estim_TOT_POP, census_TOT_POP,flag, flag_desc, geometry)


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
    PERCENT_DIFF_MR_DC = replace(PERCENT_DIFF_MR_DC, MR == 0 & CENSUS == 0, 0),
    PERCENT_DIFF_MR_PES = replace(PERCENT_DIFF_MR_PES, MR == 0 & ESTIMATE == 0, 0)) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, NUMERIC_PES_DC, PERCENT_DIFF_PES_DC,NUMERIC_MR_DC, PERCENT_DIFF_MR_DC, 
         NUMERIC_PES_MR, PERCENT_DIFF_MR_PES, estim_TOT_POP, census_TOT_POP,flag, flag_desc,geometry)


# ============================
# Melt Dataframe - tidy format
# ============================
boxplot(analytical$PERCENT_DIFF_PES_DC, analytical$PERCENT_DIFF_MR_DC, analytical$PERCENT_DIFF_MR_PES)

num_diff <- analytical %>% 
  rename(PES_DC = NUMERIC_PES_DC, MR_DC = NUMERIC_MR_DC, PES_MR = NUMERIC_PES_MR) %>%
  pivot_longer(cols = c(PES_DC, MR_DC,PES_MR), 
               names_to = "COMPARISON", values_to = "NUMERIC_DIFF") %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, NUMERIC_DIFF, estim_TOT_POP, census_TOT_POP, geometry)

perc_diff <- analytical %>% 
  rename(PES_DC = PERCENT_DIFF_PES_DC, MR_DC = PERCENT_DIFF_MR_DC, PES_MR = PERCENT_DIFF_MR_PES) %>%
  pivot_longer(cols = c(PES_DC, MR_DC,PES_MR), 
               names_to = "COMPARISON", values_to = "PERCENT_DIFF") %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, PERCENT_DIFF)

analytical <- left_join(num_diff, perc_diff, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'RACE_GROUP' = 'RACE_GROUP',"MR"="MR",
                                                    "ESTIMATE" = "ESTIMATE", "CENSUS" = "CENSUS", "COMPARISON" = "COMPARISON")) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, NUMERIC_DIFF, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, geometry)


# write to csv
st_write(analytical, "../Transformed Data/PES_DC_MR_comparison_2020.csv", layer_options = "GEOMETRY=AS_WKT")


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
