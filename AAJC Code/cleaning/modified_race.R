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
col_info <- read.csv("../../Raw Data/2000/mr-co-column-info.csv")

# Define column widths 
widths <- col_info$column_widths

# Define column names 
cols <- col_info$column_names  # col names set to "not API" if the race group does not contain Asian/NHPI


mr_2000 <- read_fwf("../../Raw Data/2000/mr-co.txt",
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
mr_al_mo_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_mt_wy.csv")

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






#####################################
# 2010 non Hispanic population only # 
#####################################

# read in datasets
# Alabama - Missouri 
mr_al_mo_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_mt_wy.csv")

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
keep_imprace <- c(4,5,8,9,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

# filter data to keep all rows where imprace has a value in keep_imprace

# JUST the population info
mr_al_mo_2010 <- mr_al_mo_2010 %>% filter(ORIGIN ==1 ) %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(STNAME,CTYNAME, IMPRACE) %>%
  summarise(count = sum(RESPOP))

# JUST the population info
mr_mt_wy_2010 <- mr_mt_wy_2010 %>% filter(ORIGIN ==1 ) %>%
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
write.csv(mr_2010_tidy, "../../Transformed Data/2010/MR_county_2010_non_hispanic.csv")



# ==========
# Simple EDA
# ==========


# # 1. % Diff boxplots by comparison 
# analytical %>%
#   ggplot(aes(x = COMPARISON, y = PERCENT_DIFF, fill = COMPARISON)) + 
#   geom_boxplot()
# 
# # 2. See all % differences distributions for each comparison 
# ggplot(analytical, aes(x=PERCENT_DIFF)) + 
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_DC"),
#                  aes(color = "green"), fill =NA, alpha = .05,binwidth = 2) +
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_DC"),
#                  color="green", fill =NA, alpha = .05,binwidth = 2) + 
#   geom_histogram(data = analytical %>% filter(COMPARISON == "MR_DC"),
#                  col = "red" , fill = NA, alpha = .05, binwidth = 2) +
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_MR"),
#                  color = "blue", fill = NA, alpha = .05, binwidth = 2) +
#   theme_minimal()  +
#   scale_color_manual(values = c("PES vs DC" = "green", "MR vs. DC" = "red", "PES vs. MR" = "blue"), name = "Comparisons") +
#   ggtitle("Percent Difference Distribution for all Comparisons") 
# 
# 
# # 3. Get % difference distributions for EACH comparison BY race group
# analytical %>%
#   filter(COMPARISON == "PES_DC") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("PES vs DC % difference by race group")
# 
# analytical %>%
#   filter(COMPARISON == "MR_DC") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("Modified Race vs DC % difference by race group")
# 
# analytical %>%
#   filter(COMPARISON == "PES_MR") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("PES vs Modified Race % difference by race group")


# ===================================================================================================================================================


########
# 2020 #
########

# read in datasets
mr_2020 <- read.csv("../../Raw Data/2020/mrf_2020.csv") # modified by Chris 


# ===============
# Get Race Groups
# ===============

# Identify which key values in the IMPRACE column are relevant
# (relevant keys determined above in 2010 section)

keep_imprace <- c(4,5,8,9,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)

# filter data to keep all rows where imprace has a value in keep_imprace

# JUST the population info
mr_2020 <- mr_2020 %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(GEOID, IMPRACE) %>%
  summarise(RESPOP = sum(RESPOP))

# ===============================
# Add city and state names 
# ===============================
fips <- read.csv("../Raw Data/2000/county_fips.csv")

# merge with mr_2020 DF
mr_2020 <- left_join(mr_2020 %>% select(fips = GEOID, IMPRACE, RESPOP), fips, by = "fips") %>%
  select(FIPS = fips, STNAME = NAME, CTYNAME, IMPRACE, RESPOP)

# inspecting nas
nas <- mr_2020[rowSums(is.na(mr_2020)) > 0, ]
unique(nas$FIPS)

# new census areas: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
# 2066 - Copper River Census Area, Alaska
# 2063 - Chugach Census Area, Alaska
# 2158 - Kusilvak Census Area, Alaska
# 46102 - Oglala Lakota County, South Dakota

# manually entering missing state and county names 
mr_2020$STNAME[mr_2020$FIPS %in% c(2066, 2063, 2158)] <- "Alaska"
mr_2020$STNAME[mr_2020$FIPS == 46102] <- "South Dakota"
mr_2020$CTYNAME[mr_2020$FIPS == 46102] <- "Oglala Lakota County"
mr_2020$CTYNAME[mr_2020$FIPS == 2066] <- "Copper River Census Area"
mr_2020$CTYNAME[mr_2020$FIPS == 2063] <- "Chugach Census Area"
mr_2020$CTYNAME[mr_2020$FIPS == 2158] <- "Kusilvak Census Area"


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
mr_2020_ASIAN <- mr_2020 %>% filter(IMPRACE == 4 | IMPRACE  %in% keep_imprace_asian) %>%
  mutate(RACE = case_when(IMPRACE == 4 ~ "A_A", # Asian Alone
                          IMPRACE %in% keep_imprace_asian ~ "A_AIC")) %>%
  group_by(FIPS, STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(RESPOP)) # get total Asian alone or in combo value 


# 2.
# Add alone and AIC values to get alone or in combination 
aic <- mr_2020_ASIAN %>% group_by(FIPS, STNAME, CTYNAME) %>% summarise(aic = sum(MR))
aic$RACE <- "A_AIC"


# check if counties match 
identical(mr_2020_ASIAN$CTYNAME[mr_2020_ASIAN$RACE == "A_AIC"], aic$CTYNAME)


# join the AIC values DF to the mr_al_mo_2010_ASIAN DF
mr_2020_ASIAN <- mr_2020_ASIAN %>% group_by(FIPS, STNAME, CTYNAME, RACE) %>%
  left_join(aic, by = c("FIPS", "STNAME", "CTYNAME", "RACE"))

# check for NAs
mr_2020_ASIAN[mr_2020_ASIAN$RACE != "A_A" & is.na(mr_2020_ASIAN$aic), ]

# replace MR values where RACE = alone or in combo with the aic value
mr_2020_ASIAN <- mr_2020_ASIAN %>% mutate(MR = case_when(RACE == "A_A" ~ MR, RACE == "A_AIC" ~ aic)) %>% select(-aic)


# -----------------
# NHPI populations 
# -----------------

# list of values which contain native hawaiian/pacific islander + another race
keep_imprce_nhpi <- c(9,12,14,15,18,20,21,23,24,25,27,28,29,30,31)

# 1. 
# Create a DF for asian imprace values only 
mr_2020_NHPI <- mr_2020 %>% filter(IMPRACE == 5 | IMPRACE  %in% keep_imprce_nhpi) %>%
  mutate(RACE = case_when(IMPRACE == 5 ~ "NHPI_A", # NHPI Alone
                          IMPRACE %in% keep_imprce_nhpi ~ "NHPI_AIC")) %>%
  group_by(FIPS, STNAME, CTYNAME, RACE) %>%
  summarise(MR = sum(RESPOP)) # get total Asian alone or in combo value 


# 2.
# Add alone and AIC values to get alone or in combination 
aic <- mr_2020_NHPI %>% group_by(FIPS, STNAME, CTYNAME) %>% summarise(aic = sum(MR))
aic$RACE <- "NHPI_AIC"


# check if counties match 
identical(mr_2020_NHPI$CTYNAME[mr_2020_NHPI$RACE == "NHPI_AIC"], aic$CTYNAME)


# join the AIC values DF to the mr_al_mo_2010_ASIAN DF
mr_2020_NHPI <- mr_2020_NHPI %>% group_by(FIPS, STNAME, CTYNAME, RACE) %>%
  left_join(aic, by = c("FIPS", "STNAME", "CTYNAME", "RACE"))

# check for NAs
mr_2020_NHPI[mr_2020_NHPI$RACE != "NHPI_A" & is.na(mr_2020_NHPI$aic), ]

# replace MR values where RACE = alone or in combo with the aic value
mr_2020_NHPI <- mr_2020_NHPI %>% mutate(MR = case_when(RACE == "NHPI_A" ~ MR, RACE == "NHPI_AIC" ~ aic)) %>% select(-aic)


# -------------
# Join all DFs
# --------------

mr_2020_tidy <- rbind(mr_2020_ASIAN, mr_2020_NHPI %>% ungroup()) %>%
  arrange(STNAME, CTYNAME)

# write to csv 
write.csv(mr_2020_tidy, "../Transformed Data/2020/MR_county_2020.csv")



# ==============================
# Identify any race trends (EDA)
# ==============================

# # BY STATE 
# # Get top 5 race groups for each state (by num of residents)
# # ---------------------------------------------------------
# mr_2020_top_5_race_groups <- mr_2020 %>%
#   group_by(STNAME, IMPRACE) %>%
#   summarise(population = sum(MR)) %>%
#   # get top 5 race groups with most residents per state 
#   group_by(STNAME) %>%
#   slice_max(order_by = population, n = 5)
# 
# 
# 
# # frequency table of top 5 race groups
# # -------------------------------------
# # convert imprace to factor 
# mr_2020_top_5_race_groups$IMPRACE <- as.factor(mr_2010_top_5_race_groups$IMPRACE)
# table(mr_2020_top_5_race_groups$IMPRACE)
# 
# # top race groups - "A","NA","W + A","W + NA","B + A","B + NA","AI/AN + A","A + NA","W + B + A","W + AI/AN + A","W + A + NA"
# 
# # frequency distribution of top 5 race groups
# mr_2020_top_5_race_groups %>%
#   ggplot(aes(x=IMPRACE)) +
#   geom_bar() + 
#   # scale_x_discrete(labels = c("A","NA","W + A","W + NA","B + A","B + NA","AI/AN + A","A + NA","W + B + A","W + AI/AN + A","W + A + NA")) + 
#   theme(axis.text.x = element_text(angle = 90)) + 
#   ggtitle("Distribution of top 5 race groups for all states")
# 
# 
# dist_race_groups <- mr_2020 %>%
#   # remove asian alone so we can visualize the other race groups
#   # filter(IMPRACE != 4) %>%
#   ggplot(aes(x=as.factor(IMPRACE), y=MR, fill = as.factor(STNAME))) +
#   geom_bar(stat = "identity") + 
#   # scale_x_discrete(labels = c("NA","W + A","W + NA","B + A","B + NA","AI/AN + A", "AI/AN + NA", "A + NA", "W + B + A",
#   #                             "W + B + NA", "W + AI/AN + A", "W + AI/AN + NA", "W + A + NA", "B + AI/AN + A", "B + AI/AN + NA",
#   #                             "B + A + NA", "AI/AN + A + NA", "W + B + AI/AN + A", "W + B + AI/AN + NA", "W + B + A + NA",
#   #                             "W + AI/NH + A + NA","B + AI/AN + A + NA", "W + B + AI/AN + A + NA")) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("Distribution race groups for all states") 
# 
# ggplotly(dist_race_groups)
# 
# # Race groups with highest population - Asian alone --> White + Asian --> NA  --> Black + Asian --> Asian + NA --> White + NA
# highest_top_5_rg <- mr_2020 %>%
#   filter(IMPRACE %in% c(4, 8, 5, 11, 15, 9)) %>%
#   group_by(STNAME, IMPRACE) %>%
#   summarise(population = sum(MR)) %>%
#   arrange(desc(population)) %>%
#   mutate(race_group = case_when(
#     IMPRACE == 4 ~ "Asian alone",
#     IMPRACE == 8 ~ "White and Asian",
#     IMPRACE == 5 ~ "NHPI alone",
#     IMPRACE == 9 ~ "White and NHPI",
#     IMPRACE == 11 ~ "Black and Asian",
#     IMPRACE == 15 ~ "Asian and NHPI"
#   ))
# 
# 
# 
# # ==========
# # Simple EDA
# # ==========
# 
# 
# # 1. % Diff boxplots by comparison 
# analytical %>%
#   ggplot(aes(x = COMPARISON, y = PERCENT_DIFF, fill = COMPARISON)) + 
#   geom_boxplot()
# 
# # 2. See all % differences distributions for each comparison 
# ggplot(analytical, aes(x=PERCENT_DIFF)) + 
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_DC"),
#                  aes(color = "green"), fill =NA, alpha = .05,binwidth = 2) +
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_DC"),
#                  color="green", fill =NA, alpha = .05,binwidth = 2) + 
#   geom_histogram(data = analytical %>% filter(COMPARISON == "MR_DC"),
#                  col = "red" , fill = NA, alpha = .05, binwidth = 2) +
#   geom_histogram(data = analytical %>% filter(COMPARISON == "PES_MR"),
#                  color = "blue", fill = NA, alpha = .05, binwidth = 2) +
#   theme_minimal()  +
#   scale_color_manual(values = c("PES vs DC" = "green", "MR vs. DC" = "red", "PES vs. MR" = "blue"), name = "Comparisons") +
#   ggtitle("Percent Difference Distribution for all Comparisons") 
# 
# 
# # 3. Get % difference distributions for EACH comparison BY race group
# analytical %>%
#   filter(COMPARISON == "PES_DC") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("PES vs DC % difference by race group")
# 
# analytical %>%
#   filter(COMPARISON == "MR_DC") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("Modified Race vs DC % difference by race group")
# 
# analytical %>%
#   filter(COMPARISON == "PES_MR") %>%
#   ggplot(aes(x = PERCENT_DIFF, fill=RACE_GROUP)) + 
#   geom_histogram(binwidth = 3) + 
#   theme_minimal() + 
#   ggtitle("PES vs Modified Race % difference by race group")




# ==========
# Clean Data
# ==========


# all population data is contained within columns 5 - 100
# Melt columns 5 - 100
colnames(mr_2000)[5]
colnames(mr_2000)[128]

mr_2000 <- mr_2000 %>%
  pivot_longer(cols = "not_api...5":"NHL-F-W_B_AINA_A_NHPI", names_to = "RACE_GROUP", values_to = "population") %>%
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

# get population of all races by county
mr_2000_allraces <- mr_2000 %>%
  group_by(FIPS_ST, STNAME, CTYNAME) %>% summarise(population = sum(population))
  
# save it 
write.csv(mr_2000_allraces, "../../Transformed Data/2000/MR_county_all_races_population.csv")








# ===============
# Get Race Groups
# ===============

# JUST the population info all races by county 
mr_al_mo_2010 <- mr_al_mo_2010 %>% group_by(STATE, COUNTY, STNAME, CTYNAME) %>%
  summarise(population = sum(RESPOP))

mr_mt_wy_2010 <- mr_mt_wy_2010 %>% group_by(STATE, COUNTY, STNAME, CTYNAME) %>%
  summarise(population = sum(RESPOP))

mr_2010_allraces <- rbind(mr_al_mo_2010, mr_mt_wy_2010)

# save it 
write.csv(mr_2010_allraces, "../../Transformed Data/2010/MR_county_all_races_population.csv")




# 2020 

# ===============
# Get Race Groups
# ===============

mr_2020 <- mr_2020 %>% group_by(GEOID) %>% summarise(population = sum(RESPOP))


# ===============================
# Add city and state names 
# ===============================
fips <- read.csv("../../Raw Data/2000/county_fips.csv")

# merge with mr_2020 DF
mr_2020 <- left_join(mr_2020 %>% select(fips = GEOID, population), fips, by = "fips") %>%
  select(FIPS = fips, STNAME = NAME, CTYNAME, population)

# inspecting nas
nas <- mr_2020[rowSums(is.na(mr_2020)) > 0, ]
unique(nas$FIPS)

# new census areas: https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
# 2066 - Copper River Census Area, Alaska
# 2063 - Chugach Census Area, Alaska
# 2158 - Kusilvak Census Area, Alaska
# 46102 - Oglala Lakota County, South Dakota

# manually entering missing state and county names 
mr_2020$STNAME[mr_2020$FIPS %in% c(2066, 2063, 2158)] <- "Alaska"
mr_2020$STNAME[mr_2020$FIPS == 46102] <- "South Dakota"
mr_2020$CTYNAME[mr_2020$FIPS == 46102] <- "Oglala Lakota County"
mr_2020$CTYNAME[mr_2020$FIPS == 2066] <- "Copper River Census Area"
mr_2020$CTYNAME[mr_2020$FIPS == 2063] <- "Chugach Census Area"
mr_2020$CTYNAME[mr_2020$FIPS == 2158] <- "Kusilvak Census Area"


# save it 
write.csv(mr_2020, "../../Transformed Data/2020/MR_county_all_races_population_2020.csv")
