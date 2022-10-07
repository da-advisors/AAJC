library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(sf)
source("../AAJC_theme.R")
source("../aajc_tools.R")

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')


########
# 2010 # 
########

# 1.
# READ IN 2010 ESTIMATES DATA 

estim2010 <- read.csv("../../Raw Data/2010/estimates_2010_county.csv")

# documentation for dataset https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/cc-est2010-alldata.pdf


# key for the YEAR variable - 14 
# 14 = 7/1/2010 population estimate (most recent)

# Needed variables:
keepCols <- c(
  "STATE", 
  "COUNTY",
  "STNAME",
  "CTYNAME", 
  "YEAR" ,
  "AGEGRP", 
  "AA_MALE" ,    # Asian alone male population 
  "AA_FEMALE",   # Asian alone female population 
  "NA_MALE" ,
  "NA_FEMALE",
  "AAC_MALE" ,
  "AAC_FEMALE", 
  "NAC_MALE",
  "NAC_FEMALE") 

estim2010 <- estim2010[, keepCols]

str(estim2010)

# get the correct year and drop AGEGRP = 0 (total)
estim2010 <- estim2010 %>%
  filter(YEAR == 14) %>% # 14 = 7/1/2010
  filter(AGEGRP != 0)

# Creating totals columns (men pop + woman pop)
estim2010 <- estim2010 %>%
  mutate(A_A = AA_MALE + AA_FEMALE, # asian alone
         NHPI_A = NA_MALE + NA_FEMALE,  # nhpi alone
         A_AIC = AAC_FEMALE + AAC_MALE, # asian alone or in combo 
         NHPI_AIC = NAC_MALE + NAC_FEMALE) %>% # nhpi alone or in combo 
  select(STATE, COUNTY, STNAME, CTYNAME, AGEGRP, A_A, A_AIC, NHPI_A, NHPI_AIC)

# melt data frame
estim2010 <- estim2010 %>% pivot_longer(cols = 'A_A':'NHPI_AIC', names_to = "RACE", values_to = "ESTIM")


# 2.
# READ IN 2010 MR DATA

# Alabama - Missouri 
mr_al_mo_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_al_mo.csv")
# Montana - Wyoming
mr_mt_wy_2010 <- read.csv("../../Raw Data/2010/modified_race_2010_mt_wy.csv")


# list of race groups which contain Asian + another race & nhpi + another race 
keep_imprace_aic <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)
keep_imprace_nhpi <- c(9,12,14,15,18,20,21,23,24,25,27,28,29,30,31)

# The key for SEX is as follows:
#   1 = Male
#   2 = Female
# The key for ORIGIN is as follows:
#   1 = Not Hispanic
#   2 = Hispanic

# 2a. 
# NHPI SEPERATE DF  
mr_al_mo_2010_nhpi <- mr_al_mo_2010 %>%
  filter(IMPRACE == 5 | IMPRACE %in% keep_imprace_nhpi) %>% 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 5 ~ 'NHPI_A', #alone
    IMPRACE %in% keep_imprace_nhpi ~ 'NHPI_AIC' #alone or in combo
  )) %>%
  
  # get total populations by race group 
  group_by(STATE, COUNTY,STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))

# Asian seperate DF 
mr_al_mo_2010_asian <- mr_al_mo_2010 %>%
  filter(IMPRACE == 4 | IMPRACE %in% keep_imprace_aic) %>% 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 4 ~ 'A_A', #alone
    IMPRACE %in% keep_imprace_aic ~ 'A_AIC' #alone or in combo
  )) %>%
  
  # get total populations by race group 
  group_by(STATE, COUNTY,STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))


# 3.
# Add alone and AIC values to get alone or in combination 
aic <- mr_al_mo_2010_asian %>% group_by(STATE, COUNTY,STNAME, CTYNAME,AGEGRP) %>% summarise(aic = sum(MR))
aic$RACE_GROUP <- "A_AIC"

# nhpi
aic2 <- mr_al_mo_2010_nhpi %>% group_by(STATE, COUNTY,STNAME, CTYNAME,AGEGRP) %>% summarise(aic = sum(MR))
aic2$RACE_GROUP <- "NHPI_AIC"

# join the AIC values DF to the mr_al_mo_2010_ASIAN DF
mr_al_mo_2010_asian <- mr_al_mo_2010_asian %>% group_by(STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  left_join(aic, by = c("STATE", "COUNTY", "STNAME", "CTYNAME", "AGEGRP","RACE_GROUP"))

mr_al_mo_2010_nhpi <- mr_al_mo_2010_nhpi %>% group_by(STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  left_join(aic2, by = c("STATE", "COUNTY", "STNAME", "CTYNAME", "AGEGRP","RACE_GROUP"))

# check for NAs
mr_al_mo_2010_asian[mr_al_mo_2010_asian$RACE_GROUP != "A_A" & is.na(mr_al_mo_2010_asian$aic), ]
mr_al_mo_2010_nhpi[mr_al_mo_2010_nhpi$RACE_GROUP != "NHPI_A" & is.na(mr_al_mo_2010_nhpi$aic), ]

# replace MR values where RACE = alone or in combo with the aic value 
mr_al_mo_2010_asian <- mr_al_mo_2010_asian %>% mutate(MR = case_when(RACE_GROUP == "A_A" ~ MR, RACE_GROUP == "A_AIC" ~ aic)) %>%
  select(-aic)

mr_al_mo_2010_nhpi <- mr_al_mo_2010_nhpi %>% mutate(MR = case_when(RACE_GROUP == "NHPI_A" ~ MR, RACE_GROUP == "NHPI_AIC" ~ aic)) %>%
  select(-aic)


# 4. 
# combine nhpi and asian data 
mr_al_mo_2010_combined <- mr_al_mo_2010_nhpi %>% left_join(mr_al_mo_2010_asian,  by = c("STATE", "COUNTY",
                                                                                        "STNAME", "CTYNAME", 
                                                                                        "AGEGRP")) %>%
  pivot_wider(names_from = RACE_GROUP.x, values_from = MR.x) %>%
  pivot_wider(names_from = RACE_GROUP.y, values_from = MR.y) %>%
  # NAs introduced because NHPI does not have residents in ALL age groups neither does Asian for some counties
  select(STATE, COUNTY, STNAME, CTYNAME, AGEGRP, A_A, A_AIC, NHPI_A, NHPI_AIC) %>%
  pivot_longer(cols = 'A_A':'NHPI_AIC', names_to = "RACE", values_to = "MR")


# 5.
# OTHER SET OF STATES
# -------------------
# NHPI SEPERATE DF  
mr_mt_wy_2010_nhpi <- mr_mt_wy_2010 %>%
  filter(IMPRACE == 5 | IMPRACE %in% keep_imprace_nhpi) %>% 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 5 ~ 'NHPI_A', #alone
    IMPRACE %in% keep_imprace_nhpi ~ 'NHPI_AIC' #alone or in combo
  )) %>%
  
  # get total populations by race group 
  group_by(STATE, COUNTY,STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))

# Asian seperate DF 
mr_mt_wy_2010_asian <- mr_mt_wy_2010 %>%
  filter(IMPRACE == 4 | IMPRACE %in% keep_imprace_aic) %>% 
  
  # create race group col to define A and AIC
  mutate(RACE_GROUP = case_when(
    IMPRACE == 4 ~ 'A_A', #alone
    IMPRACE %in% keep_imprace_aic ~ 'A_AIC' #alone or in combo
  )) %>%
  
  # get total populations by race group 
  group_by(STATE, COUNTY,STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  summarise(MR = sum(RESPOP))


# 6.
# Add alone and AIC values to get alone or in combination 
aic <- mr_mt_wy_2010_asian %>% group_by(STATE, COUNTY,STNAME, CTYNAME,AGEGRP) %>% summarise(aic = sum(MR))
aic$RACE_GROUP <- "A_AIC"

# nhpi
aic2 <- mr_mt_wy_2010_nhpi %>% group_by(STATE, COUNTY,STNAME, CTYNAME,AGEGRP) %>% summarise(aic = sum(MR))
aic2$RACE_GROUP <- "NHPI_AIC"

# join the AIC values DF to the mr_al_mo_2010_ASIAN DF
mr_mt_wy_2010_asian <- mr_mt_wy_2010_asian %>% group_by(STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  left_join(aic, by = c("STATE", "COUNTY", "STNAME", "CTYNAME", "AGEGRP","RACE_GROUP"))

mr_mt_wy_2010_nhpi <- mr_mt_wy_2010_nhpi %>% group_by(STNAME, CTYNAME, AGEGRP, RACE_GROUP) %>%
  left_join(aic2, by = c("STATE", "COUNTY", "STNAME", "CTYNAME", "AGEGRP","RACE_GROUP"))

# check for NAs
mr_mt_wy_2010_asian[mr_mt_wy_2010_asian$RACE_GROUP != "A_A" & is.na(mr_mt_wy_2010_asian$aic), ]
mr_mt_wy_2010_nhpi[mr_mt_wy_2010_nhpi$RACE_GROUP != "NHPI_A" & is.na(mr_mt_wy_2010_nhpi$aic), ]

# replace MR values where RACE = alone or in combo with the aic value 
mr_mt_wy_2010_asian <- mr_mt_wy_2010_asian %>% mutate(MR = case_when(RACE_GROUP == "A_A" ~ MR, RACE_GROUP == "A_AIC" ~ aic)) %>%
  select(-aic)

mr_mt_wy_2010_nhpi <- mr_mt_wy_2010_nhpi %>% mutate(MR = case_when(RACE_GROUP == "NHPI_A" ~ MR, RACE_GROUP == "NHPI_AIC" ~ aic)) %>%
  select(-aic)


# combine nhpi and asian data 
mr_mt_wy_2010_combined <- mr_mt_wy_2010_nhpi %>% left_join(mr_mt_wy_2010_asian,  by = c("STATE", "COUNTY",
                                                                                        "STNAME", "CTYNAME", 
                                                                                        "AGEGRP")) %>%
  pivot_wider(names_from = RACE_GROUP.x, values_from = MR.x) %>%
  pivot_wider(names_from = RACE_GROUP.y, values_from = MR.y) %>%
  # NAs introduced because NHPI does not have residents in ALL age groups neither does Asian for some counties
  select(STATE, COUNTY, STNAME, CTYNAME, AGEGRP, A_A, A_AIC, NHPI_A, NHPI_AIC) %>%
  pivot_longer(cols = 'A_A':'NHPI_AIC', names_to = "RACE", values_to = "MR")

# 2b. 
# MERGE ALABAMA-MISSOURI DATA AND MONTANA-WYOMING DATA
mr_county_agegrp <- rbind(mr_al_mo_2010_combined, mr_mt_wy_2010_combined)


# 3. 
# JOIN ESTIMATES AND MODIFIED RACE DATA

analytical <- estim2010 %>% left_join(mr_county_agegrp, by = c("STATE", "COUNTY", "STNAME",
                                                               "CTYNAME","AGEGRP", "RACE"))

# inspect NAs
nas <- analytical[rowSums(is.na(analytical)) > 0, ] # rows where there were no reported residents for a given race category 

# setting NAs = 0 (represents 0 residents reported)
analytical$MR[rowSums(is.na(analytical)) > 0] <- 0

# 4.
# NUMERIC + PERCENT DIFFERENCE CALCULATIONS
analytical <- analytical %>% mutate(NUM_DIFF = MR - ESTIM,   # numeric diff
                                    PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),   # percent difference/error of closure (EOC)
                                    COVERAGE = case_when(
                                      NUM_DIFF < 0 ~ 'undercount',
                                      NUM_DIFF > 0 ~ 'overcount',
                                      NUM_DIFF == 0 ~ 'equal'
                                    ))

# inspect NAs
nas <- analytical[rowSums(is.na(analytical)) > 0, ] 
# percent diff = NaN because of 0 denominator these should all be set to 0
analytical$PERC_DIFF[rowSums(is.na(analytical)) > 0] <- 0

# write to csv
write.csv(analytical, "../../Transformed Data/2010/ES_MR_AGEGRP_comparison_2010.csv")

