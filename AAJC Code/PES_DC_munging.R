library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(sf)
source("AAJC_theme.R")
source("aajc_tools.R")

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')


########
# 2000 # 
########

# 1.
# READ IN 2000 ESTIMATES DATA 

estim2000 <- read.csv("../Transformed Data/2000/county_estimates_2000_interpolated.csv")
fips <- read.csv("../Raw Data/2000/county_FIPS_codes.csv")

estim2000 <- left_join(estim2000, fips,  by = c("STNAME","CTYNAME")) %>% 
  select(FIPS, STNAME, CTYNAME, API = api)
  
# inspecting nas
estim2000[rowSums(is.na(estim2000)) > 0, ]

# add FIPS code to Kalawao County HI
estim2000$FIPS[rowSums(is.na(estim2000)) > 0 ] <- 15005

# 2.
# READ IN 2000 MODIFIED RACE DATA 

mr_2000 <- read.csv("../Transformed Data/2000/MR_county_2000_API.csv")
fips <- read.csv("../Raw Data/2000/county_fips.csv")

# drop Puerto Rico 
mr_2000 <- mr_2000 %>% filter(STNAME != "PR")

# Add FIPS codes to MR data 
mr_2000 <- mr_2000 %>% left_join(fips, by = c("STNAME", "CTYNAME")) %>% select(FIPS = fips, STNAME, CTYNAME, RACE, MR)

# inspect NAs 
mr_2000[rowSums(is.na(mr_2000)) > 0, ]

# manually add missing FIPS 
missing_fips <- c(2201, 2201, 2232, 2232, 2280, 2280, 17099, 17099, 35013, 35013, 51560, 51560)
mr_2000$FIPS[rowSums(is.na(mr_2000)) > 0] <- missing_fips


# 3. 
# JOIN ESTIM AND MR DATA
analytical <- estim2000 %>% left_join(mr_2000, by = c("FIPS")) %>%
  select(FIPS, STNAME = STNAME.x, CTYNAME = CTYNAME.y, RACE, ESTIM = API, MR)

# inspect NAs 
analytical[rowSums(is.na(analytical)) > 0, ] # no NAs

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
write.csv(analytical, "../Transformed Data/2000/ES_MR_comparison_2000.csv")

# ====================================================================================================================================



########
# 2010 # 
########

# 1.
# READ IN 2010 ESTIMATES DATA 

estim2010 <- read.csv("../Raw Data/2010/estimates_2010_county.csv")

# documentation for dataset https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/cc-est2010-alldata.pdf


# key for the YEAR variable - 14 
    # 14 = 7/1/2010 population estimate (most recent)

# key for AGEGRP - 0 = Total

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

# get the correct age groups, year, and totals 
estim2010 <- estim2010 %>%
  filter(YEAR == 14) %>%  # 14 = 7/1/2010
  filter(AGEGRP == 0)   # 0 = total 

# Creating totals columns (men pop + woman pop)
estim2010 <- estim2010 %>%
  mutate(A_A = AA_MALE + AA_FEMALE, # asian alone
         NHPI_A = NA_MALE + NA_FEMALE,  # nhpi alone
         A_AIC = AAC_FEMALE + AAC_MALE, # asian alone or in combo 
         NHPI_AIC = NAC_MALE + NAC_FEMALE) # nhpi alone or in combo 


# drop unneeded columns 
estim2010 <- estim2010 %>%
  select(STATE, COUNTY, STNAME, CTYNAME, A_A, A_AIC, NHPI_A, NHPI_AIC) 

# melt data frame
estim2010 <- estim2010 %>% pivot_longer(cols = 'A_A':'NHPI_AIC', names_to = "RACE", values_to = "ESTIM")


# 2.
# READ IN 2010 MR DATA

mr_2010 <- read.csv("../Transformed Data/2010/MR_county_2010.csv")


# 3. 
# JOIN ESTIMATES AND MODIFIED RACE DATA

analytical <- estim2010 %>% left_join(mr_2010, by = c("STNAME", "CTYNAME", "RACE")) %>% select(-X, STfips = STATE, CTYfips = COUNTY)

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
write.csv(analytical, "../Transformed Data/2010/ES_MR_comparison_2010.csv")


# ====================================================================================================================================


#################################
## READ IN 2020 ESTIMATES DATA ##
#################################


estim2020 <- read.csv("../Raw Data/2020/estimates_2020_county.csv")


# key for the YEAR variable - 13
# 13 = 7/1/2020 population estimate
# key for AGEGRP - 0 = Total

# Needed variables:
keepCols <- c(
  "STATE", 
  "COUNTY",
  "STNAME",
  "CTYNAME", 
  "YEAR" ,
  "AGEGRP",
  "AA_MALE" ,
  "AA_FEMALE",
  "NA_MALE" ,
  "NA_FEMALE",
  "AAC_MALE" ,
  "AAC_FEMALE", 
  "NAC_MALE",
  "NAC_FEMALE") 

estim2020 <- estim2020[, keepCols]

# get the correct age groups, year, and totals 
estim2020 <- estim2020 %>%
  filter(YEAR == 13) %>%
  filter(AGEGRP == 0)

# convert numeric fields to int 
str(estim2020)
estim2020[7:14] = lapply(estim2020[7:14], FUN = as.integer)

# Creating totals columns (men pop + woman pop)
estim2020 <- estim2020 %>%
  mutate(A_A = AA_MALE + AA_FEMALE, # asian alone
         NHPI_A = NA_MALE + NA_FEMALE, # nhpi alone
         A_AIC = AAC_FEMALE + AAC_MALE,   # asian alone or in combo 
         NHPI_AIC = NAC_MALE + NAC_FEMALE)  # nhpi alone or in combo 


# drop unneeded columns 
estim2020 <- estim2020 %>%
  select(STATE, COUNTY, STNAME, CTYNAME, A_A, A_AIC, NHPI_A, NHPI_AIC) 

# melt data frame
estim2020 <- estim2020 %>% pivot_longer(cols = 'A_A':'NHPI_AIC', names_to = "RACE", values_to = "ESTIM")


# 2.
# READ IN 2010 MR DATA

mr_2020 <- read.csv("../Transformed Data/2020/MR_county_2020.csv")


# 3. 
# JOIN ESTIMATES AND MODIFIED RACE DATA

analytical <- estim2020 %>% left_join(mr_2020, by = c("STNAME", "CTYNAME", "RACE")) %>% 
  select(FIPS, -X, -STATE, -COUNTY, STNAME, CTYNAME, RACE, ESTIM, MR)

# inspect NAs
nas <- analytical[rowSums(is.na(analytical)) > 0, ] # rows where there were no reported residents for a given race category 

# La Salle Parish, LA is NA due to white space between La and Salle
analytical$FIPS[analytical$STNAME == "Louisiana" & is.na(analytical$FIPS)] <- 22059
analytical$MR[analytical$STNAME == "Louisiana" & is.na(analytical$MR)] <- c(253,289,10,19)

# Dona Ana County 
analytical$FIPS[analytical$STNAME == "New Mexico" & is.na(analytical$FIPS)] <- 35013
analytical$MR[analytical$STNAME == "New Mexico" & is.na(analytical$MR)] <- c(2483, 3444, 123, 326)
analytical$CTYNAME[analytical$FIPS == 35013] <- "Dona Ana County"

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
write.csv(analytical, "../Transformed Data/2020/ES_MR_comparison_2020.csv")


