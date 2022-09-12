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


estim2020 <- read.csv("../Raw Data/estimates_2020_county.csv")


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
  "TOT_POP", 
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
estim2020[7:15] = lapply(estim2020[7:15], FUN = as.integer)

# Creating totals columns (men pop + woman pop)
estim2020 <- estim2020 %>%
  mutate(AA_TOT = AA_MALE + AA_FEMALE,
         NA_TOT = NA_MALE + NA_FEMALE,
         AAC_TOT = AAC_FEMALE + AAC_MALE, 
         NAC_TOT = NAC_MALE + NAC_FEMALE)


##############################
## READ IN 2020 CENSUS DATA ##
##############################

# d2020 <- load_variables(2020, 'pl', cache = T)
# function to get DC data - defined above 

# 2020 vars: 
# Asian Alone pop., NHPI pop.
# For Asian and NHPI combo, we need to get all the race variables with Asian/NHPI + another race and sum them 

# asian variables
vars_2020_a <- c(AA_TOT = "P1_006N",
               AAC = c("P1_013N",'P1_017N','P1_020N','P1_023N','P1_024N','P1_028N','P1_031N','P1_034N','P1_035N','P1_037N',
                       'P1_040N','P1_041N','P1_043N','P1_044N','P1_046N','P1_048N','P1_051N','P1_052N','P1_054N','P1_055N',
                       'P1_057N','P1_058N','P1_059N','P1_061N','P1_062N','P1_064N','P1_065N','P1_067N','P1_068N','P1_069N',
                       'P1_071N'))

# nhpi variables
vars_2020_nhpi <- c(NHPI_TOT = "P1_007N",
                    NAC = c('P1_014N','P1_018N','P1_021N','P1_023N','P1_025N','P1_029N','P1_032N','P1_034N','P1_036N','P1_038N',
          'P1_040N','P1_042N','P1_043N','P1_045N','P1_046N','P1_049N','P1_051N','P1_053N','P1_054N','P1_056N',
          'P1_057N','P1_058N','P1_060N','P1_061N','P1_062N','P1_064N','P1_066N','P1_067N','P1_068N','P1_069N','P1_071N'))

totalPop_2020 <- "P1_001N"

# there is no hispanic variable. only total and not-hispanic. No aggregation needed - will use totals.

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# get data
census2020_a <- get_census_data(vars_2020_a, totalPop_2020, 2020)
census2020_n <- get_census_data(vars_2020_nhpi, totalPop_2020, 2020)

# remove puerto rico 
census2020_a <- census2020_a[!grepl("Puerto Rico", census2020_a$NAME), ]
census2020_n <- census2020_n[!grepl("Puerto Rico", census2020_n$NAME), ]

# pivot_wider so we can sum all of our combination races
census2020_a <- census2020_a %>% pivot_wider(names_from = variable, values_from = value) %>%
  rowwise() %>%
  mutate(AAC_TOT = sum(c_across(AA_TOT:AAC31))) %>%
  select(GEOID, NAME, AA_TOT, AAC_TOT, summary_value)

census2020_n <- census2020_n %>% pivot_wider(names_from = variable, values_from = value) %>%
  rowwise() %>%
  mutate(NAC_TOT = sum(c_across(NHPI_TOT:NAC31))) %>%
  select(GEOID, NAME, NA_TOT = NHPI_TOT, NAC_TOT, summary_value, geometry)

# merge the Asian and NHPI DFs together 
census2020 <- census2020_n %>% left_join(census2020_a, by = c("GEOID", "NAME")) %>%
  select(GEOID, NAME,AA_TOT,AAC_TOT, NA_TOT, NAC_TOT, census_TOT_POP = summary_value.x, geometry)


# ================
# Calc Percentages
# ================

# estim2010_tidy <- estim2010_tidy %>%
#   mutate(value_perc = round((value/TOT_POP) * 100, 2))
# 
# census2020 <- census2020 %>%
#   mutate(value_perc = round((value/summary_value) * 100, 2))


# ========================================
# Join ESTIM & CENSUS into 1 analytical DF
# ========================================

# splitting up name column into 2 columns - state and county names
census2020 <- census2020 %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

# melting both estimates and census data 
census2020 <- census2020 %>% pivot_longer(cols = "AA_TOT":"NAC_TOT", names_to = "RACE_GROUP", values_to = "CENSUS") %>%
  select(CTYNAME, STNAME, RACE_GROUP, CENSUS, census_TOT_POP,geometry)

estim2020 <- estim2020 %>% pivot_longer(cols = "AA_TOT":"NAC_TOT", names_to = "RACE_GROUP", values_to = "ESTIMATE") %>%
  select(CTYNAME, STNAME, RACE_GROUP, ESTIMATE, TOT_POP)


# merge
analytical <- merge(x = census2020, y = estim2020, by = c("CTYNAME", "STNAME", "RACE_GROUP"),
            all.y = TRUE) %>%
  select(CTYNAME, STNAME, RACE_GROUP, ESTIMATE, CENSUS, estim_TOT_POP = TOT_POP, census_TOT_POP, geometry)


# Doña Ana county,New Mexico error due to special character 
analytical$CENSUS[is.na(analytical$CENSUS)] <- c(2700, 4203, 195, 502)
analytical$CTYNAME[is.na(analytical$census_TOT_POP)] <- rep("Dona Ana County", 4)
analytical$census_TOT_POP[is.na(analytical$census_TOT_POP)] <- rep(219561, 4)


# ==================================================
# Numeric and % differences betw. estimates & census
# ==================================================

analytical <- analytical %>%
  mutate(
    # estimates were ___ people higher(or lower) than census results
    NUMERIC_DIFF = round(ESTIMATE - CENSUS, 2),
    # estimates were __% higher(or lower) than census results
    PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2)) %>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, geometry)

# no calculation for total population difference. It could explain a really high jump in a county if needed


# ==================
# Handling 0% values
# ==================

# ISSUES: 
# 1. Cases in which estimates and census both reported 0, should indicate a 0% change but is instead NA
# 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
#    a county but the census reported 0 population)

analytical <- analytical %>% 
  # If estimate & census both = 0, then PERCENT_DIFF should be 0
  mutate(PERCENT_DIFF = replace(PERCENT_DIFF, ESTIMATE == 0 & CENSUS == 0, 0),
         # Code undefined percentages to 0% 
         PERCENT_DIFF = replace(PERCENT_DIFF, PERCENT_DIFF == Inf, 0),
         
         # Creating a flag for undefined percentages (ie. when census value is 0 but estimate value is not)
         flag = case_when(ESTIMATE > 0 & CENSUS == 0 ~ 1,
                          # no flag if condition not met 
                          TRUE ~ 0),
         flag_desc = case_when(flag == 1 ~ "Undefined % difference (census = 0 & estimate != 0)",
                               TRUE ~ " ")) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF,estim_TOT_POP, census_TOT_POP,
         flag, flag_desc, geometry)

analytical <- analytical %>%
  arrange(STNAME, CTYNAME)

st_write(analytical, "../Transformed Data/PES_DC_comparison_2020.csv", layer_options = "GEOMETRY=AS_WKT")





