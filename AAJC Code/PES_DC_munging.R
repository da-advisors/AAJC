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


#################################
## READ IN 2010 ESTIMATES DATA ##
#################################

estim2010 <- read.csv("../Raw Data/estimates_2010_county.csv")


# key for the YEAR variable - 13 or 14 
    # 13 = 4/1/2010 population estimate
    # 14 = 7/1/2010 population estimate (let's use this one since more recent)
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

estim2010 <- estim2010[, keepCols]

str(estim2010)

# get the correct age groups, year, and totals 
estim2010 <- estim2010 %>%
  filter(YEAR == 14) %>%
  filter(AGEGRP == 0)

# Creating totals columns (men pop + woman pop)
estim2010 <- estim2010 %>%
  mutate(AA_TOT = AA_MALE + AA_FEMALE,
         NA_TOT = NA_MALE + NA_FEMALE,
         AAC_TOT = AAC_FEMALE + AAC_MALE, 
         NAC_TOT = NAC_MALE + NAC_FEMALE)


##############################
## READ IN 2010 CENSUS DATA ##
##############################

# d2010 <- load_variables(2010, 'sf1', cache = T)

options(tigris_use_cache = TRUE)

# function to get data
get_census_data <- function (vars, totalPop, year) {
  get_decennial(
    geography = "county",
    variables = vars,
    geometry = TRUE,
    resolution = "20m",
    summary_var = totalPop,  # total population in this instance 
    year = year) %>%
    shift_geometry()
}

# 2010 vars: 
# Asian Alone pop., Asian alone or in combination pop., NHPI pop., NHPI alone or in combo
vars_2010 <- c(AA_TOT = "P003005",
               AAC_TOT = "P006005",
               NA_TOT = "P003006",
               NAC_TOT = "P006006")
totalPop_2010 <- "P003001"

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# get data
census2010 <- get_census_data(vars_2010, totalPop_2010, 2010)

# remove puerto rico 
census2010 <- census2010[!grepl("Puerto Rico", census2010$NAME), ]

# write.csv(census2010, "../Data/census_2010_county.csv")

###################################
## NUMERIC & PERCENT DIFFERENCES ##
###################################

# ===============================
# MELT ESTIM2010 DF - TIDY FORMAT 
# ===============================

# make a copy
estim2010_tidy <- estim2010

# subset needed cols (totals for now)
keepCols <- c("STATE", 
              "COUNTY",
              "STNAME",
              "CTYNAME",
              "TOT_POP",
              "AA_TOT", 
              "NA_TOT",
              "AAC_TOT",
              "NAC_TOT")

estim2010_tidy <- estim2010_tidy[, keepCols]

# melt 
estim2010_tidy <- estim2010_tidy %>%
  pivot_longer(cols = 'AA_TOT':'NAC_TOT', names_to = 'variable', values_to = 'value')

# estim DF and census DF now have the same num of rows  

# ================
# Calc Percentages
# ================

estim2010_tidy <- estim2010_tidy %>%
  mutate(value_perc = round((value/TOT_POP) * 100, 2))

census2010 <- census2010 %>%
  mutate(value_perc = round((value/summary_value) * 100, 2))


# ========================================
# Join ESTIM & CENSUS into 1 analytical DF
# ========================================

# splitting up name column into 2 columns - state and county names
census2010 <- census2010 %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

# rearrange DF columns
estim2010_tidy <- estim2010_tidy %>%
  mutate(estim_value = value,
         estim_TOT_POP = TOT_POP) %>% 
  select(STNAME, CTYNAME, variable, estim_value, estim_TOT_POP, value_perc)


# drop duplicate geometry column
census2010 <- census2010[-2]

# rearrange DF columns
census2010 <- census2010 %>%
  mutate(census_TOT_POP = summary_value,
         census_value = value) %>%
  select(STNAME, CTYNAME, variable, census_value, census_TOT_POP, value_perc,geometry)


# merge
df <- merge(x = estim2010_tidy, y = census2010, by = c("CTYNAME", "STNAME", "variable"),
           all.y = TRUE)

analytical <- df %>%
  # changing col names for better readability 
  mutate(ESTIMATE = estim_value,
         CENSUS = census_value,
         ESTIMATE_PERC = value_perc.x,
         CENSUS_PERC = value_perc.y) %>%
  select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, ESTIMATE_PERC, CENSUS_PERC, estim_TOT_POP, census_TOT_POP,geometry)


# ==================================================
# Numeric and % differences betw. estimates & census
# ==================================================

analytical <- analytical %>%
  mutate(
    # estimates were ___ people higher(or lower) than census results
    NUMERIC_DIFF = round(ESTIMATE - CENSUS, 2),
    # estimates were __% higher(or lower) than census results
    PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2),
    # estimates show that County X would have __% more(or less) AAC pop. than it actually did in census results 
    PERCENT_OF_COUNTY_DIFF = round(ESTIMATE_PERC - CENSUS_PERC,2)) %>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF,
         ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, geometry)
  
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
  select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF,
         ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, flag, flag_desc, geometry)


# export table to "Transformed Data" folder
# write.csv(analytical, "../Transformed Data/estimates_census_comparison_2010.csv", row.names = F)
st_write(analytical, "../Transformed Data/estimates_census_comparison_2010.csv", layer_options = "GEOMETRY=AS_WKT")




# ==================================
# Getting summaries of percent diff.
# ==================================


# inspecting the PERCENT_DIFF column 
summary(analytical$PERCENT_DIFF)

# A list of our race values (AA, AAC, NHPI, NHPIC)
variables <- unique(analytical$variable)

# get % summaries for each race group
for (i in variables){
  dummy <- analytical %>%
    filter(variable == i)
  
  cat(" --------\n", i, "\n", "--------\n")
  percent_breakdown(dummy$PERCENT_DIFF)
  
}


# Sample output for NHPI (alone or in combination) pop: 

# NAC_TOT
#  Population                  Number of US Counties       Percentage of all US counties
# 
#  0%                                   241                           7.67 
#  Between 1 & 25%                      472                           15.0  
#  Between 25 & 50%                     415                           13.2  
#  Between 50 & 75%                     227                           7.22 
#  Greater than 75%                     671                           21.3  
#  Less than 0%                         1106                          35.2  
#  Less than 1%                         11                            0.350





