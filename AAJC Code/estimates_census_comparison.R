library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
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

write.csv(census2010, "../Data/census_2010_county.csv")

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
    NUMERIC_DIFF_AS_PERC = round((ESTIMATE - CENSUS) / CENSUS * 100 ,2),
    # estimates show that County X would have __% more(or less) AAC pop. than it actually did in census results 
    PERCENT_DIFF = round(ESTIMATE_PERC - CENSUS_PERC,2)) %>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, NUMERIC_DIFF, NUMERIC_DIFF_AS_PERC,
         ESTIMATE_PERC, CENSUS_PERC, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, geometry)
  
# no calculation for total population difference. It could explain a really high jump in a county if needed


# ==================
# Handling 0% values
# ==================

# ISSUES: 
# 1. Cases in which estimates and census both reported 0, should indicate a 0% change but is instead NA
# 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
#    a county but the census reported 0 population)

analytical <- analytical %>% 
        # If estimate & census both = 0, then NUMERIC_DIFF_AS_PERC should be 0
  mutate(NUMERIC_DIFF_AS_PERC = replace(NUMERIC_DIFF_AS_PERC, ESTIMATE == 0 & CENSUS == 0, 0),
         # Code undefined percentages to 0% 
         NUMERIC_DIFF_AS_PERC = replace(NUMERIC_DIFF_AS_PERC, NUMERIC_DIFF_AS_PERC == Inf, 0),
         
         # Creating a flag for undefined percentages (ie. when census value is 0 but estimate value is not)
         flag = case_when(ESTIMATE > 0 & CENSUS == 0 ~ 1,
                          # no flag if condition not met 
                          TRUE ~ 0),
         flag_desc = case_when(flag == 1 ~ "Undefined % difference (census = 0 & estimate != 0)",
                               TRUE ~ " ")) %>%
  select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, NUMERIC_DIFF, NUMERIC_DIFF_AS_PERC,
         ESTIMATE_PERC, CENSUS_PERC, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, flag, flag_desc, geometry)


# export table to "Transformed Data" folder
write.csv(analytical, "../Transformed Data/estimates_census_comparison_2010.csv", row.names = F)



#############################
## PERCENT DIFFERENCE MAPS ##
#############################


# inspecting the numeric_diff_as_perc column 
summary(analytical$NUMERIC_DIFF_AS_PERC)


# ==================================
# Getting summaries of percent diff.
# ==================================

# A list of our race values (AA, AAC, NHPI, NHPIC)
variables <- unique(analytical$variable)

# get % summaries for each race group
for (i in variables){
  dummy <- analytical %>%
    filter(variable == i)
  
  cat(" --------\n", i, "\n", "--------\n")
  percent_breakdown(dummy$NUMERIC_DIFF_AS_PERC)
  
  # remove the maximum (6000) for display - its a big outlier and makes it hard to see the distribution 
  dummy <- dummy[dummy$NUMERIC_DIFF_AS_PERC != max(dummy$NUMERIC_DIFF_AS_PERC), ]
  hist(abs(dummy$NUMERIC_DIFF_AS_PERC), breaks=50)
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


# =======================
# State Overlay + AK & HI
# =======================

# STATE boundary overlap data (makes it look cleaner)
state_overlay <- states(
  cb = TRUE,
  resolution = "20m",
  year = 2010
) %>%
  shift_geometry()

# Drop Puerto Rico from state overlay 
state_overlay <- state_overlay[!state_overlay$STATE == 72, ]


# Shifting Alaska & Hawaii on US Map 
#   Link - geometric operations on vector data 
#   https://geocompr.robinlovelace.net/geometric-operations.html

# Isolating Alaska & Hawaii geospatial data 
alaska <- state_overlay$geometry[state_overlay$NAME == "Alaska"]
hawaii <- state_overlay$geometry[state_overlay$NAME == "Hawaii"]

# Shifting alaska and hawaii to the right and down
alaska.mod = alaska +c(600000, -100000)
hawaii.mod = hawaii + c(1700000, -199000)

# updating state_overlay data with our new geometry of alaska & hawaii
state_overlay$geometry[state_overlay$NAME == "Alaska"] <- alaska.mod
state_overlay$geometry[state_overlay$NAME == "Hawaii"] <- hawaii.mod

# # trial and error to see what looks good 
# plot(state_overlay$geometry)
# plot(alaska.mod, col="lightblue", add=T)
# plot(hawaii.mod, col="lightblue", add=T)

# Doing the same for the actual data 
analytical$geometry[grepl("Alaska", analytical$STNAME)] <- 
  analytical$geometry[grepl("Alaska", analytical$STNAME)] + c(600000, -100000)
analytical$geometry[grepl("Hawaii", analytical$STNAME) ] <- 
  analytical$geometry[grepl("Hawaii", analytical$STNAME) ] + c(1700000, -199000)



# ================
# Mapping Function
# ================

county_map <- function(data){
  
  map <- data %>%
    ggplot(aes(fill = percent_fctr, geometry = geometry)) +
    geom_sf(color = "black", size = 0.04) +
    geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.15) +
    theme_AAJC +
    scale_fill_manual(values = c("Less than 0%" = "white",
                                 "0 to 1%" = "#FFF9F2",
                                 "1 to 25%" = "#FAC687",
                                 "25 to 50%" = "#BD6D62",
                                 "50 to 100%" = "#6D1162",
                                 "Greater than 100%" = "#4C061D"))
  
  # change margins to fit alaska and hawaii changes
  map <- map + theme(plot.margin = margin(1,1,1,1, "cm"))
  map
}


# ==============================
# Create Map for each Race Group
# ==============================


# for each race group, call the county map function 
for (i in variables){
  dummy <- analytical %>%
    # get each race group
    filter(variable == i) %>%
    # create a f% groups column for discrete color scale
    mutate(percent_fctr = case_when(
      NUMERIC_DIFF_AS_PERC < 0.0 ~ "Less than 0%",
      NUMERIC_DIFF_AS_PERC >= 0 & NUMERIC_DIFF_AS_PERC <= 1 ~ "0 to 1%",
      NUMERIC_DIFF_AS_PERC > 1 & NUMERIC_DIFF_AS_PERC <= 25 ~ "1 to 25%",
      NUMERIC_DIFF_AS_PERC > 25 & NUMERIC_DIFF_AS_PERC <= 50 ~ "25 to 50%",
      NUMERIC_DIFF_AS_PERC > 50 & NUMERIC_DIFF_AS_PERC <= 100 ~ "50 to 100%",
      NUMERIC_DIFF_AS_PERC > 100 ~ "Greater than 100%",)) %>%
    county_map() 
  
  # adding appropriate labels
  if (i == "AA_TOT"){
    dummy <- dummy +
      labs(fill = "% difference between Population\nEstimates and Census Results     ",
           title ="      Percent Difference in 2010 Asian American (alone) Population\n      Estimates Compared to 2010 Decennial Census ",
           subtitle = "         Resident Population By County",
           caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for Asian Americans (only) was less than the census results ie. the Asian\n American (alone) population was greater than estimated in 2010.") +
      titles_upper()
  }
  
  else if (i == "AAC_TOT") {
    dummy <- dummy +
      labs(fill = "% difference between Population\nEstimates and Census Results     ",
           title ="      Percent Difference in 2010 Asian American\n      (alone or in Combination) Population Estimates\n      Compared to 2010 Decennial Census ",
           subtitle = "         Resident Population By County",
           caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for Asian Americans (alone or in combination) was less than the census results ie. the Asian\n American (alone or in combination) population was greater than estimated in 2010.") +
      titles_upper()
  }
  else if (i == "NA_TOT") {
    dummy <- dummy +
      labs(fill = "% difference between Population\nEstimates and Census Results     ",
           title ="      Percent Difference in 2010 Native Hawaiian and Pacific Islander\n      (alone) Population Estimates\n      Compared to 2010 Decennial Census ",
           subtitle = "         Resident Population By County",
           caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for  Native Hawaiian and Pacific Islander (alone) was less than\nthe census results ie. the Native Hawaiian and Pacific Islander (alone)\npopulation was greater than estimated in 2010.") +
      titles_upper()
  }
  else {
    dummy <- dummy +
      labs(fill = "% difference between Population\nEstimates and Census Results     ",
           title ="      Percent Difference in 2010 Native Hawaiian and Pacific Islander\n      (alone or in combination) Population Estimates\n      Compared to 2010 Decennial Census ",
           subtitle = "         Resident Population By County",
           caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for  Native Hawaiian and Pacific Islander (alone or in combination) was less than\nthe census results ie. the Native Hawaiian and Pacific Islander (alone or in combination)\npopulation was greater than estimated in 2010.") +
      titles_upper()
  }
  
  
  ggsave(filename = paste("../AAJC Vis/",i,"_estimates_census_comparison_2010_COUNTY_MAP.png",sep=""),
         plot = dummy, bg = "white")
}


