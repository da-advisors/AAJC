library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
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





#######
# 2010
#######

# read in dataset 
mod_race_2010 <- read.csv("../Data/modified_race_2010.csv")

# ---------
# Data Prep
# ---------

# keep only county geographic summary level
mod_race_2010 <- mod_race_2010[mod_race_2010$SUMLEV == 50, ]

# remove unecessary columns -> State FIPS code and State name
drop_cols = c(2, 4)
mod_race_2010 <- mod_race_2010[, -drop_cols]

# Identify which key values in the IMPRACE column are relevant

# Asian Alone - 4
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

keep_imprace <- c(4,8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)

# filter data to keep all rows where imprace has a value in keep_imprace
    # Removed age groups and sex columns so that we have a true total count
    # Can bring back these columns if we need age/sex breakdowns

mod_race_2010 <- mod_race_2010[-c(1,4,5,6)] %>%
  filter(IMPRACE %in% keep_imprace) %>%
  group_by(CTYNAME, IMPRACE) %>%
  summarise(count = sum(RESPOP))

# Create new DF to specify "Asian Alone" pop. and "Asian or in Combination" pop.
mod_race_2010_final <- mod_race_2010

# Values in column IMPRACE that == Asian Alone pop.
AA_imprace <- 4

# Values in column IMPRACE that == Asian Alone or in Combination pop.
    # NOTE - we are summing ALL rows where IMPRACE has asian + some other race(s)
    #         we can break down into which specific other race(s) if needed

AAC_imprace <- c(8,11,13,15,17,19,21,22,24,25,26,28,29,30,31)

mod_race_2010_final <- mod_race_2010_final %>%
  # set race_type to AA_alone or AA_combination
  mutate(race_type = case_when(
    IMPRACE == 4 ~ "AA_alone",
    IMPRACE %in% AAC_imprace ~ "AA_combination"
  )) %>%
  # sum by race_type
  group_by(CTYNAME, race_type) %>%
  summarise(population = sum(count))


# ---------------------------------------------
# Adding geospatial data to mod_race_2010_final
# ---------------------------------------------

# Anam's Census API Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# Use tidycensus to get geo data
geospatial_county_data <- get_decennial(
  geography = "county",
  variables = c("P003005","P006005"), # AA_alone & AA_combination vars
  resolution = "20m",
  geometry = T,
  summary_var = "P003001",  # total population
  year = 2010) %>%
  shift_geometry()

head(geospatial_county_data)


# drop unnecessary columns
# Can add these back if we want to compare modified race with decennial census values
geospatial_county_data <- geospatial_county_data[-c(3,4)]

# Remove state names from NAME column
geospatial_county_data$NAME <- gsub("(.*),.*", "\\1", geospatial_county_data$NAME)

# verify that ALL county names in mod_race_2010_final are present in geospatial_county_data
sum(mod_race_2010_final$CTYNAME %in% geospatial_county_data$NAME) == length(mod_race_2010_final$CTYNAME)

# Change column name in preperation for left join
colnames(mod_race_2010_final)[1] <- "NAME"

# left join geospatial_county_data & mod_race_2010_final
geospatial_county_data <- geospatial_county_data %>% left_join(mod_race_2010_final, by = "NAME")

# State boundaries overlay 
state_overlay <- states(
  cb = TRUE,
  resolution = "20m",
  year = 2000
) %>%
  shift_geometry()

# Drop Puerto Rico from state overlay 
state_overlay <- state_overlay[!state_overlay$STATE == 72, ]


# Shifting Alaska & Hawaii on US Map 
#   Link - geometric operations on vector data 
#   https://geocompr.robinlovelace.net/geometric-operations.html

# Isolating Alaska & Hawaii geospatial data 
# alaska <- state_overlay$geometry[state_overlay$NAME == "Alaska"]
# hawaii <- state_overlay$geometry[state_overlay$NAME == "Hawaii"]
# 
# # Shifting alaska and hawaii to the right and down
# alaska.mod = alaska +c(600000, -100000)
# hawaii.mod = hawaii + c(1700000, -199000)
# 
# # updating state_overlay data with our new geometry of alaska & hawaii
# state_overlay$geometry[state_overlay$NAME == "Alaska"] <- alaska.mod
# state_overlay$geometry[state_overlay$NAME == "Hawaii"] <- hawaii.mod

# # Doing the same for the actual decennial census data 
# d2000$geometry[grepl("Alaska", d2000$NAME) ] <- 
#   d2000$geometry[grepl("Alaska", d2000$NAME) ] + c(600000, -100000)
# d2000$geometry[grepl("Hawaii", d2000$NAME) ] <- 
#   d2000$geometry[grepl("Hawaii", d2000$NAME) ] + c(1700000, -199000)


# ------------
# MAPPING DATA   
# ------------

# remove duplicate rows
geospatial_county_data <- geospatial_county_data %>% distinct()

geospatial_county_data[geospatial_county_data$race_type == 'AA_alone', ] %>%
  mutate(percent = 100 * (population/summary_value)) %>%
  mutate(percent_fctr = case_when(
    percent == 0.0 ~ "0%",
    percent > 0.0 & percent < 1.0 ~ "Less than 1%",
    percent >= 1.0 & percent < 25.0 ~ "1 to 25%",
    percent >= 25.0 & percent < 50.0 ~ "25 to 50%",
    percent >= 50.0 ~ "Greater than 50%"
  )) %>%

  ggplot(aes(fill = percent_fctr)) +
  geom_sf(color = "black", size = 0.01) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  theme_AAJC +
  scale_fill_manual(values = c("0%" = "white",
                               "Less than 1%" = "#FFF9F2",
                               "1 to 25%" = "#FAC687",
                               "25 to 50%" = "#BD6D62",
                               "Greater than 50%" = "#6D1162")) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     Asian American US Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

# calc total state population by 

