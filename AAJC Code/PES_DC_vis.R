library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(sf)
source("AAJC_theme.R")
source("aajc_tools.R")
library(rcartocolor) # "PurpOr" color pallete 

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

options(tigris_use_cache = TRUE)

############
##  2000  ##
############

# ========
# get data 
# ========

# analytical <- read.csv("../Transformed Data/PES_DC_MR_comparison_2010.csv")
analytical <- read.csv("../Transformed Data/PES_DC_MR_comparison_2000.csv")

geo <- read.csv("../Transformed Data/PES_DC_comparison_2000.csv")

geo <- geo %>% select(WKT, STNAME, CTYNAME)

analytical <- analytical %>%
  select(-geometry) %>%
  left_join(geo, by = c("STNAME", "CTYNAME")) %>%
  rename(geometry = WKT)


#############################
## PERCENT DIFFERENCE MAPS ##
#############################

# =======================
# State Overlay + AK & HI
# =======================

# STATE boundary overlap data (makes it look cleaner)
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
alaska <- state_overlay$geometry[state_overlay$NAME == "Alaska"]
hawaii <- state_overlay$geometry[state_overlay$NAME == "Hawaii"]

# Shifting alaska and hawaii to the right and down
alaska.mod = alaska +c(675000, -400000)
hawaii.mod = hawaii + c(1700000, -500000)

# updating state_overlay data with our new geometry of alaska & hawaii
state_overlay$geometry[state_overlay$NAME == "Alaska"] <- alaska.mod
state_overlay$geometry[state_overlay$NAME == "Hawaii"] <- hawaii.mod

# # trial and error to see what looks good
# plot(state_overlay$geometry[state_overlay$NAME != "Alaska"])
# plot(alaska.mod, col="lightblue", add=T)
# plot(hawaii.mod, col="lightblue", add=T)

# Doing the same for the actual data 
# analytical$geometry[grepl("Alaska", analytical$STNAME)] <- 
#   as.numeric(analytical$geometry[grepl("Alaska", analytical$STNAME)]) + c(675000, -400000)
# analytical$geometry[grepl("Hawaii", analytical$STNAME) ] <- 
#   as.numeric(analytical$geometry[grepl("Hawaii", analytical$STNAME) ])+ c(1700000, -500000)




# ================
# Mapping Function
# ================

county_map <- function(data){
  
  map <- data %>%
    ggplot(aes(fill = percent_fctr, geometry = geometry)) +
    geom_sf(color = "black", size = 0.04) +
    geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.15) +
    theme_AAJC +
    scale_fill_manual(values = c("-200 to -125%" = "#FCB03E",
                                 "-125 to -50%" = "#FCDCB6",
                                 "-50 to 0%" = "#FFF9F2",
                                 "0 to 50%" = "#D9998F",
                                 "50 to 125%" = "#BD6D62",
                                 "125 - 200%" = "#6D1162"))
  
  # change margins to fit alaska and hawaii changes
  map <- map + theme(plot.margin = margin(1,1,1,1, "cm"))
  map
}


# ==============================
# Create Map for each Race Group - API Alone & API AIC 
# ==============================

# --------------------
# Fixing Alaska values
# --------------------

# Making sure the comparisons are PES and MR and not PES and DC
analytical$NUMERIC_DIFF[analytical$STNAME == "Alaska"] <-
  analytical$ESTIMATE[analytical$STNAME == "Alaska"] - analytical$MR[analytical$STNAME == "Alaska"]

analytical <- analytical %>%
  mutate(lag_estim = ESTIMATE)  %>%
  fill(lag_estim)
  
analytical$PERCENT_DIFF[analytical$STNAME == "Alaska"] <-
  ( (analytical$lag_estim[analytical$STNAME == "Alaska"] - analytical$MR[analytical$STNAME == "Alaska"]) / ( (analytical$lag_estim[analytical$STNAME == "Alaska"] + analytical$MR[analytical$STNAME == "Alaska"]) / 2) ) * 100 

# changing COMPARISON values for Alaska 
analytical$COMPARISON[analytical$STNAME == "Alaska" & analytical$COMPARISON == "PES_alone_DC_alone"] <- "PES_alone_MR_alone"
analytical$COMPARISON[analytical$STNAME == "Alaska" & analytical$COMPARISON == "PES_alone_DC_combo"] <- "PES_alone_MR_combo"


# multiplying percent_diff & numeric_diff by -1 to fix calculation 8/22
analytical$PERCENT_DIFF <- analytical$PERCENT_DIFF * -1
analytical$NUMERIC_DIFF <- analytical$NUMERIC_DIFF * -1


# --------------------
# Subsetting relevant data
# -------------------- 
# change out COMPARISON == to "PES_alone_MR_alone" or "PES_alone_MR_combo" depending on what is needed 
dummy <-  analytical %>%
  # get each race group
  filter(COMPARISON == "PES_alone_MR_combo") %>%
  # create groups column for discrete color scale
  mutate(percent_fctr = case_when(
    PERCENT_DIFF <= -125 ~ "-200 to -125%",
    PERCENT_DIFF > -125 & PERCENT_DIFF <= -50 ~ "-125 to -50%",
    PERCENT_DIFF > -50 & PERCENT_DIFF <= 0 ~ "-50 to 0%",
    PERCENT_DIFF > 0 & PERCENT_DIFF <= 50 ~ "0 to 50%",
    PERCENT_DIFF > 50 & PERCENT_DIFF < 125 ~ "50 to 125%",
    PERCENT_DIFF >= 125 ~ "125 - 200%",)) %>%
  select(STNAME, CTYNAME, percent_fctr, -geometry)

# ------------
# Getting geospatial data 
# ------------

# Get 2000 geospatial data 
geo <- get_decennial(
  geography = "county",
    geometry = TRUE,
    resolution = "20m",
    variables = "P007006",  # total population in this instance 
    year = 2000) %>%
    shift_geometry()

geo <- geo %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

geo <- geo[, 1:6]
geo <- geo %>% select(STNAME, CTYNAME, geometry)

# fix positioning of alaska and hawaii 
geo$geometry[grepl("Alaska", geo$STNAME)] <- 
  geo$geometry[grepl("Alaska", geo$STNAME)] + c(675000, -400000)
geo$geometry[grepl("Hawaii", geo$STNAME) ] <- 
  geo$geometry[grepl("Hawaii", geo$STNAME) ]+ c(1700000, -500000)


dummy$CTYNAME[dummy$STNAME == "Alaska"] <- geo$CTYNAME[geo$STNAME == "Alaska"]

dummy <- left_join(dummy, geo, by = c("STNAME", "CTYNAME"))

# --------------------
# MAPPING
# --------------------

# API COMBO 
API_combo_2000_county <- county_map(dummy)
API_combo_2000_county +  
  labs(fill = "% difference between Census\n Results and Population Estimates     ",
       title ="      Population Estimates and Census Comparison for API\n      (Alone or in Combination) Populations",
       subtitle = "         Resident Population By County",
       caption = "A percentage difference value of less than 0% indicates \na potential undercount ie. the population estimates for API\n(alone or in combination) were greater than the census results.") +
  titles_upper()

# API ALONE
API_alone_2000_county <- county_map(dummy)
API_alone_2000_county +  
  labs(fill = "% difference between Census\n Results and Population Estimates     ",
                              title ="      Population Estimates and Census Comparison for API Alone Populations",
                              subtitle = "         Resident Population By County",
                              caption = "A percentage difference value of less than 0% indicates \na potential undercount ie. the population estimate for API\n(alone) was less than the census results.") +
  titles_upper()




#########
# STATE #
#########

analytical_state <- read.csv("../Transformed Data/state_level_comparisons_2000.csv")


# multiplying percent_diff & numeric_diff by -1 to fix calculation 8/22
analytical_state$PERCENT_DIFF <- analytical_state$PERCENT_DIFF * -1
analytical_state$NUMERIC_DIFF <- analytical_state$NUMERIC_DIFF * -1

# --------------------
# Subsetting relevant data
# -------------------- 
# change out COMPARISON == to "PES_alone_MR_alone" or "PES_alone_MR_combo" depending on what is needed 
dummy <-  analytical_state %>%
  # get each race group
  filter(COMPARISON == "PES_alone_MR_combo") %>%
  # create groups column for discrete color scale
  mutate(percent_fctr = case_when(
    PERCENT_DIFF < -25 ~ "Less than -25%",
    PERCENT_DIFF >= -25 & PERCENT_DIFF < 0 ~ "-25 to 0%",
    PERCENT_DIFF >= 0 & PERCENT_DIFF < 25 ~ "0 to 25%",
    PERCENT_DIFF >= 25 & PERCENT_DIFF <= 50 ~ "25 to 50%",
    PERCENT_DIFF > 50 ~ "Greater than 50%")) %>%
  select(STNAME, percent_fctr)



# ------------
# Getting geospatial data 
# ------------

# can use state overlay here 
state_overlay_geo <- state_overlay %>% select(STNAME = NAME, geometry)

dummy <- left_join(dummy, state_overlay_geo, by = "STNAME")


# --------------------
# MAPPING
# --------------------

# Color Paletted 
display_carto_pal(7, "PurpOr") #state maps
display_carto_pal(7, "SunsetDark") #county maps / tables
display_carto_pal(7, "Sunset") #county maps
display_carto_pal(7, "OrYel") #state maps, option 2

PurpOr7 <- carto_pal(7, "PurpOr")


# ================
# Mapping Function - STATE 
# ================

state_map <- function(data){
  
  map <- data %>%
    ggplot(aes(fill = percent_fctr, geometry = geometry)) +
    geom_sf(color = "black", size = 0.04) +
    geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.15) +
    theme_AAJC +
    scale_fill_manual(values = c("Less than -25%" = PurpOr7[1],
                                 "-25 to 0%" = PurpOr7[2],
                                 "0 to 25%" = PurpOr7[3],
                                 "25 to 50%" = PurpOr7[4],
                                 "Greater than 50%" = PurpOr7[5]))
  
  # change margins to fit alaska and hawaii changes
  map <- map + theme(plot.margin = margin(1,1,1,1, "cm"))
  map
}



# API COMBO 
API_combo_2000_state <- state_map(dummy)
API_combo_2000_state +  
  labs(fill = "Error of Closure (%)     ",
       title ="      Population Estimates and Census Comparison for API\n      (Alone or in Combination) Populations",
       subtitle = "         Resident Population By State",
       caption = "An error of closure value less than 0% indicates a potential\nundercount ie. the population estimates for API (alone or\nin combination) were greater than the census results.") +
  titles_upper()

# API ALONE
API_alone_2000_state <- state_map(dummy)
API_alone_2000_state +  
  labs(fill = "Error of Closure (%)     ",
       title ="      Population Estimates and Census Comparison for\n      API Alone Populations",
       subtitle = "         Resident Population By State",
       caption = "An error of closure value less than 0% indicates \na potential undercount ie. the population estimate\nfor API (alone) was less than the census results.") +
  titles_upper()












################
# OLD VIS CODE #
################
# A list of our race values (AA, AAC, NHPI, NHPIC)
variables <- unique(analytical$variable)

# for each race group, call the county map function 
for (i in variables){
  dummy <- analytical %>%
    # get each race group
    filter(variable == i) %>%
    # create a f% groups column for discrete color scale
    mutate(percent_fctr = case_when(
      PERCENT_DIFF < 0.0 ~ "Less than 0%",
      PERCENT_DIFF >= 0 & PERCENT_DIFF <= 1 ~ "0 to 1%",
      PERCENT_DIFF > 1 & PERCENT_DIFF <= 25 ~ "1 to 25%",
      PERCENT_DIFF > 25 & PERCENT_DIFF <= 50 ~ "25 to 50%",
      PERCENT_DIFF > 50 & PERCENT_DIFF <= 100 ~ "50 to 100%",
      PERCENT_DIFF > 100 ~ "Greater than 100%",)) %>%
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


