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


# ========
# get data 
# ========

analytical <- read.csv("../Transformed Data/estimates_census_comparison_2010.csv")



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
analytical$geometry[grepl("Alaska", analytical$STNAME)] <- 
  analytical$geometry[grepl("Alaska", analytical$STNAME)] + c(675000, -400000)
analytical$geometry[grepl("Hawaii", analytical$STNAME) ] <- 
  analytical$geometry[grepl("Hawaii", analytical$STNAME) ]+ c(1700000, -500000)



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


