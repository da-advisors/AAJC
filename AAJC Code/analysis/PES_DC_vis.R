library(tidycensus)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tigris)
library(sf)
source("../AAJC_theme.R")
source("../aajc_tools.R")
library(rcartocolor) # "PurpOr" color pallete 
library(png)
library(grid)
library(gridExtra)
library(imager)
library(OpenImageR)


# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('../theme_AAJC.rds')

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




# ================
# Mapping Function
# ================

display_carto_pal(6, "SunsetDark") #county maps / tables
display_carto_pal(6, "Sunset") #county maps
display_carto_pal(6, "ag_Sunset")

SunsetDark <- carto_pal(6, "SunsetDark")
Sunset <- carto_pal(6, "Sunset")

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


# carto_pal(6, "PurpOr")
# Sunset <- c("#a44360","#e7807d","#f4b191", "#f0b2c1", "#bb69b0" ,"#8c4fa4")

# burgyl <- carto_pal(6, "BurgYl")
# display.brewer.pal(n = 6, name = 'PuOr')
# Sunset <- carto_pal(6, "Sunset")
# 
# Sunset2 <- c('#704F8D', '#B15C86', '#D98790', '#E9AE85', '#E7CE8C', '#F5E1CC')


county_map_sunset <- function(data){
  
  map <- data %>%
    ggplot(aes(fill = percent_fctr, geometry = geometry)) +
    geom_sf(color = "black", size = 0.04) +
    geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.15) +
    theme_AAJC + 
    scale_fill_manual(values = c("-200 to -125%" = Sunset[1],
                                 "-125 to -50%" = Sunset[2],
                                 "-50 to 0%" = Sunset[3],
                                 "0 to 50%" = Sunset[4],
                                 "50 to 125%" = Sunset[5],
                                 "125 - 200%" = Sunset[6]))
    # scale_fill_brewer(palette = "PuOr")
  
  # change margins to fit alaska and hawaii changes
  map <- map + theme(plot.margin = margin(1,1,1,1, "cm"))
  map
}


# ==============================
# 2x2  maps for AA Alone and AA Alone or in combo for 2010 and 2020 Census
# ==============================
# read in data 
comparison_2010 <- read.csv("../../Transformed Data/2010/ES_MR_comparison_2010.csv")
comparison_2020 <- read.csv("../../Transformed Data/2020/ES_MR_comparison_2020.csv")
  

# ------------
# Getting geospatial data 
# ------------
remotes::install_github("walkerke/tidycensus")
geo <- get_decennial(
  geography = "county",
  geometry = TRUE,
  resolution = "20m",
  variables = "P1_001N",  # total population in this instance 
  year = 2020) %>%
  shift_geometry()

geo <- geo %>% separate(NAME, sep = ', ', into = c('CTYNAME', 'STNAME'))
# geo <- geo %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

# geo <- geo %>% select(STNAME, CTYNAME, geometry)

# fix positioning of alaska and hawaii 
geo$geometry[grepl("Alaska", geo$STNAME)] <- 
  geo$geometry[grepl("Alaska", geo$STNAME)] + c(675000, -400000)
geo$geometry[grepl("Hawaii", geo$STNAME) ] <- 
  geo$geometry[grepl("Hawaii", geo$STNAME) ]+ c(1700000, -500000)

geo <- geo %>% select(GEOID,CTYNAME,STNAME,geometry)


# --------------------
# FUNCTION FOR CREATING/SAVING MAPS  
# --------------------

save_county_maps <- function(data, year) {
  
  # list of all race groups 
  all_races <- unique(data$RACE)
  races_titles <- c("Asian (alone)", "Asian (alone or in combination)", "NHPI (alone)", "NHPI (alone or in combination)")
  races_caps <- c("Asian\n(alone)", "Asian\n(alone or in combination)", "NHPI\n(alone)", "NHPI\n(alone or in combination)")
  
  # ------------
  # Getting geospatial data 
  # ------------
  geo <- get_decennial(
    geography = "county",
    geometry = TRUE,
    resolution = "20m",
    variables = if (year == 2010) "P007006" else "P1_001N",  # total population in this instance 
    year = year) %>%
    shift_geometry()
  
  geo <- geo %>% separate(NAME, sep = ', ', into = c('CTYNAME', 'STNAME'))
  
  # fix positioning of alaska and hawaii 
  geo$geometry[grepl("Alaska", geo$STNAME)] <- 
    geo$geometry[grepl("Alaska", geo$STNAME)] + c(675000, -400000)
  geo$geometry[grepl("Hawaii", geo$STNAME) ] <- 
    geo$geometry[grepl("Hawaii", geo$STNAME) ]+ c(1700000, -500000)
  
  # removing ñ from Doña Ana County for merge 
  geo$CTYNAME[geo$CTYNAME == "Doña Ana County"] <- "Dona Ana County"
  
  geo <- geo %>% select(GEOID,CTYNAME,STNAME,geometry)
  
  # --------------------
  # SUBSETTING BY RACE  
  # --------------------
  for (i in seq_along(all_races)) {
    dummy <- data %>% filter(RACE == all_races[i]) %>% 
      # create a new column for percent different (as factor)
      mutate(percent_fctr = case_when(
        PERC_DIFF <= -125 ~ "-200 to -125%",
        PERC_DIFF > -125 & PERC_DIFF <= -50 ~ "-125 to -50%",
        PERC_DIFF > -50 & PERC_DIFF <= 0 ~ "-50 to 0%",
        PERC_DIFF > 0 & PERC_DIFF <= 50 ~ "0 to 50%",
        PERC_DIFF > 50 & PERC_DIFF < 125 ~ "50 to 125%",
        PERC_DIFF >= 125 ~ "125 - 200%",))
    
    # join geometry data for mapping 
    dummy <- left_join(dummy, geo, by=c("STNAME", "CTYNAME"))
    
    # --------------------
    # MAPPING
    # --------------------
    sunset_county <- county_map_sunset(dummy)
    sunset_county <- sunset_county +  
      labs(fill = "EOC (%)",
           title =paste0("      Population Estimates and Census Comparison for\n      ",races_titles[i], " Populations"),
           subtitle = paste0("         Resident Population By County - ",year),
           caption = paste0("A percentage difference value of less than 0% indicates \na potential undercount ie. the population estimate for", 
                            races_caps[i]," was less than the census results.")) +
      titles_upper()
    
    ggsave(filename = paste("../../AAJC Vis/county_maps/county_map_",all_races[i],"_", year,".png",sep=""),
           plot = sunset_county, bg = "white", width = 9, height = 7)
  }
  
}

# call function 
save_county_maps(comparison_2010, 2010)
save_county_maps(comparison_2020, 2020)




comparison_2010 %>% filter(STNAME == "California", RACE == "A_A") %>% arrange(desc(PERC_DIFF))









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
  year = 2010) %>%
  shift_geometry()

geo <- geo %>% separate(NAME, sep = ', ', into = c('CTYNAME', 'STNAME'))
# geo <- geo %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

# geo <- geo %>% select(STNAME, CTYNAME, geometry)

# fix positioning of alaska and hawaii 
geo$geometry[grepl("Alaska", geo$STNAME)] <- 
  geo$geometry[grepl("Alaska", geo$STNAME)] + c(675000, -400000)
geo$geometry[grepl("Hawaii", geo$STNAME) ] <- 
  geo$geometry[grepl("Hawaii", geo$STNAME) ]+ c(1700000, -500000)

geo <- geo %>% select(GEOID,CTYNAME,STNAME,geometry)
# dummy$CTYNAME[dummy$STNAME == "Alaska"] <- geo$CTYNAME[geo$STNAME == "Alaska"]

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

# ------
#  2000
# ------
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

api2010_breaks <- c("Less than -25%" = PurpOr7[1],
                    "-25 to 0%" = PurpOr7[2],
                    "0 to 25%" = PurpOr7[3],
                    "25 to 50%" = PurpOr7[4],
                    "Greater than 50%" = PurpOr7[5])

state_map <- function(data, breaks){
  
  map <- data %>%
    ggplot(aes(fill = percent_fctr, geometry = geometry)) +
    geom_sf(color = "black", size = 0.04) +
    geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.15) +
    theme_AAJC +
    scale_fill_manual(values = breaks, na.value = "grey") # pass in a list of breaks like above 
  
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




# ------
#  2010
# ------

# analytical_state_10 <- read.csv("../Transformed Data/state_level_comparisons_2010.csv")
analytical_state_10_age <- read.csv("../../Transformed Data/2010/state_level_comparisons_2010_by_agegrp.csv")

# --------------------
# Subsetting relevant data
# -------------------- 
# change out RACE == to "A" or "AIC" depending on what is needed 
# dummy <-  analytical_state_10_age %>%
#   # get each race group
#   filter(RACE == "A") %>%
#   # create groups column for discrete color scale
#   mutate(percent_fctr = case_when(
#     EOC < -125 ~ "Less than -125%",
#     EOC >= -125 & EOC < -120 ~ "-125 to -120%",
#     EOC >= -120 & EOC < -115 ~ "-120 to -115%",
#     EOC >= -115 & EOC <= -110 ~ "-115 to -110%",
#     EOC > -110 ~ "Greater than -110%"))
# 
# # ------------
# # Getting geospatial data 
# # ------------
# 
# # can use state overlay here 
# state_overlay_geo <- state_overlay %>% select(STNAME = NAME, geometry)
# 
# dummy <- left_join(dummy, state_overlay_geo, by = "STNAME")
# 
# 
# # --------------------
# # MAPPING
# # --------------------
# 
# # breaks 
# a2010_breaks <- c("Less than -125%" = PurpOr7[1],
#                   "-125 to -120%" = PurpOr7[2],
#                   "-120 to -115%" = PurpOr7[3],
#                   "-115 to -110%" = PurpOr7[4],
#                   "Greater than -110%" = PurpOr7[5])
# 
# # Asian Alone - A 
# A_2010_state <- state_map(dummy, a2010_breaks)
# A_2010_state +  
#   labs(fill = "Error of Closure (%)     ",
#        title ="      Population Estimates and Census Comparison\n      for Asian (Alone) Populations - 2010",
#        subtitle = "         Resident Population By State",
#        caption = "An error of closure value less than 0% indicates a potential\nundercount ie. the estimates for Asian (alone) populations\nwere greater than the census results.") +
#   titles_upper()
# 
# # Asian Alone or in Combination - AIC
# AIC_2010_state <- state_map(dummy, a2010_breaks)
# AIC_2010_state +  
#   labs(fill = "Error of Closure (%)     ",
#        title ="      Population Estimates and Census Comparison for\n      Asian (Alone or in Combination) Populations - 2010",
#        subtitle = "         Resident Population By State",
#        caption = "An error of closure value less than 0% indicates a potential\nundercount ie. the population estimate for API (alone or in\ncombination) was less than the census results.") +
#   titles_upper()
# 



# ==============================
# AGE GROUP DIFFERENTIATION MAPS 
# ==============================

state_overlay_geo <- state_overlay %>% select(STNAME = NAME, geometry)

agegrp_labels <- c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                   "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 or older")


state_agegrp_imgs <- c()

# FOR NHPI - some MR values are NA because that information was not available for certain age groups
# and certain race groups (alone)
#   - North Dakota: No MR data for NHPI populations in age group 17 (alone), 18 (alone and AIC), 14 (alone)
#   - Vermont: No MR data for NHPI populations in age group 18 (alone and AIC), 16 (alone), 17 (alone)
#   - DC: No MR data for NHPI populations in age group 18 (alone) 
#   - Maine: No MR data for NHPI populations in age group 15 (alone)
#   - New Hampshire: No MR data for NHPI populations in age group 16 (alone)

analytical_state_10_age[is.na(analytical_state_10_age$EOC) & analytical_state_10_age$RACE == "AIC", ]
analytical_state_10_age[is.na(analytical_state_10_age$EOC) & analytical_state_10_age$RACE == "A", ]

# for each age group
for (i in unique(analytical_state_10_age$AGEGRP)){
  # --------------------
  # Subsetting relevant data
  # -------------------- 
  dummy_age <- analytical_state_10_age %>% filter(RACE == "A") %>% filter(AGEGRP == i)
  
  min_i <- min(dummy_age$EOC)
  max_i <- max(dummy_age$EOC)
  
  splits <- as.integer(seq(min_i, max_i, length.out = 4))
  
  dummy_age <- dummy_age %>% 
    mutate(percent_fctr = case_when(
      EOC < splits[1] ~ paste0("Less than ",splits[1],"%"),
      EOC >= splits[1] & EOC < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
      EOC >= splits[2] & EOC < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
      EOC >= splits[3] & EOC <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
      EOC > splits[4] ~ paste0("Greater than ", splits[4], "%")))
  
  dummy_age$percent_fctr <- as.factor(dummy_age$percent_fctr)
  
  # ------------
  # Getting geospatial data 
  # ------------
  
  dummy_age <- left_join(dummy_age, state_overlay_geo, by = "STNAME")
  
  # --------------------
  # MAPPING
  # --------------------
  a2010_breaks <- c(PurpOr7[1], PurpOr7[2], PurpOr7[3], PurpOr7[4], PurpOr7[5])
  
  names(a2010_breaks) <- c(paste0("Less than ",splits[1],"%"), paste0(splits[1], " to ", splits[2], "%"), paste0(splits[2], " to ", splits[3], "%"),
                           paste0(splits[3], " to ", splits[4], "%"), paste0("Greater than ", splits[4], "%"))
  
  # Asian Alone - A 
  A_2010_state <- state_map(dummy_age, a2010_breaks)
  A_2010_state <- A_2010_state +  
    labs(fill = "Error of Closure (%)     ",
         title =paste0("      Population Estimates and Census Comparison\n      Asian (Alone) Populations - Ages ", agegrp_labels[i]),
         subtitle = "         Resident Population By State - 2010",
         caption = "An error of closure value less than 0% indicates a potential\nundercount ie. the estimates for Asian (alone) populations\n were greater than the census results.") +
    titles_upper()
  
  # state_agegrp_imgs <- append(state_agegrp_imgs, A_2010_state)
  # save
  ggsave(filename = paste("../../AAJC Vis/diff_state_agegrp_2010/diff_by_agegrp_",i,"_AA_2010_state_map.png",sep=""),
         plot = A_2010_state, bg = "white", width = 9, height = 7)
}




# --------------------
# HISTOGRAM
# --------------------
analytical_county_2010 <- read.csv("../Transformed Data/PES_DC_MR_comparison_2010.csv")

dummy <- analytical_county_2010 %>% filter(COMPARISON == "PES_MR") %>% 
  filter(RACE_GROUP == "AA" | RACE_GROUP == "AAC") %>%
  select(STNAME, CTYNAME, RACE_GROUP, NUMERIC_DIFF, PERCENT_DIFF)

# convert char to numeric
dummy$PERCENT_DIFF <- as.numeric(dummy$PERCENT_DIFF)

dummy %>% filter(RACE_GROUP == "AA") %>%
  ggplot(aes(x = PERCENT_DIFF)) +
  geom_histogram(fill = "#f4c78d", binwidth = 10) +
  theme_minimal() +
  labs(fill = "Population was greater\nthan estimated?\n(Census results > estimated results)",
       title = "Percent Difference between 2010 Census Modified Race and\nPopulation Estimates, of Counties: Asian Alone",
       x = "% Difference",
       y = "Counties") +
  scale_x_continuous(breaks=seq(-200, max(dummy$PERCENT_DIFF), 50))+
  theme(axis.text.x = element_text(angle = 45))




########################
## STATE MAPS 2x2 SET ##
########################

# [insert state maps for AA Alone and AA Alone or in combo for 2010 and 2020 Census this would be a 2x2 set of maps]

state_2010 <- read.csv("../../Transformed Data/2010/state_level_comparisons_2010.csv")
state_2020 <- read.csv("../../Transformed Data/2020/state_level_comparisons_2020.csv")

year_dfs <- c(state_2010, state_2020)

title_labels <- c("Asian (Alone)", "Asian (Alone or in Combination)", "NHPI (Alone)", "NHPI (Alone or in Combination)")
caption_labels <- c("Asian (Alone)\n", "Asian (Alone or in Combination)\npopulations", "NHPI (Alone)\n", "NHPI (Alone or in Combination)\npopulations")
race_groups <- unique(state_2010$RACE)

# for each year 2010 and 2020
for (i in seq(race_groups)) {
  
  # filter data for race group
  # -------
  dummy <- state_2020
  dummy <- dummy %>% filter(RACE == race_groups[i]) %>% rename(EOC = PERC_DIFF)
  
  # creating percent_fctr column
  # -------
  min_i <- min(dummy$EOC)
  max_i <- max(dummy$EOC)
  
  splits <- as.integer(seq(min_i, max_i, length.out = 4))
  
  dummy <- dummy %>% 
    mutate(percent_fctr = case_when(
      EOC < splits[1] ~ paste0("Less than ",splits[1],"%"),
      EOC >= splits[1] & EOC < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
      EOC >= splits[2] & EOC < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
      EOC >= splits[3] & EOC <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
      EOC > splits[4] ~ paste0("Greater than ", splits[4], "%")))
  
  dummy$percent_fctr <- as.factor(dummy$percent_fctr)
  
  # add geospatial data 
  # -------
  dummy <- left_join(dummy, state_overlay_geo, by = "STNAME")
  
  # Mapping
  # -------
  a2010_breaks <- c(PurpOr7[1], PurpOr7[2], PurpOr7[3], PurpOr7[4], PurpOr7[5])
  
  names(a2010_breaks) <- c(paste0("Less than ",splits[1],"%"), paste0(splits[1], " to ", splits[2], "%"), paste0(splits[2], " to ", splits[3], "%"),
                           paste0(splits[3], " to ", splits[4], "%"), paste0("Greater than ", splits[4], "%"))
  
  
  state_vis <- state_map(dummy, a2010_breaks)
  state_vis <- state_vis +  
    labs(fill = "Error of Closure (%)     ",
         title =paste0("      Population Estimates and Census Comparison\n      ", title_labels[i], " Populations"),
         subtitle = "         Resident Population By State - 2020",
         caption = paste0("An error of closure value less than 0% indicates a potential\nundercount ie. the estimates for ",caption_labels[i]," were greater than the census results.")) +
    titles_upper()
  
  # state_agegrp_imgs <- append(state_agegrp_imgs, A_2010_state)
  # save
  ggsave(filename = paste("../../AAJC Vis/state_maps/state_map_",race_groups[i],"_2020.png",sep=""),
         plot = state_vis, bg = "white", width = 9, height = 7)
  
}





################
# OLD VIS CODE #
################
# A list of our race values (AA, AAC, NHPI, NHPIC)
variables <- unique(analytical$variable)

# # for each race group, call the county map function 
# for (i in variables){
#   dummy <- analytical %>%
#     # get each race group
#     filter(variable == i) %>%
#     # create a f% groups column for discrete color scale
#     mutate(percent_fctr = case_when(
#       PERCENT_DIFF < 0.0 ~ "Less than 0%",
#       PERCENT_DIFF >= 0 & PERCENT_DIFF <= 1 ~ "0 to 1%",
#       PERCENT_DIFF > 1 & PERCENT_DIFF <= 25 ~ "1 to 25%",
#       PERCENT_DIFF > 25 & PERCENT_DIFF <= 50 ~ "25 to 50%",
#       PERCENT_DIFF > 50 & PERCENT_DIFF <= 100 ~ "50 to 100%",
#       PERCENT_DIFF > 100 ~ "Greater than 100%",)) %>%
#     county_map() 
#   
#   # adding appropriate labels
#   if (i == "AA_TOT"){
#     dummy <- dummy +
#       labs(fill = "% difference between Population\nEstimates and Census Results     ",
#            title ="      Percent Difference in 2010 Asian American (alone) Population\n      Estimates Compared to 2010 Decennial Census ",
#            subtitle = "         Resident Population By County",
#            caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for Asian Americans (only) was less than the census results ie. the Asian\n American (alone) population was greater than estimated in 2010.") +
#       titles_upper()
#   }
#   
#   else if (i == "AAC_TOT") {
#     dummy <- dummy +
#       labs(fill = "% difference between Population\nEstimates and Census Results     ",
#            title ="      Percent Difference in 2010 Asian American\n      (alone or in Combination) Population Estimates\n      Compared to 2010 Decennial Census ",
#            subtitle = "         Resident Population By County",
#            caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for Asian Americans (alone or in combination) was less than the census results ie. the Asian\n American (alone or in combination) population was greater than estimated in 2010.") +
#       titles_upper()
#   }
#   else if (i == "NA_TOT") {
#     dummy <- dummy +
#       labs(fill = "% difference between Population\nEstimates and Census Results     ",
#            title ="      Percent Difference in 2010 Native Hawaiian and Pacific Islander\n      (alone) Population Estimates\n      Compared to 2010 Decennial Census ",
#            subtitle = "         Resident Population By County",
#            caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for  Native Hawaiian and Pacific Islander (alone) was less than\nthe census results ie. the Native Hawaiian and Pacific Islander (alone)\npopulation was greater than estimated in 2010.") +
#       titles_upper()
#   }
#   else {
#     dummy <- dummy +
#       labs(fill = "% difference between Population\nEstimates and Census Results     ",
#            title ="      Percent Difference in 2010 Native Hawaiian and Pacific Islander\n      (alone or in combination) Population Estimates\n      Compared to 2010 Decennial Census ",
#            subtitle = "         Resident Population By County",
#            caption = "A percentage difference value of 'Less than 0%' indicates that the the population\nestimate for  Native Hawaiian and Pacific Islander (alone or in combination) was less than\nthe census results ie. the Native Hawaiian and Pacific Islander (alone or in combination)\npopulation was greater than estimated in 2010.") +
#       titles_upper()
#   }
#   
#   
#   ggsave(filename = paste("../AAJC Vis/",i,"_estimates_census_comparison_2010_COUNTY_MAP.png",sep=""),
#          plot = dummy, bg = "white")
# }
# 
# 
