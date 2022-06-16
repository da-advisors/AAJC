library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(mapproj)
library(maps)
source("AAJC_theme.R") # for titles_upper() - applies capitalization to all titles of ggplot object


#  [Map] Decennial Census: Asian Alone population by state count / percent
#           of total pop (2000, 2010, 2020)


# Setting up THEME, COLORS, & API 
# -------------------------------

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')


# Loading all vars and searching for the needed ones
# d2000 <- load_variables(2000, 'sf1', cache = T)

options(tigris_use_cache = TRUE)

# ------------
# GETTING DATA
# ------------

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

# 2000 + 2010 vars: 
# Asian Alone pop., Asian alone or in combination pop., NHPI pop.
vars_2000 <- c(AA_alone = "P007005",
               AA_alone_combination = "P009005",
               NHPI_alone = "P007006")
totalPop_2000 <- "P007001"

vars_2010 <- c(AA_alone = "P003005",
               AA_alone_combination = "P006005",
               NHPI_alone = "P003006")
totalPop_2010 <- "P003001"

# 2020 vars: 
# Asian Alone pop., NHPI pop.   (could not find AA alone or in combination for 2020)
vars_2020 <- c(AA_alone = "P1_006N",
               NHPI_alone = "P1_007N")
totalPop_2020 <- "P1_001N"

# Possible solution - We can use the ACS 2020 estimates
#   From the ACS 2020 ESTIMATES:
#   ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE GROUPS
vars_2020_acs <- c(AA_alone_combination = "B02011_001")
estimate_totalPop_2020 <- "B01001_001"


# GETTING DATA 
d2000 <- get_census_data(vars_2000, totalPop_2000, 2000)
d2010 <- get_census_data(vars_2010, totalPop_2010, 2010)
d2020 <- get_census_data(vars_2020, totalPop_2020, 2020)

# Dropping Puerto Rico 
d2000 <- d2000[!grepl("Puerto Rico", d2000$NAME), ]
d2010 <- d2010[!grepl("Puerto Rico", d2010$NAME), ]
d2020 <- d2020[!grepl("Puerto Rico", d2020$NAME), ]


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
alaska.mod = alaska +c(600000, -100000)
hawaii.mod = hawaii + c(1700000, -199000)

# updating state_overlay data with our new geometry of alaska & hawaii
state_overlay$geometry[state_overlay$NAME == "Alaska"] <- alaska.mod
state_overlay$geometry[state_overlay$NAME == "Hawaii"] <- hawaii.mod

# # trial and error to see what looks good 
# plot(state_overlay$geometry)
# plot(alaska.mod, col="lightblue", add=T)
# plot(hawaii.mod, col="lightblue", add=T)

# Doing the same for the actual decennial census data 
d2000$geometry[grepl("Alaska", d2000$NAME) ] <- 
  d2000$geometry[grepl("Alaska", d2000$NAME) ] + c(600000, -100000)
d2000$geometry[grepl("Hawaii", d2000$NAME) ] <- 
  d2000$geometry[grepl("Hawaii", d2000$NAME) ] + c(1700000, -199000)


# ------------
# MAPPING DATA
# ------------

# 1. Asian Alone
# --------------
d2000_MAP_AA <- d2000[d2000$variable == 'AA_alone', ] %>%
  mutate(percent = 100 * (value/summary_value)) %>% 
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
  # scale_fill_viridis_c(end = .95, direction = -1, option = "magma") +
  scale_fill_manual(values = c("0%" = "white",
                               "Less than 1%" = "#FFF9F2",
                               "1 to 25%" = "#FAC687",
                               "25 to 50%" = "#BD6D62",
                               "Greater than 50%" = "#6D1162")) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     Asian American US Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

# change margins to fit alaska and hawaii changes
d2000_MAP_AA <- d2000_MAP_AA + theme(plot.margin = margin(1,1,1,1, "cm"))

ggsave("../AAJC Vis/AA_alone_2000_COUNTY_MAP_v2.png",
       plot = d2000_MAP_AA, bg = "white")

# 2. Asian Alone or in combination
# --------------------------------
d2000_MAP_AAC <- d2000[d2000$variable == 'AA_alone_combination', ] %>%
  mutate(percent = 100 * (value/summary_value)) %>% 
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
  # scale_fill_viridis_c(end = .95, direction = -1, option = "magma") +
  scale_fill_manual(values = c("0%" = "white",
                               "Less than 1%" = "#FFF9F2",
                               "1 to 25%" = "#FAC687",
                               "25 to 50%" = "#BD6D62",
                               "Greater than 50%" = "#6D1162")) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     Asian American (alone or in combination) US Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

# change margins to fit alaska and hawaii changes
d2000_MAP_AAC <- d2000_MAP_AAC + theme(plot.margin = margin(1,1,1,1, "cm"))

ggsave("../AAJC Vis/AA_alone_combination_2000_COUNTY_MAP_v2.png",
       plot = d2000_MAP_AAC, bg = "white")
  
# 3. NHPI 
# -------
d2000_MAP_NHPI <- d2000[d2000$variable == 'NHPI_alone', ] %>%
  mutate(percent = 100 * (value/summary_value)) %>%
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
  # scale_fill_viridis_c(end = .95, direction = -1, option = "magma") +
  scale_fill_manual(values = c("0%" = "white",
                               "Less than 1%" = "#FFF9F2",
                               "1 to 25%" = "#FAC687",
                               "25 to 50%" = "#BD6D62",
                               "Greater than 50%" = "#6D1162")) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     NHPI US Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

# change margins to fit alaska and hawaii changes
d2000_MAP_NHPI <- d2000_MAP_NHPI + theme(plot.margin = margin(1,1,1,1, "cm"))


ggsave("../AAJC Vis/NHPI_2000_COUNTY_MAP_v2.png",
       plot = d2000_MAP_NHPI, bg = "white")

