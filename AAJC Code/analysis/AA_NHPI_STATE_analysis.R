library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(mapproj)
library(maps)
source("AAJC_theme.R") # for titles_upper() - applies capitalization to all titles of ggplot object


# NEEDED VIS: 
# 1. [Map] Decennial Census: Asian Alone population by state count / percent
#           of total pop (2000, 2010, 2020)



# Setting up THEME, COLORS, & API 
# -------------------------------

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')

# Defining color variables (see AAJC_them.R for info on these colors)
red <- '#EF6D59'
orange <- '#E37232'
orange_80t <- '#f9e3d6'
orange_10t <- "#e68047"
purple <- '#3F3875'


#################
## ASIAN ALONE ##
#################

# Pulling data  
# ------------

# 2000
# ----

# Loading all vars and searching for the needed ones
d2000 <- load_variables(2000, 'sf1', cache = T)

# Whats the diff between 'Total!!Asian alone' and 'Total!!Population of one race!!Asian alone'
#     They are the same values - only need one
#     P007005 vs P003006
#     Total population for RACE [8] - P007005 --> P007001

options(tigris_use_cache = TRUE)

# Needed variables 
totPop <- "P007001"
asian_alone <- "P007005" 
# total races tallied!!Asian alone or in combination with one or more other races
asian_alone_or_combination <- "P009005" 

# Total!!Native Hawaiian and Other Pacific Islander alone
nhpi_alone <- "P007006"

AA_alone_2000 <- 
  get_decennial(
  geography = "state",
  variables = nhpi_alone,
  geometry = TRUE,
  resolution = "20m",
  summary_var = totPop,  # total population in this instance 
  year = 2000) %>%
  shift_geometry()

head(AA_alone_2000)

# MAP 
# ---
AA_alone_2000_MAP <- AA_alone_2000 %>% 
  mutate(percent = 100 * (value/summary_value)) %>%
  ggplot(aes(fill = percent)) + 
  geom_sf(color = "white") +
  theme_AAJC + 
  scale_fill_gradient(low = orange_80t, high = orange) + 
  labs(fill = "% of NHPI     \npopulation     ",
       title ="     Native Hawaiian and Other Pacific Islander Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()



# ----
# 2010
# ----

# Loading all vars and searching for the needed ones
d2010 <- load_variables(2010, 'sf1', cache = T)

# Asian alone 
# "P003005"

# total races tallied!!Asian alone or in combination with one or more other races
asian_alone_or_combination <- "P006005"

# Total!!Native Hawaiian and Other Pacific Islander alone
nhpi_alone <- "P003006"

AA_alone_2010 <- 
  get_decennial(
    geography = "state",
    variables = nhpi_alone,
    geometry = TRUE,
    resolution = "20m",
    summary_var = "P003001",  # total population in this instance 
    year = 2010) %>%
  shift_geometry()

head(AA_alone_2010)

# MAP
# ---
AA_alone_2010_MAP <- AA_alone_2010 %>% 
  mutate(percent = 100 * (value/summary_value)) %>%
  ggplot(aes(fill = percent)) + 
  geom_sf(color = "white") + 
  scale_fill_gradient(low = orange, high = purple) +
  labs(fill = "% of NHPI     \npopulation     ",
       title ="     Native Hawaiian and Other Pacific Islander Population",
       subtitle = "         2010 Decennial Census") +
  titles_upper() +
  theme_AAJC



# ----
# 2020
# ----

# Loading all vars and searching for the needed ones
d2020 <- load_variables(2020, 'pl', cache = T)

# !!Total:!!Population of one race:!!Native Hawaiian and Other Pacific Islander alone
nhpi_alone <- "P1_007N"

AA_alone_2020 <- 
  get_decennial(
    geography = "state",
    variables = nhpi_alone,
    geometry = TRUE,
    resolution = "20m",
    summary_var = "P1_001N",  # total population in this instance 
    year = 2020) %>%
  shift_geometry()

head(AA_alone_2020)

# MAP
# ---
AA_alone_2020_MAP <- AA_alone_2020 %>% 
  mutate(percent = 100 * (value/summary_value)) %>%
  ggplot(aes(fill = percent)) + 
  geom_sf(color = "white") + 
  scale_fill_gradient(low = orange_80t, high = orange) + 
  labs(fill = "% of NHPI     \npopulation     ",
       title ="     Native Hawaiian and Other Pacific Islander Population",
       subtitle = "         2020 Decennial Census") +
  titles_upper() +  # function to uppercase titles 
  theme_AAJC


# Preview maps 
AA_alone_2000_MAP
AA_alone_2010_MAP
AA_alone_2020_MAP


# Save into Vis Folder
ggsave("../AAJC Vis/NHPI_alone_2000_STATE_MAP_one_color.png",
       plot = AA_alone_2000_MAP, bg = "white")
ggsave("../AAJC Vis/NHPI_alone_2010_STATE_MAP_one_color.png",
       plot= AA_alone_2010_MAP, bg="white")
ggsave("../AAJC Vis/NHPI_alone_2020_STATE_MAP_one_color.png",
       plot= AA_alone_2020_MAP, bg="white")
