library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)


# 2020 NHPI community population (as a %) of the total US population #
# - raw census data


# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

options(tigris_use_cache = TRUE)

vars <- load_variables(2020, "pl", cache = T)

# store nhpi variables 
nhpi_vars <- vars %>% filter(grepl('Native Hawaiian', label) & concept == "RACE")
nhpi_vars <- nhpi_vars$name

nhpi_2020 <- get_decennial(geography = "us",
              variables = nhpi_vars,
              summary_var = 'P1_001N', # total pop. of a tract 
              year = 2020)


nhpi_2020 <- nhpi_2020 %>% group_by(NAME, summary_value) %>% summarise(nhpi_pop = sum(value))

nhpi_2020$nhpi_pop/nhpi_2020$summary_value
