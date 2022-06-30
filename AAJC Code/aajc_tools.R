library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
# source("AAJC_theme.R")


# =============================
# PERCENTAGE BREAKDOWNS SUMMARY 
# =============================

# Pass in a list of percentages and get a breakdown summary 
# For example:
#                             |  % of US counties 
# -----------------------------------------------
# AA Pop = 0%                 |       1%
# AA Pop between 1 and 25%    |       20%

percent_breakdown <- function(list_perc) {
  # covnert to % and create a DF 
  list_perc <- data.frame(list_perc)
  
  total_counties = nrow(list_perc)
  
  list_perc <- list_perc %>%
    mutate(list_perc_factor = case_when(
      list_perc < 0 ~ "Less than 0%",
      list_perc == 0 ~ "0%",
      list_perc > 0 & list_perc < 1 ~ "Less than 1%",
      list_perc > 1 & list_perc <= 25 ~ "Between 1 & 25%",
      list_perc > 25 & list_perc <= 50 ~ "Between 25 & 50%",
      list_perc > 50 & list_perc <= 75 ~ "Between 50 & 75%",
      list_perc > 75 & list_perc <= 100 ~ "Between 75 & 100%",
      list_perc > 100 & list_perc <= 1000 ~ "Between 100 & 1000%",
      list_perc > 1000 & list_perc <= 2000 ~ "Between 1000 & 2000%",
      list_perc > 2000 & list_perc <= 3000 ~ "Between 2000 & 3000%",
      list_perc > 3000 & list_perc <= 4000 ~ "Between 3000 & 4000%",
      list_perc > 4000 ~ "Greater than 4000%")) %>%
    group_by(list_perc_factor) %>% 
    summarise(number_of_counties = n()) %>%
    mutate(percent_of_all_counties = (number_of_counties/total_counties)*100)
    
  colnames(list_perc) <- c("Population", "Number of US Counties", "Percentage of all US counties")
  
  x <- c("Less than 0%", "0%",  "Less than 1%", "Between 1 & 25%","Between 25 & 50%", "Between 50 & 75%", "Between 75 & 100%",
         "Between 100 & 1000%","Between 1000 & 2000%","Between 2000 & 3000%","Between 3000 & 4000%",
         "Greater than 4000%")
  
  list_perc <- list_perc %>% slice(match(x, Population))
  print(list_perc)
 
}







