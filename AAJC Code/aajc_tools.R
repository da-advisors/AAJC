library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(formattable)
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
      list_perc < 0 & list_perc >= -50 ~ "-50 to 0% (under)",
      list_perc < -50 & list_perc >= -100 ~ "-100 to -50% (under)",
      list_perc < -100 & list_perc >= -200 ~ "-200 to -100% (under)",
      list_perc == 0 ~ "0% (equal)",
      list_perc > 0 & list_perc < 1 ~ "Less than 1% (over)",
      list_perc > 1 & list_perc <= 25 ~ "Between 1 & 25% (over)",
      list_perc > 25 & list_perc <= 50 ~ "Between 25 & 50% (over)",
      list_perc > 50 & list_perc <= 75 ~ "Between 50 & 75% (over)",
      list_perc > 75 & list_perc <= 100 ~ "Between 75 & 100% (over)",
      list_perc > 100 & list_perc <= 200 ~ "Between 100 & 200% (over)")) %>%
    group_by(list_perc_factor) %>% 
    summarise(number_of_counties = n()) %>%
    mutate(percent_of_all_counties = (number_of_counties/total_counties)*100)
    
  colnames(list_perc) <- c("Percentage Difference", "Number of US Counties", "Percentage of all US counties")
  
  x <- c("-200 to -100% (under)", "-100 to -50% (under)", "-50 to 0% (under)", "0% (equal)",  "Less than 1% (over)", "Between 1 & 25% (over)","Between 25 & 50% (over)", "Between 50 & 75% (over)", "Between 75 & 100% (over)",
         "Between 100 & 200% (over)")
  
  list_perc <- list_perc %>% slice(match(x, `Percentage Difference`))
  print(formattable(data.frame(list_perc),
                    align = c("l", rep("r", NCOL(list_perc) - 1)),
                    Percentage.Difference = formatter("span", style = ~ style(color = "grey", font.weight = "bold"))))
  # print(list_perc)
  
 
}







