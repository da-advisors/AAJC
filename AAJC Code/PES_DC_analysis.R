library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(grid)
library(gridExtra)
library(dvmisc)
source("aajc_tools.R")



# ==================================
# Analysis and data exploration of 
# 2010 population estimates compared 
# to 2010 Decennial Census Results
# ==================================

# ========
# get data 
# ========

# analytical <- read.csv("../Transformed Data/estimates_census_comparison_2010.csv")
# analytical <- read.csv("../Transformed Data/PES_DC_MR_comparison_2000.csv")
analytical <- read.csv("../Transformed Data/PES_DC_MR_comparison_2020.csv")

# ==================================
# Cleaning step - for 2020 only
# ==================================

# NHPI is recorded as "NA" in the RACE_GROUP column 
# R is reading them in as NA values 

analytical$RACE_GROUP[is.na(analytical$RACE_GROUP)] <- "NA"


# ==================================
# Getting summaries of percent diff.
# ==================================


# inspecting the PERCENT_DIFF column 
summary(analytical$NUMERIC_DIFF)

# A list of our race values (AA, AAC, NHPI, NHPIC)
variables <- unique(analytical$RACE_GROUP)
# MR_comparisons <- c("PES_alone_MR_alone", "MR_alone_DC_alone", "PES_alone_MR_combo", "MR_combo_DC_combo")

# not in function
'%!in%' <- function(x,y)!('%in%'(x,y))


# get % summaries for each race group
for (i in variables){
  dummy <- analytical %>%
    filter(RACE_GROUP == i)
           # get rid of MR 
           # ,COMPARISON %!in% MR_comparisons)
  
  cat(" --------\n", i, "\n", "--------\n")
  # get percent breakdown summaries
  # percent_breakdown(dummy$PERCENT_DIFF)

  # Get overestimate, underestimate, and equal estimate summaries
  # num rows where % diff is negative (underestimated pop)
  # under <- nrow(dummy[dummy$PERCENT_DIFF < 0, ])
  # # num rows where % diff is positive (overestiamted pop)
  # over <- nrow(dummy[dummy$PERCENT_DIFF > 0, ])
  # # num rows where % diff is 0 -- no percentage diff between PES & DC
  # equal <- nrow(dummy[dummy$PERCENT_DIFF == 0, ])
  # # total rows
  # tot <- nrow(dummy)
  # # DF for table
  # OOE <- data.frame(Population = c("underestimated", "overestiamted", "equal"),
  #                   Number_of_US_counties = c(under,over,equal),
  #                   Percentage_of_US_counties = c(under/tot *100, over/tot*100, equal/tot*100))
  # 
  # print(formattable(OOE,
  #             align = c("l", rep("r", NCOL(OOE) - 1))))

  # get quantiles
  # print(summary(dummy$PERCENT_DIFF))
  # print(table(quant_groups(dummy$PERCENT_DIFF, groups = 5)))
  # print(table(quant_groups(dummy$PERCENT_DIFF, groups = 4)))

  # Top 20 counties by numeric difference in population between Estimates & Census
  # dummy1 <- dummy %>%
  #   top_n(20, abs(NUMERIC_DIFF)) %>%
  #   select(STNAME, CTYNAME, NUMERIC_DIFF, PERCENT_DIFF) %>%
  #   arrange(desc(abs(NUMERIC_DIFF)))
  # 
  # print(dummy1)

  # Top 20 counties by % difference in population between Estimates & Census
  # NOT USING THIS BC HUNDREDS OF COUNTIES HAVE THE MAX % DIFF VALUE OF 200%
  # dummyp <- dummy %>%
  #   top_n(20, abs(PERCENT_DIFF)) %>%
  #   select(STNAME, CTYNAME, PERCENT_DIFF, NUMERIC_DIFF) %>%
  #   arrange(desc(abs(PERCENT_DIFF)))
  # 
  # print(dummyp)
  
  
  # dummyp <- dummy %>%
  #   top_n(20, abs(PERCENT_OF_COUNTY_DIFF)) %>%
  #   select(STNAME, CTYNAME, PERCENT_OF_COUNTY_DIFF) %>%
  #   arrange(desc(abs(PERCENT_OF_COUNTY_DIFF)))
  # 
  # print(dummyp)
  # 
  # Top 10 states
  # aggregate numeric diff by state
  dummy2 <- dummy %>%
    group_by(STNAME) %>%
    summarise(NUMERIC_DIFF = sum(abs(NUMERIC_DIFF))) %>%
    top_n(10, NUMERIC_DIFF) %>%
    arrange(desc(NUMERIC_DIFF))

  print(dummy2)

  # Top 10 states
  # aggregate % diff by state
  dummy3 <- dummy %>%
    group_by(STNAME) %>%
    summarise(ESTIMATE = sum(ESTIMATE), CENSUS = sum(CENSUS),
              PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2)) %>%
    top_n(10, abs(PERCENT_DIFF)) %>%
    arrange(desc(abs(PERCENT_DIFF)))

  print(dummy3)


  # remove the maximum (6000) for display - its a big outlier and makes it hard to see the distribution 
  # dummy <- dummy[dummy$PERCENT_DIFF != max(dummy$PERCENT_DIFF), ]
  # hist(dummy$PERCENT_DIFF, breaks=100)
  
#   histo <- dummy %>%
#     ggplot(aes(x = PERCENT_DIFF)) +
#     geom_histogram(aes(fill = PERCENT_DIFF>0), binwidth = 5) +
#     theme_minimal() +
#     labs(fill = "Population was greater\nthan estimated?\n(Census results > estimated results)",
#          title = i,
#          x = "% Difference") +
#     scale_x_continuous(breaks=seq(-200, max(dummy$PERCENT_DIFF), 50))+
#     theme(axis.text.x = element_text(angle = 45))
# 
#   print(histo)
}


# dummy3 <- analytical %>%
#   mutate(
#   # dummy column to store ESTIMATE values for subtraction for numeric_diff
#   lag_estim = ESTIMATE) %>%
#   # filling NA values - ESTIMATE has NAs because there is no API_combo data available 
#   fill(lag_estim) %>%
#   filter(# get rid of MR 
#          COMPARISON == "PES_alone_DC_combo") %>%
#   group_by(STNAME) %>%
#   summarise(ESTIMATE = sum(lag_estim), CENSUS = sum(CENSUS),
#             PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2)) %>%
#   top_n(10, abs(PERCENT_DIFF)) %>%
#   arrange(desc(abs(PERCENT_DIFF)))
# 
# print(dummy3)
# 
# analytical[is.na(analytical$PERCENT_DIFF), ]


