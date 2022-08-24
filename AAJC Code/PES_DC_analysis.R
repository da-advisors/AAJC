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


# =====================================
# CALCULATIONS
# =====================================

# At the national-level, the 2000 Census came in [x] below the population estimates for the API race group
# =====================================

analytical <- read.csv("../Transformed Data/PES_DC_MR_comparison_2000.csv")

analytical[, c(4,5,6)] <- sapply(analytical[, c(4,5,6)], as.numeric)
# get total populations on a national level - ESTIMATES - API alone
estim_val <- analytical %>%
  filter(COMPARISON == 'PES_alone_DC_alone') %>%
  # group_by(CTYNAME) %>%
  summarise(ESTIMATE = sum(ESTIMATE), CENSUS = sum(as.integer(CENSUS)), MR = sum(as.integer(MR)))


# get total populations on a national level - CENSUS & MR - API alone or combo
cen_mr_val <- analytical %>%
  filter(COMPARISON == 'MR_combo_DC_combo') %>%
  summarise(CENSUS_combo = sum(CENSUS), MR_combo = sum(MR))

national_totals <- cbind(estim_val, cen_mr_val)

# percent difference Alone and in combo
(national_totals$ESTIMATE - national_totals$MR)/ ((national_totals$ESTIMATE + national_totals$MR)/2)
(national_totals$ESTIMATE - national_totals$MR_combo)/ ((national_totals$ESTIMATE + national_totals$MR_combo)/2)



# State level comparisons - [Top 10 and bottom 10 table]
# =====================================

# get relevant data and aggregate by state 
analytical_states <- analytical %>% 
  filter(COMPARISON == 'PES_alone_DC_alone' | COMPARISON == 'PES_alone_DC_combo') %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, MR, COMPARISON)

mr_combo = rep(analytical_states$MR[analytical_states$COMPARISON == 'PES_alone_DC_combo'], each = 2)

analytical_states$MR_COMBO = mr_combo

analytical_states[, c(4,5,7)] <- sapply(analytical_states[, c(4,5,7)], as.numeric)

# aggregate by state 
analytical_states <- analytical_states[!is.na(analytical_states$ESTIMATE), ] %>%
  group_by(STNAME) %>% 
  summarise(ESTIMATE = sum(ESTIMATE), MR = sum(MR), MR_COMBO = sum(MR_COMBO))

# Calculate differences 
t10_alone_pdiff <- analytical_states %>%
  mutate(P_DIFF_alone = (ESTIMATE - MR)/ ((ESTIMATE + MR)/2),
         N_DIFF_alone = ESTIMATE - MR,
         P_DIFF_combo = (ESTIMATE - MR_COMBO)/ ((ESTIMATE + MR_COMBO)/2),
         N_DIFF_combo = ESTIMATE - MR_COMBO) %>%
  top_n(-10, abs(P_DIFF_alone)) %>%
  arrange(abs(P_DIFF_alone))

# Bottom 10 states by % DIFF (PES alone vs MR alone )
t10_states_alone <- data.frame(State = t10_alone_pdiff$STNAME,
                               percent_difference = t10_alone_pdiff$P_DIFF_alone*100,
                               numeric_difference = t10_alone_pdiff$N_DIFF_alone)

print(formattable(t10_states_alone, align = c("l", rep("r", NCOL(t10_states_alone) - 1)),
                  caption = "Bottom 10 States by Percent Difference - API Alone (estimates) and API Alone (MR)"))


# State level comparisons - [Map of differences at state level]
# =====================================

# 1. Clean Data 
# --------------
state_differences_percent <- analytical_states %>%
  mutate(P_DIFF_alone = (ESTIMATE - MR)/ ((ESTIMATE + MR)/2)*100,
         P_DIFF_combo = (ESTIMATE - MR_COMBO)/ ((ESTIMATE + MR_COMBO)/2)*100) %>%
  pivot_longer(cols = c(P_DIFF_alone, P_DIFF_combo), names_to = 'COMPARISON', values_to = 'PERCENT_DIFF') %>%
  mutate(COMPARISON = case_when(
    COMPARISON == "P_DIFF_alone" ~ "PES_alone_MR_alone",
    COMPARISON == "P_DIFF_combo" ~ "PES_alone_MR_combo",
  ))

state_differences_numeric <- analytical_states %>%
  mutate(N_DIFF_alone = ESTIMATE - MR,
         N_DIFF_combo = ESTIMATE - MR_COMBO) %>%
  pivot_longer(cols = c(N_DIFF_alone, N_DIFF_combo), names_to = 'COMPARISON', values_to = 'NUMERIC_DIFF') %>%
  mutate(COMPARISON = case_when(
    COMPARISON == "N_DIFF_alone" ~ "PES_alone_MR_alone",
    COMPARISON == "N_DIFF_combo" ~ "PES_alone_MR_combo",
  ))

# join
state_differences <- left_join(state_differences_percent,state_differences_numeric, by = c("STNAME","ESTIMATE", "MR", "MR_COMBO", "COMPARISON") )

write.csv(state_differences, "../Transformed Data/state_level_comparisons_2000.csv")

# 1. Mapping 
# --------------

# Mapping in PES_DC_vis.R

