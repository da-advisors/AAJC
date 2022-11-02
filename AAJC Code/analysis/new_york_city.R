library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(grid)
library(gridExtra)
library(dvmisc)
source("../aajc_tools.R")


# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('../theme_AAJC.rds')


# ==========================
# LINE CHART: Add line chart with overcounts and undercounts by age groups for New York City as well as
#     for each county in the city and for the United States as a whole for AA Alone and AA Alone or in Combination for 2010
# ==========================

# read data 
agegrp_2010 <- read.csv("../../Transformed Data/2010/ES_MR_AGEGRP_comparison_2010.csv")

agegrp_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                   "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")


# -------
# V2:
# --------

# 1. 
# aggregate data for entire US
agegrp_2010_UNITED_STATES <- agegrp_2010 %>% group_by(AGEGRP, RACE) %>% summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM,   # numeric diff
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),   # percent difference/error of closure (EOC)
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))


# 2. 
# Prepare NYC & US data 
# using agegrp_2010_UNITED_STATES filter for race group -> A_A or A_AIC
agegrp_2010_UNITED_STATES$CTYNAME <- "United States"
agegrp_2010_UNITED_STATES <- agegrp_2010_UNITED_STATES %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)


#   NY County - manhattan 
#   Kings County - Brooklyn
#   Bronx County - Bronx 
#   Richmond County - Staten Island 
#   Queens County - Queens

# all 5 boroughs (counties) in NY 
ny_boroughs <- c("New York County", "Kings County", "Bronx County", "Richmond County", "Queens County")
agegrp_2010_NY_boroughs <- agegrp_2010 %>% filter(CTYNAME %in% ny_boroughs & STNAME == "New York")  %>%
  select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)

# aggregate boroughs into 1 for nyc
agegrp_2010_NY <- agegrp_2010_NY_boroughs %>% group_by(AGEGRP, RACE) %>% summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM,   # numeric diff
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),   # percent difference/error of closure (EOC)
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))
agegrp_2010_NY$CTYNAME <- "New York City"
agegrp_2010_NY <- agegrp_2010_NY %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)

# join US, NYC, and county data  
agegrp_2010_NY_USA <- rbind(agegrp_2010_NY,agegrp_2010_NY_boroughs, agegrp_2010_UNITED_STATES)


# 3. 
# Plot
v2_line <- agegrp_2010_NY_USA %>% filter(RACE == "A_AIC") %>%
  ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = CTYNAME, linetype = CTYNAME)) +
  geom_hline(yintercept = 0, linetype='dotted', col='grey')+
  geom_line(aes(color=CTYNAME), size=.7, alpha=.8) +
  scale_linetype_manual(values = c("dashed",  "dashed",  "solid",
                                   "dashed", "dashed",  "dashed",
                                   "solid"),
                        name = "Region") +
  scale_color_manual(values = c("#916a92", "#f4c78d", "#CC5500", "#62929E", "#780116","#008148", "#0F1108"), name = "Region") +
  theme_minimal() +
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("Coverage by Age Group for Asian (Alone or in Combination) Populations - 2010")+
  scale_x_discrete(labels = agegrp_labels) +
  annotate("text",x=18.0, y=1.3, label="overcount", size=2.5, color='grey') +
  annotate("text",x=18.0, y=-1.3, label="undercount", size=2.5, color='grey')

# change age group labels 
v2_line <- v2_line + theme(axis.text.x = element_text(angle=45)) 

ggsave(filename = "../../AAJC Vis/case_studies/new_york_city//US_AND_NY_line_graph_coverage_by_agegrp_A_AIC_2010.png",
       plot = v2_line, bg = "white", width =10.0, height = 5.47)


# To-Dos: 11/2 # 
#   1. Cannot reorder legend so that NYC and US are both on the bottom. Alysha might have to do this part 
#   2. Colors 




