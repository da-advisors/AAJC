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





# ==========================
# add a scatterplot of response rate by % AA by tract in New York City
# ==========================

# read in self response data 
sr_2020 <- read.csv("../../Raw Data/2020/newyork_selfresponse_rates_2020_by_tract.csv")


# Questions for Chris
# (?) Which data are we using for % AA by tract --> MR or Estimates 
#    - Because we used Chris's MR file for 2020, Census populations were pulled by tracts using tidycensus 
#    - Chris MR file contains counties only 

# (?) Do we want to size the scatter plot points by total tract popualtion? 
#     - little harder to read but could provide insight 
#     - Some points are at 0 or a very small tract population so it would make sense for 
#       for those to have really low self response rates 


# -------
# Pulling Census populations by tract (tidycensus)
# --------

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

options(tigris_use_cache = TRUE)

# search for relevant variables 
vars_2020 <- load_variables(2020, "pl", cache = TRUE)

# store asian variables 
asian_vars <- vars_2020 %>% filter(grepl('Asian', label) & concept == "RACE")
asian_vars <- asian_vars$name

# api call - pulling census data 
ny_census_tract_asian <- get_decennial(geography = "tract",
                                       variables = asian_vars,
                                       state = "NY",
                                       county = ny_boroughs,
                                       summary_var = 'P1_001N', # total pop. of a tract 
                                       year = 2020)

# -------
# Preparing SR and Census Data 
# --------

# fix GEOID column in self response DF to match and join later with census DF
sr_2020 <- sr_2020 %>% separate(GEO_ID, c('A','GEO_ID_tract'), sep = 'US') %>% select(-A)

# filtering self response data 
sr_2020_NY <- sr_2020 %>% filter(COUNTY %in% c(" New York County", " Kings County", " Bronx County", " Richmond County", " Queens County")
                                 & STATE == " New York")


# -------
# Aggregating all 'in combination' race variables for census population 
# --------

# create new DFs for aic values and alone values 
ny_census_tract_aic <- ny_census_tract_asian %>%group_by(GEOID, summary_value) %>%
  summarise(value = sum(value)) %>% mutate(RACE = "A_AIC") %>% rename('total_tract_pop' = summary_value)

ny_census_tract_a <- ny_census_tract_asian %>% mutate(RACE = case_when(variable == "P1_006N" ~ 'A_A'), 
                                                      value = case_when(
                                                        variable == "P1_006N" ~ value)) %>%
  filter(!is.na(value)) %>% select(-NAME, -variable) %>% rename('total_tract_pop' = summary_value)

# combine the two DFs 
ny_census <- rbind(ny_census_tract_aic, ny_census_tract_a) %>% arrange(GEOID, RACE)


# -------
# Adding percentage calculations
# --------

# Compute % of Asian populations for scatter plot 
ny_census <- ny_census %>% mutate(pop_percentage = round((value/total_tract_pop)*100, 2))

# joining self response and census population data
sr_2020_NY <- sr_2020_NY %>% left_join(ny_census %>% select(GEO_ID_tract = GEOID, total_tract_pop, RACE, value, pop_percentage), by = 'GEO_ID_tract')


# -------
# Plot
# --------

# Asian Alone 
scatter_response <- sr_2020_NY %>% filter(RACE == 'A_A') %>%
  ggplot(aes(x = pop_percentage, y = CRRALL, size = total_tract_pop)) + 
  geom_point(color = "#e49d48", alpha = 0.7) + 
  theme_minimal() + 
  xlab("Asian (Alone) Population (%)") + 
  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
  ggtitle("Response Rate by Percentage of Asian Population by Census Tract - 2020")

ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/resp_by_tract_pop_scatter_AA_2020_SIZE.png",
       plot = scatter_response, bg = "white", width =9.07, height = 5.47)

# light or - e49d48
# dark or - ac550f
#      

# 2 options 
#   1) points are not sized according to census population & no alpha 
#   2) points sized by census population and alpha to make them lighter for readablity 










