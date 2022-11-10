library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(grid)
library(gridExtra)
library(dvmisc)
source("aajc_tools.R")


# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('../theme_AAJC.rds')


# ==========================
# LINE CHART: overcounts and undercounts by age groups for LA County and for the United States
# as a whole for AA Alone and AA Alone or in Combination for 2010
# ==========================

# read data 
agegrp_2010 <- read.csv("../../Transformed Data/2010/ES_MR_AGEGRP_comparison_2010.csv")

agegrp_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                   "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

# -------
# LA County 
# --------

# la_line <- agegrp_2010 %>% filter(CTYNAME == "Los Angeles County", RACE %in% c("A_A", "A_AIC")) %>%
#   ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = RACE)) +
#   geom_hline(yintercept = 0, linetype='dotted', col='grey')+
#   geom_line(aes(color=RACE), size=1) +
#   scale_color_manual(values = c("#916a92", "#f4c78d"), labels=c("Asian (alone)", "Asian (alone\nor in combination)"), name = "Race") + 
#   theme_minimal() +
#   xlab("Age Group") + 
#   ylab("Error of Closure (%)") + 
#   ggtitle("Los Angeles County Coverage by Age Group for Asian Populations - 2010")+
#   scale_x_discrete(labels = agegrp_labels) +
#   annotate("text",x=17.7, y=1, label="overcount", size=2.5, color='grey') +
#   annotate("text",x=17.7, y=-1, label="undercount", size=2.5, color='grey')
# 
# # change age group labels 
# la_line <- la_line + theme(axis.text.x = element_text(angle=45))
# 
# ggsave(filename = "../../AAJC Vis/case_studies/los_angeles/line_graph_coverage_by_agegrp_Asian_2010.png",
#        plot = la_line, bg = "white")


# -------
# US as a whole 
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
# Plot
us_line <- agegrp_2010_UNITED_STATES %>% filter(RACE %in% c("A_A", "A_AIC")) %>%
  ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = RACE)) +
  geom_hline(yintercept = 0, linetype='dotted', col='grey')+
  geom_line(aes(color=RACE), size=1) +
  scale_color_manual(values = c("#916a92", "#f4c78d"), labels=c("Asian (alone)", "Asian (alone\nor in combination)"), name = "Race") +
  theme_minimal() +
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("United States Coverage by Age Group for Asian Populations - 2010")+
  scale_x_discrete(labels = agegrp_labels) +
  annotate("text",x=17.7, y=1, label="overcount", size=2.5, color='grey') +
  annotate("text",x=17.7, y=-1, label="undercount", size=2.5, color='grey')

# change age group labels 
us_line <- us_line + theme(axis.text.x = element_text(angle=45))

ggsave(filename = "../../AAJC Vis/case_studies/los_angeles/US_line_graph_coverage_by_agegrp_Asian_2010.png",
       plot = us_line, bg = "white")


# -------
# Version 2: 
#   1 chart for AA comparing LA and US and 1 for AA_AIC
# --------

# 1. 
# Prepare data 
# using agegrp_2010_UNITED_STATES filter for race group -> A_A or A_AIC
agegrp_2010_UNITED_STATES$CTYNAME <- "United States"
agegrp_2010_UNITED_STATES <- agegrp_2010_UNITED_STATES %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)

# filter LA data 
agegrp_2010_LA <- agegrp_2010 %>% filter(CTYNAME == "Los Angeles County") %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)
                       
# join US and LA data 
agegrp_2010_LA_USA <- rbind(agegrp_2010_LA, agegrp_2010_UNITED_STATES)

# 2. 
# Plot
v2_line <- agegrp_2010_LA_USA %>% filter(RACE == "NHPI_AIC") %>%
  ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = CTYNAME)) +
  geom_hline(yintercept = 0, linetype='dotted', col='grey')+
  geom_line(aes(color=CTYNAME), size=1) +
  scale_color_manual(values = c("#916a92", "#f4c78d"), name = "Region") +
  theme_minimal() +
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("Coverage by Age Group for NHPI (Alone or in Combination) Populations - 2010")+
  scale_x_discrete(labels = agegrp_labels) +
  annotate("text",x=17.7, y=1.3, label="overcount", size=2.5, color='grey') +
  annotate("text",x=17.7, y=-1.3, label="undercount", size=2.5, color='grey')

# change age group labels 
v2_line <- v2_line + theme(axis.text.x = element_text(angle=45))

ggsave(filename = "../../AAJC Vis/case_studies/los_angeles/US_AND_LA_line_graph_coverage_by_agegrp_NHPI_AIC_2010.png",
       plot = v2_line, bg = "white", width =9.07, height = 5.47)



# ==========================
# add a scatterplot of response rate by % AA by tract in LA County
# ==========================

# read in self response data 
sr_2020 <- read.csv("../../Raw Data/2020/california_selfresponse_rates_2020_by_tract.csv")


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

# store nhpi variables 
nhpi_vars <- vars_2020 %>% filter(grepl('Native Hawaiian', label) & concept == "RACE")
nhpi_vars <- nhpi_vars$name

# api call - pulling census data 
la_census_tract_asian <- get_decennial(geography = "tract",
                                 variables = asian_vars,
                                 state = "CA",
                                 county = "Los Angeles County",
                                 summary_var = 'P1_001N', # total pop. of a tract 
                                 year = 2020)

la_census_tract_nhpi <- get_decennial(geography = "tract",
                                       variables = nhpi_vars,
                                       state = "CA",
                                       county = "Los Angeles County",
                                       summary_var = 'P1_001N', # total pop. of a tract 
                                       year = 2020)

# -------
# Preparing SR and Census Data 
# --------

# fix GEOID column in self response DF to match and join later with census DF
sr_2020$GEO_ID_tract <- sub('^', '06037',sr_2020$tract)

# filtering self response data 
sr_2020_LA <- sr_2020 %>% filter(COUNTY == " Los Angeles County" & STATE == " California")


# -------
# Aggregating all 'in combination' race variables for census population 
# --------

# create new DFs for aic values and alone values 
la_census_tract_aic <- la_census_tract_nhpi %>%group_by(GEOID, summary_value) %>%
  summarise(value = sum(value)) %>% mutate(RACE = "NHPI_AIC") %>% rename('total_tract_pop' = summary_value)

la_census_tract_a <- la_census_tract_nhpi %>% mutate(RACE = case_when(variable == "P1_007N" ~ 'NHPI_A'), 
                                                      value = case_when(
                                                        variable == "P1_007N" ~ value)) %>%
  filter(!is.na(value)) %>% select(-NAME, -variable) %>% rename('total_tract_pop' = summary_value)

# combine the two DFs 
la_census <- rbind(la_census_tract_aic, la_census_tract_a) %>% arrange(GEOID, RACE)


# -------
# Adding percentage calculations
# --------

# Compute % of Asian populations for scatter plot 
la_census <- la_census %>% mutate(pop_percentage = round((value/total_tract_pop)*100, 2))

# joining self response and census population data
sr_2020_LA <- sr_2020_LA %>% left_join(la_census %>% select(GEO_ID_tract = GEOID, total_tract_pop, RACE, value, pop_percentage), by = 'GEO_ID_tract')


# -------
# Plot
# --------

scatter_response <- sr_2020_LA %>% filter(RACE == 'NHPI_A') %>%
  ggplot(aes(x = pop_percentage, y = CRRALL, size = total_tract_pop)) + 
  geom_point(color = "#e49d48", alpha = 0.7) + 
  theme_minimal() + 
  xlab("NHPI (Alone or in Combination) Population (%)") + 
  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
  ggtitle("Response Rate by Percentage of NHPI Population by Census Tract - 2020")

ggsave(filename = "../../AAJC Vis/case_studies/los_angeles/resp_by_tract_pop_scatter_NHPI_AIC_2020_SIZE.png",
       plot = scatter_response, bg = "white", width =9.07, height = 5.47)

# light or - e49d48
# dark or - ac550f

# 2 options 
#   1) points are not sized according to census population & no alpha 
#   2) points sized by census population and alpha to make them lighter for readablity 




# ==========================
# map of response rate for 2020 by census tract for LA
# ==========================

# 1. 
# Get geospatial data for county by tract 

geo <- get_decennial(
  geography = "tract",
  state = "CA",
  county = "Los Angeles County",
  variables = 'P1_001N', # total pop. of a tract 
  year = 2020,
  geometry = TRUE,
  resolution = "20m")

# merge geo data with sr data frame 
sr_2020_LA_geo <- sr_2020_LA %>% left_join(geo %>% select(GEO_ID_tract = GEOID, geometry), by = 'GEO_ID_tract')

max <- max(sr_2020_LA_geo$CRRALL)
splits <- c(0,25,50,75,100)

sr_2020_LA_geo <- sr_2020_LA_geo %>% 
  mutate(CRRALL_fctr = case_when(
    CRRALL < splits[1] ~ paste0("Less than ",splits[1],"%"),
    CRRALL >= splits[1] & CRRALL < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    CRRALL >= splits[2] & CRRALL < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    CRRALL >= splits[3] & CRRALL <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    CRRALL >= splits[4] & CRRALL <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    CRRALL > splits[4] ~ paste0("Greater than ", splits[5], "%")))

sr_2020_LA_geo$CRRALL_fctr <- as.factor(sr_2020_LA_geo$CRRALL_fctr)

# 2. 
# Plot 
reponse_map <- sr_2020_LA_geo %>% 
  ggplot(aes(fill = CRRALL_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  # scale_fill_viridis_c(option = "magma") + 
  scale_fill_brewer(palette = "PuOr") + 
  ggtitle("Response Rate by Census Tract - 2020") +
  labs(fill = "Cumulative Self-Response\nRate - Overall (%)",
            caption = "Census tracts shaded in white indicate\nno self responsedata reported") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) +
  theme_void() + 
  titles_upper()


ggsave(filename = "../../AAJC Vis/case_studies/los_angeles/resp_by_tract_map_1.png",
       plot = response_map, bg = "white", width =9.07, height = 12)
  





# ==========================
#  map of % of AA community that is foreign born by tract within LA County
# ==========================

# 2020 

# ------
# inspect/find variable
# ------
acs_vars <- load_variables(2020, "acs5", cache = TRUE)

# variable list also available https://www2.census.gov/programs-surveys/acs/summary_file/2020/documentation/user_tools/ACS2020_Table_Shells.xlsx

# B06004D_005 - Estimate!!Total:!!Foreign born - PLACE OF BIRTH (ASIAN ALONE) IN THE UNITED STATES
# B06004D_001 - Estimate!!Total - PLACE OF BIRTH (ASIAN ALONE) IN THE UNITED STATES

# B06004E_005 - Estimate!!Total:!!Foreign born - PLACE OF BIRTH (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) IN THE UNITED STATES
# B06004E_001 - Estimate!!Total - PLACE OF BIRTH (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) IN THE UNITED STATES

fborn_vars <- c('B06004D_005','B06004D_001', 'B06004E_005', 'B06004E_001')


# ------
# pull 5 year acs data 
# ------

foreign_born <- get_acs(geography = "tract",
        state = "CA",
        county = "Los Angeles County",
        variables = fborn_vars, 
        year = 2020,
        geometry = TRUE,
        resolution = "20m")

# ------
# Clean data for mapping
# ------

# replace this with NHPI_A for NHPI
race <- 'NHPI_A'

# 1. 
# race column for Asian/NHPI pops
foreign_born_perc <- foreign_born %>% mutate(RACE = case_when(variable %in% c('B06004D_005','B06004D_001') ~ 'A_A',
                                         variable %in% c('B06004E_005', 'B06004E_001') ~ 'NHPI_A'),
                        # renaming the variables to something more readable 
                        variable = case_when(variable == 'B06004D_005' ~ 'foreign',
                                             variable == 'B06004D_001' ~ 'total_pop',
                                             variable == 'B06004E_005' ~ 'foreign',
                                             variable == 'B06004E_001' ~ 'total_pop')) %>%
  filter(RACE == race) %>% group_by(GEOID, NAME) %>% 
  summarise(percent_foreign = round(((estimate[variable == 'foreign'] / estimate[variable == 'total_pop'])*100),2 ))


# 2. 
# inspect NAs
foreign_born_perc[is.na(foreign_born_perc$percent_foreign),]

# tracts where total population is 0 leading to a 0 in denominator
foreign_born_perc$percent_foreign[is.na(foreign_born_perc$percent_foreign)] <- 0


# 3.
# Create percent factor column for mapping 
max <- max(foreign_born_perc$percent_foreign)
splits <- c(0,25,50,75,100)

foreign_born_perc <- foreign_born_perc %>% 
  mutate(percent_foreign_fctr = case_when(
    percent_foreign < splits[1] ~ paste0("Less than ",splits[1],"%"),
    percent_foreign >= splits[1] & percent_foreign < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    percent_foreign >= splits[2] & percent_foreign < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    percent_foreign >= splits[3] & percent_foreign <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    percent_foreign >= splits[4] & percent_foreign <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    percent_foreign > splits[4] ~ paste0("Greater than ", splits[5], "%")))

foreign_born_perc$percent_foreign_fctr <- as.factor(sforeign_born_perc$percent_foreign_fctr)

# ------
# Plot
# ------
fborn_map <- foreign_born_perc %>% 
  ggplot(aes(fill = percent_foreign_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  scale_fill_brewer(palette = "PuOr") + 
  ggtitle("Foreign Born NHPI Population - 2020") +
  labs(fill = "Percentage of NHPI Alone\nPopulation that is Foreign Born", size=1) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) +
  theme_void() + 
  titles_upper()

ggsave(filename = "../../AAJC Vis/case_studies/los_angeles/foreign_born_NHPIA.png",
       plot = fborn_map, bg = "white", width =6.07, height = 5.46)






# ==========================
#  scatterplot from above with dots colored by citizenship
# ==========================


