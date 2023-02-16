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









# ==========================
# map of response rate for 2020 by census tract for NYC
# ==========================

# 1. 
# Get geospatial data for county by tract 

geo <- get_decennial(
  geography = "tract",
  state = "NY",
  county = ny_boroughs,
  variables = 'P1_001N', # total pop. of a tract 
  year = 2020,
  geometry = TRUE,
  cb = TRUE,
  resolution = "20m")

nyc_county_overlay <- get_decennial(
  geography = "county",
  state = "NY",
  county = ny_boroughs,
  variables = 'P1_001N', # total pop. of a tract 
  year = 2020,
  geometry = TRUE,
  cb = FALSE,
  resolution = "20m")

# merge geo data with sr data frame 
sr_2020_NY_geo <- sr_2020_NY %>% full_join(geo %>% select(GEO_ID_tract = GEOID, geometry), by = 'GEO_ID_tract')

splits <- c(0,25,50,75,100)

sr_2020_NY_geo <- sr_2020_NY_geo %>% 
  mutate(CRRALL_fctr = case_when(
    CRRALL < splits[1] ~ paste0("Less than ",splits[1],"%"),
    CRRALL >= splits[1] & CRRALL < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    CRRALL >= splits[2] & CRRALL < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    CRRALL >= splits[3] & CRRALL <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    CRRALL >= splits[4] & CRRALL <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    CRRALL > splits[4] ~ paste0("Greater than ", splits[5], "%")))

sr_2020_NY_geo$CRRALL_fctr <- as.factor(sr_2020_NY_geo$CRRALL_fctr)

# 2. 
# Plot 
resp_NYC <- sr_2020_NY_geo %>% 
  ggplot(aes(fill = CRRALL_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  # geom_sf(data = nyc_county_overlay, fill = NA, color='black', size=.3) +
  scale_fill_brewer(palette = "PuOr",na.translate=FALSE) +
  # scale_fill_viridis_c(option = "magma") +
  ggtitle("          Response Rate by Census Tract - 2020") +
  labs(fill = "Cumulative Self-Response\nRate - Overall (%)",
       caption = "Census tracts shaded in white indicate no self response data reported") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1),
  ) +
  theme_void()
# titles_upper()


ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/resp_by_tract_map_1.png",
       plot = resp_NYC, bg = "white")








# ==========================
#  map of % of AA community that is foreign born by tract within NYC
# ==========================

# 2020 

# ------
# inspect/find variable
# ------
# acs_vars <- load_variables(2020, "acs5", cache = TRUE)

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
                        state = "NY",
                        county = ny_boroughs,
                        variables = fborn_vars, 
                        year = 2020)

# ------
# Clean data for mapping
# ------

# replace this with NHPI_A for NHPI
race <- 'A_A'

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

foreign_born_perc$percent_foreign_fctr <- as.factor(foreign_born_perc$percent_foreign_fctr)

# merge geo data with sr data frame 
foreign_born_perc <- foreign_born_perc %>% full_join(geo %>% select(GEOID, geometry), by = 'GEOID')

# ------
# Plot
# ------
fborn_map <- foreign_born_perc %>% 
  ggplot(aes(fill = percent_foreign_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  scale_fill_brewer(palette = "PuOr",na.translate=FALSE) + 
  ggtitle("Foreign Born Asian Population - 2020") +
  labs(fill = "Percentage of Asian Alone\nPopulation that is Foreign Born", size=1) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) +
  theme_void()
  # titles_upper()

ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/foreign_born_A.png",
       plot = fborn_map, bg = "white")






# ==========================
#  scatterplot from above with dots colored by citizenship
# ==========================

# CALCULATION - % of Asian population that are citizens 

# Citizenship = [Over 18 Males (native) + Over 18 Males (naturalized citizen) + 
#                Under 18 Males (native) + Under 18 Males (naturalized citizen) +
#                Over 18 Females (native) + Over 18 Males (naturalized citizen) +
#                Under 18 Females (native) + Under 18 Males (naturalized citizen)] / Total Asian Alone Population 


# VARIABLES FROM ACS:

# B05003D_001	 - Total

# B05003D_009 - Over 18 Males (native)
# B05003D_011 - Over 18 Males (naturalized citizen)
# B05003D_004 - Under 18 Males (native)
# B05003D_006 - Under 18 Males (naturalized citizen)

# B05003D_020 - Over 18 Females (native)
# B05003D_022 -  Over 18 Males (naturalized citizen)
# B05003D_015 - Under 18 Females (native)
# B05003D_017 - Under 18 Males (naturalized citizen)

asian_citizen_vars <- c("B05003D_001", "B05003D_009", "B05003D_011", "B05003D_004", "B05003D_006",
                        "B05003D_020", "B05003D_022", "B05003D_015", "B05003D_017")


# ------
# pull 5 year acs data 
# ------

citizenship <- get_acs(geography = "tract",
                       state = "NY",
                       county = ny_boroughs,
                       variables = asian_citizen_vars, 
                       year = 2020)


# ------
# citizenship calculation 
# ------

# filter out total population data 
citizenship_totalpop <- citizenship %>% filter(variable == "B05003D_001") %>% mutate(variable = "total_race_pop")

# filter out total population data & sum the rest
citizenship <- citizenship %>% filter(variable != "B05003D_001") %>% group_by(GEOID, NAME) %>%
  summarise(citizen = sum(estimate))

# join total pop and total citizenship data
citizenship <- citizenship %>% left_join(citizenship_totalpop %>% select(GEOID, NAME, total_pop = estimate), by = c('GEOID', 'NAME'))

# calculation 
citizenship <- citizenship %>% mutate(citizenship_perc = round(((citizen/total_pop)*100),2) )

# inspect NAs 
nas <- citizenship[is.na(citizenship$citizenship_perc),]

# all 0s - 0 denominator issue - replace with 0
citizenship$citizenship_perc[is.na(citizenship$citizenship_perc)] <- 0



# Non-citizen map # 
# -----------------

# add non cit col 
citizenship <- citizenship %>% mutate(non_citizenship_perc = 100 - citizenship_perc) 


# Create percent factor column for mapping 
max <- max(citizenship$non_citizenship_perc)
splits <- c(0,25,50,75,100)

citizenship <- citizenship %>% 
  mutate(non_citizenship_perc_fctr = case_when(
    non_citizenship_perc < splits[1] ~ paste0("Less than ",splits[1],"%"),
    non_citizenship_perc >= splits[1] & non_citizenship_perc < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    non_citizenship_perc >= splits[2] & non_citizenship_perc < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    non_citizenship_perc >= splits[3] & non_citizenship_perc <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    non_citizenship_perc >= splits[4] & non_citizenship_perc <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    non_citizenship_perc > splits[4] ~ paste0("Greater than ", splits[5], "%")))

citizenship$non_citizenship_perc_fctr <- as.factor(citizenship$non_citizenship_perc_fctr)

# add geometry 
citizenship_plot <- foreign_born_perc %>% left_join(citizenship  %>% select(NAME, non_citizenship_perc_fctr), by = 'NAME')

# ------
# Plot
# ------
non_citizen_map <- citizenship_plot %>% 
  ggplot(aes(fill = non_citizenship_perc_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  scale_fill_brewer(palette = "PuOr") + 
  ggtitle("Non-Citizen Asian Alone Population - 2020") +
  labs(fill = "Percentage Non-citizen ", size=1) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) +
  theme_void() + 
  titles_upper()


# save 
ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/non_citizenship_map_AA_2020.png",
       plot = non_citizen_map, bg = "white")


# ------
# join with self response data  
# ------

sr_2020_NY <- sr_2020_NY %>% left_join(citizenship %>% select(GEO_ID_tract = GEOID, citizenship_perc), by = 'GEO_ID_tract')


# turn into factor for plotting 
sr_2020_NY <- sr_2020_NY %>% 
  mutate(citizenship_perc_fctr = case_when(
    citizenship_perc < splits[1] ~ paste0("Less than ",splits[1],"%"),
    citizenship_perc >= splits[1] & citizenship_perc < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    citizenship_perc >= splits[2] & citizenship_perc < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    citizenship_perc >= splits[3] & citizenship_perc <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    citizenship_perc >= splits[4] & citizenship_perc <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    citizenship_perc > splits[4] ~ paste0("Greater than ", splits[5], "%")))

sr_2020_NY$citizenship_perc_fctr <- as.factor(sr_2020_NY$citizenship_perc_fctr)


# ------
# plot 
# ------
scatter_response_color <- sr_2020_NY %>% filter(RACE == 'A_A') %>%
  ggplot(aes(x = pop_percentage, y = CRRALL, size = total_tract_pop, color = citizenship_perc_fctr)) + 
  geom_point(alpha = .8) + 
  scale_color_brewer(palette = "PuOr") +
  theme_minimal() + 
  xlab("Asian (alone) Population (%)") + 
  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
  ggtitle("Response Rate by Percentage of Asian Population and Citizenship Status",
          subtitle =  "Census Tract - 2020") + 
  labs(color = "Citizenship of Asian\n(alone) Population (%)", size = 'Total Tract Population', size=2) + 
  theme(legend.title = element_text(size = 10), axis.title.y=element_text(size=10), axis.title.x=element_text(size=10))

scatter_response_color

ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/resp_by_citizenship_A_A_2020_SCATTER.png",
       plot = scatter_response_color, bg = "white")






















# ==========================
#  TABLE: with total population, AA Alone, AA Alone or in Combo and 
#   percentages for the city, and each county
# ==========================

# read in data 
esmr_2020 <- read.csv("../../Transformed Data/2020/ES_MR_comparison_2020.csv")


# all 5 (counties) in HI 
ny_boroughs

# total population of Hawaii
# vars2020 <- load_variables(2020, "pl", cache = TRUE)

tot_ny <- get_decennial(geography = "state",
                            variables = c('P1_001N'),
                            state = "New York",
                            year = 2020)


tot_ny = tot_ny$value

# aggregate county populations
esmr_2020_counties <- esmr_2020 %>% filter(CTYNAME %in% ny_boroughs & STNAME == "New York") %>%
  filter(RACE %in% c('A_A', 'A_AIC')) %>% select(STNAME,CTYNAME,RACE,MR) %>%
  group_by(STNAME, CTYNAME, RACE) %>% 
  summarise(MR = sum(MR))

# aggreagte state pop
esmr_2020_st <- esmr_2020 %>% filter(STNAME == "New York") %>%
  filter(RACE %in% c('A_A', 'A_AIC')) %>% select(STNAME,CTYNAME,RACE,MR) %>%
  group_by(STNAME, RACE) %>% 
  summarise(MR = sum(MR))

esmr_2020_st$CTYNAME <- 'New York State'
esmr_2020_st <- esmr_2020_st %>% select(STNAME, CTYNAME, RACE,MR)

# join data 
esmr_2020_NY <- rbind(esmr_2020_counties, esmr_2020_st)

# get percentages
esmr_2020_NY %>% mutate(percent_nhpi = round(((MR/tot_ny)*100),3) ) %>% filter(RACE == "A_AIC")








##################################################################
#                                                                # 
# BY THE NUMBERS                                                 #
#                                                                #
##################################################################


# ======================================
# (d) Chart of the top 10 sub-ethnicities (for AA) or top 5 sub-ethnicities (for NHPI)
#       in the current geography versus the nation as a whole (use the most recent 5 year ACS)
# ======================================

# ----------
# 1. AA 
# ----------

# Table ID - B02018
#   ASIAN ALONE OR IN ANY COMBINATION BY SELECTED GROUPS

acs_vars <- load_variables(2020, "acs5", cache = TRUE)

asian_groups_vars <- acs_vars[acs_vars$concept == "ASIAN ALONE OR IN ANY COMBINATION BY SELECTED GROUPS",]

# ------
# pull 5 year acs data 
# ------

subethnicity_aa_20 <- get_acs(geography = "county",
                              state = "NY",
                              county = ny_boroughs,
                              table = 'B02018', 
                              year = 2020)

subethnicity_aa_20_NATIONAL <- get_acs(geography = "us",
                                       table = 'B02018', 
                                       year = 2020)

# get sub ethnicity labels 
subethnicity_aa_20 <- subethnicity_aa_20 %>% left_join(asian_groups_vars %>% select('variable' = name, label), by='variable')
subethnicity_aa_20_NATIONAL <- subethnicity_aa_20_NATIONAL %>% 
  left_join(asian_groups_vars %>% select('variable' = name, label), by='variable')

# extract sub ethnicity from label column
subethnicity_aa_20$label <- sub(".*Estimate!!Total Groups Tallied:!!", "", subethnicity_aa_20$label) 
subethnicity_aa_20_NATIONAL$label <- sub(".*Estimate!!Total Groups Tallied:!!", "", subethnicity_aa_20_NATIONAL$label) 

# remove total asian population count 
subethnicity_aa_20 <- subethnicity_aa_20[subethnicity_aa_20$variable != 'B02018_001',]
subethnicity_aa_20_NATIONAL <- subethnicity_aa_20_NATIONAL[subethnicity_aa_20_NATIONAL$variable != 'B02018_001',]

# aggregate all boroughs
subethnicity_aa_20 <- subethnicity_aa_20 %>% group_by(label) %>% summarise(estimate = sum(estimate))

subethnicity_aa_20 <- subethnicity_aa_20 %>% top_n(10, wt=estimate) %>% arrange(desc(estimate))
subethnicity_aa_20_NATIONAL <- subethnicity_aa_20_NATIONAL %>% top_n(10, wt=estimate) %>% arrange(desc(estimate))


# change estimate values for readability in plot 
subethnicity_aa_20$estimate <- subethnicity_aa_20$estimate/1000
subethnicity_aa_20_NATIONAL$estimate <- subethnicity_aa_20_NATIONAL$estimate/1000

# Save for Alysha
# write.csv(subethnicity_aa_20, "../../Transformed Data/data for viz_alysha/top_eth_ny.csv")

subethnicity_aa_20$label <- sub(",.*", "", subethnicity_aa_20$label) 
subethnicity_aa_20_NATIONAL$label <- sub(",.*", "", subethnicity_aa_20_NATIONAL$label)

# facet grid 
subethnicity_aa_20$NAME <- sub(",.*", "", subethnicity_aa_20$NAME)
subethnicity_aa_20_facet <- rbind(subethnicity_aa_20,subethnicity_aa_20_NATIONAL)

# ------
# plot - NYC
# ------

# US - #f4c78d

ethnicities_bar <- subethnicity_aa_20 %>% 
  ggplot(aes(x=reorder(label,estimate),y=estimate)) + 
  geom_bar(stat = 'identity',fill="#916a92")+
  theme_minimal()+
  labs(subtitle = "New York City - 2020")+
  theme(
        plot.caption=element_text(size=6, hjust=1, vjust = .5, face="italic"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  coord_flip()

ethnicities_bar

# ------
# plot - National
# ------

ethnicities_bar_NATIONAL <- subethnicity_aa_20_NATIONAL %>% 
  ggplot(aes(x=reorder(label,estimate),y=estimate)) + 
  geom_bar(stat = 'identity',fill="#f4c78d")+
  theme_minimal()+
  xlab("") + 
  ylab("") + 
  labs(
    subtitle = "United States - 2020")+
  # caption = "\"Chinese\" population excludes Taiwanese individuals.\n\"Other Asian\" groups are not specified in the ACS data") +
  theme(
        plot.caption=element_text(size=6, hjust=1, vjust = .5, face="italic"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+ 
  coord_flip()

ethnicities_bar_NATIONAL


# ------
# plot - FACET 
# ------

ethnicities_bar_FACET <- grid.arrange(ethnicities_bar, ethnicities_bar_NATIONAL, nrow=1,
                                      top = textGrob("Top 10 Asian (alone or in combination) Subgroups",gp=gpar(fontsize=14)),
                                      bottom = textGrob("Estimates (thousands)"),
                                      left="Subgroup")

ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/sub_ethn_AA_FACET_2020_horizontal.png", 
       plot = ethnicities_bar_FACET, bg = "white", width = 6.75, height = 4)

# NOTE - facet plot does not contain caption


# ------
# plot - king county
# ------

# US - #f4c78d

ethnicities_bar <- subethnicity_aa_20 %>% 
  ggplot(aes(x=reorder(label,-estimate),y=estimate)) + 
  geom_bar(stat = 'identity',fill="#916a92")+
  theme_minimal()+
  xlab("Ethnicity") +
  ylab("Estimate (thousands)") +
  ggtitle("Top 10 Asian (alone or in combination) Ethnicity Groups")+
  labs(subtitle = "New York City - 2020",
       caption = "\"Chinese\" population excludes Taiwanese individuals.\n\"Other Asian\" groups are not specified in the ACS data") +
  theme(axis.text.x = element_text(angle=30),
        plot.caption=element_text(size=6, hjust=1, vjust = .5, face="italic"))
# axis.title.x=element_blank(),
# axis.title.y=element_blank())

ethnicities_bar
ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/sub_ethn_AA_2020.png", 
       plot = ethnicities_bar, bg = "white")



# ------
# plot - National
# ------

ethnicities_bar_NATIONAL <- subethnicity_aa_20_NATIONAL %>% 
  ggplot(aes(x=reorder(label,-estimate),y=estimate)) + 
  geom_bar(stat = 'identity',fill="#f4c78d")+
  theme_minimal()+
  xlab("Ethnicity") +
  ylab("Estimate (thousands)") +
  labs(title = "Top 10 Asian (alone or in combination) Ethnicity Groups",
       subtitle = "United States - 2020",
       caption = "\"Chinese\" population excludes Taiwanese individuals.\n\"Other Asian\" groups are not specified in the ACS data") +
  theme(axis.text.x = element_text(angle=30),
        plot.caption=element_text(size=6, hjust=1, vjust = .5, face="italic"))

ethnicities_bar_NATIONAL

ggsave(filename = "../../AAJC Vis/case_studies/new_york_city/sub_ethn_AA_NATIONAL_2020.png", 
       plot = ethnicities_bar_NATIONAL, bg = "white")











# ======================================
# (e) Citizenship status for the race group we are looking at in the county versus the state, versus the nation (ACS 5 year)
# (g) English ability by citizenship status for the race group we are talking about in the county versus the state versus 
#     the nation (ACS 5 year)
# ======================================


# =====
# AA
# =====

# 1.
# ------
# Pulling ACS 5 year data - citizenship
# ------

# Table - SEX BY AGE BY NATIVITY AND CITIZENSHIP STATUS (ASIAN ALONE)
# Table ID - B05003D

# Native = Under 18 & Native (F) + Under 18 & Native (M) + Over 18 and Native (F) + Over 18 and Native (M)
# Foreign born (naturalized citizen) = Under 18 & Foreign born (Naturalized citizen M + F) + Over 18 & Foreign born (Naturalized citizen M +F)
# Foreign born (not citizen) = Under 18 & Foreign born (not citizen M + F) + Over 18 & Foreign born (non citizen M+ F)

asian_citizen_vars <- c("B05003D_004", "B05003D_009", 'B05003D_015', 'B05003D_020', # native vars
                        "B05003D_006", "B05003D_011", "B05003D_017", "B05003D_022", # fborn - naturalized vars
                        "B05003D_007", "B05003D_012", "B05003D_018", "B05003D_023") # fborn - non citizen

# ------
# pull 5 year acs data 
# ------

citizenship <- get_acs(geography = "county",
                       state = "NY",
                       variables = asian_citizen_vars, 
                       year = 2020)


# ------
# citizenship calculation 
# ------

native <- c("B05003D_004", "B05003D_009", 'B05003D_015', 'B05003D_020')
fborn_naturalized <- c( "B05003D_006", "B05003D_011", "B05003D_017", "B05003D_022")
fborn_non_citzn <- c("B05003D_007", "B05003D_012", "B05003D_018", "B05003D_023")

citizenship <- citizenship %>% mutate(citizenship_status = case_when(
  variable %in% native ~ "Native",
  variable %in% fborn_naturalized ~ "Foreign born - naturalized citizen",
  variable %in% fborn_non_citzn ~ "Foreign born - non citizen"
)) %>% # aggregate
  group_by(GEOID, NAME, citizenship_status) %>%
  summarise(estimate = sum(estimate))


# ------
# get county and state level info
# ------

ny_borough_full <- c("New York County, New York", "Kings County, New York", "Bronx County, New York",
                     "Richmond County, New York","Queens County, New York")
citizenship_county <- citizenship %>% filter(NAME %in% ny_borough_full)

citizenship_city <- citizenship_county %>% group_by(citizenship_status) %>% summarise(estimate = sum(estimate)) %>%
  mutate(NAME = 'New York City', GEOID = 'City') %>%
  select(GEOID, NAME, citizenship_status, estimate)

citizenship_state <- citizenship %>% group_by(citizenship_status) %>%
  summarise(estimate = sum(estimate)) %>% mutate(NAME = 'New York', GEOID = '36') %>%
  select(GEOID, NAME, citizenship_status, estimate)


# ------
# get US as a while data
# ------

citizenship_US <- get_acs(geography = 'us',
                          variables = asian_citizen_vars, 
                          year = 2020)

citizenship_US <- citizenship_US %>% mutate(citizenship_status = case_when(
  variable %in% native ~ "Native",
  variable %in% fborn_naturalized ~ "Foreign born - naturalized citizen",
  variable %in% fborn_non_citzn ~ "Foreign born - non citizen"
)) %>% # aggregate
  group_by(GEOID, NAME, citizenship_status) %>%
  summarise(estimate = sum(estimate))

# ------
# combine county, state, and US data 
# ------

citizenship <- rbind(citizenship_county,citizenship_city, citizenship_state, citizenship_US)


# 2.
# ------
# Pulling ACS 5 year data - English Ability
# ------

# Table - NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (ASIAN ALONE)
# Table ID - B16005D


english <- get_acs(geography = "county",
                   state = "NY",
                   table = 'B16005D', 
                   year = 2020)

native <- c('B16005D_003',
            'B16005D_004',
            'B16005D_005',
            'B16005D_006')

fborn <- c('B16005D_008',
           'B16005D_009',
           'B16005D_010',
           'B16005D_011')

only_english <- c('B16005D_003', 'B16005D_008')
another_lang <- c('B16005D_004', 'B16005D_009')
english_well <- c('B16005D_005', 'B16005D_010')
english_lessthan_well <- c('B16005D_006', 'B16005D_011')

english <- english %>% mutate(citizenship_status = case_when(
  variable %in% native ~ 'Native',
  variable %in% fborn ~ 'Foreign Born'
), english_ability = case_when(
  variable %in% only_english ~ 'Speak only English',
  variable %in% another_lang ~ 'Speak another language',
  variable %in% english_well ~ 'Speak English very well',
  variable %in% english_lessthan_well ~ 'Speak English less than very well'
))


# ------
# get county and state data 
# ------

english_county <- english %>% filter(NAME %in% ny_borough_full) %>% drop_na(citizenship_status) %>% 
  select(GEOID, NAME, citizenship_status, english_ability, estimate)

english_city <- english_county %>% group_by(citizenship_status, english_ability) %>%
  summarise(estimate = sum(estimate)) %>% drop_na(citizenship_status) %>% 
  mutate(GEOID = 'City', NAME = 'New York City') %>%
  select(GEOID, NAME, citizenship_status, english_ability, estimate)


english_state <- english %>% group_by(citizenship_status, english_ability) %>%
  summarise(estimate = sum(estimate)) %>% drop_na(citizenship_status) %>% 
  mutate(GEOID = '36', NAME = 'New York') %>%
  select(GEOID, NAME, citizenship_status, english_ability, estimate)


# ------
# get US Data 
# ------

english_US <- get_acs(geography = "us",
                      table = 'B16005D', 
                      year = 2020)


english_US <- english_US %>% mutate(citizenship_status = case_when(
  variable %in% native ~ 'Native',
  variable %in% fborn ~ 'Foreign Born'
), english_ability = case_when(
  variable %in% only_english ~ 'Speak only English',
  variable %in% another_lang ~ 'Speak another language',
  variable %in% english_well ~ 'Speak English very well',
  variable %in% english_lessthan_well ~ 'Speak English less than very well'
)) %>% drop_na(citizenship_status) %>%
  select(GEOID, NAME, citizenship_status, english_ability, estimate)


# ------
# combine county, state, and US data 
# ------

english <- rbind(english_county,english_city, english_state, english_US)

# ------
# export citizenship & english data to an excel sheet 
# ------
library(openxlsx)
datasets <- list("citizenship" = data.frame(citizenship), "english" = data.frame(english))
write.xlsx(datasets, file = "../../Transformed Data/2020/citizenship_english_ability_NYC.xlsx")








# ======================================
# (f) Renter vs Owner for the race group we are talking about in the county versus the state versus the nation (ACS 5 year)
# ======================================

# =====
# AA
# =====

# 1.
# ------
# Pulling ACS 5 year data
# ------

# Table - TENURE (ASIAN ALONE HOUSEHOLDER)
# Table ID - B25003D


housing <- get_acs(geography = "county",
                   state = "New York",
                   table = 'B25003D', 
                   year = 2020)


housing <- housing %>% mutate(variable = case_when(
  variable == "B25003D_001" ~ 'total',
  variable == "B25003D_002" ~ 'owner',
  variable == "B25003D_003" ~ 'renter'
))

# get county and state data 
housing_county <- housing %>% filter(NAME %in% ny_borough_full) %>% select(GEOID, NAME, variable, estimate)

housing_city <- housing_county %>% group_by(variable) %>% summarise(estimate =sum(estimate)) %>% mutate(
  GEOID = 'NYC', NAME = 'NYC'
) %>% select(GEOID, NAME, variable, estimate)

housing_state <- housing %>% group_by(variable) %>% summarise(estimate =sum(estimate)) %>% mutate(
  GEOID = '36', NAME = 'New York'
) %>% select(GEOID, NAME, variable, estimate)

# ------
# Get national data 
# ------

housing_US <- get_acs(geography = "us",
                      table = 'B25003D', 
                      year = 2020)

housing_US <- housing_US %>% mutate(variable = case_when(
  variable == "B25003D_001" ~ 'total',
  variable == "B25003D_002" ~ 'owner',
  variable == "B25003D_003" ~ 'renter'
)) %>% select(GEOID, NAME, variable, estimate)


# ------
# Combine couty, state and national data 
# ------

housing_NY_AA <- rbind(housing_county,housing_city, housing_state, housing_US)


# export data 
datasets <- list("LA - AA" = data.frame(housing_LA_AA), "LA - NHPI" = data.frame(housing_LA_NHPI),
                 "KC - AA" = data.frame(housing_KC_AA), "HC - AA" = data.frame(housing_HC_AA),
                 "NYC - AA" = data.frame(housing_NY_AA))
write.xlsx(datasets, file = "../../Transformed Data/2020/renter_owner.xlsx")









