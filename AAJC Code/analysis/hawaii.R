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
# LINE CHART: Add line chart with overcounts and undercounts by age groups for Hawaii as well as for each county in the state and 
#             for the United States as a whole for NHPI Alone and NHPI Alone or in Combination for 2010
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
# Prepare hawaii & US data 
# using agegrp_2010_UNITED_STATES filter for race group -> A_A or A_AIC
agegrp_2010_UNITED_STATES$CTYNAME <- "United States"
agegrp_2010_UNITED_STATES <- agegrp_2010_UNITED_STATES %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)


# Hawaii County
# Maui County
# Kalawao County - dropping 
# Kauai County
# Honolulu County

# all 5 (counties) in HI 
hi_counties <- c("Hawaii County", "Maui County", "Kauai County", "Honolulu County")
agegrp_2010_HI_counties <- agegrp_2010 %>% filter(CTYNAME %in% hi_counties & STNAME == "Hawaii")  %>%
  select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)

# aggregate counties into 1 for HI
agegrp_2010_HI <- agegrp_2010_HI_counties %>% group_by(AGEGRP, RACE) %>% summarise(ESTIM = sum(ESTIM), MR = sum(MR)) %>%
  mutate(NUM_DIFF = MR - ESTIM,   # numeric diff
         PERC_DIFF = round(( (MR - ESTIM) / ( (MR + ESTIM)/2 ) * 100)  ,2),   # percent difference/error of closure (EOC)
         COVERAGE = case_when(
           NUM_DIFF < 0 ~ 'undercount',
           NUM_DIFF > 0 ~ 'overcount',
           NUM_DIFF == 0 ~ 'equal'
         ))
agegrp_2010_HI$CTYNAME <- "Hawaii"
agegrp_2010_HI <- agegrp_2010_HI %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)

# join US, NYC, and county data  
agegrp_2010_HI_USA <- rbind(agegrp_2010_HI,agegrp_2010_HI_counties, agegrp_2010_UNITED_STATES)


# 3. 
# Plot
v2_line <- agegrp_2010_HI_USA %>% filter(RACE == "NHPI_AIC") %>%
  ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = CTYNAME, linetype = CTYNAME)) +
  geom_hline(yintercept = 0, linetype='dotted', col='grey')+
  geom_line(aes(color=CTYNAME), size=.7, alpha=.7) +
  scale_linetype_manual(values = c("solid",  "dashed",  "dashed",
                                   "dashed", "dashed",
                                   "solid"),
                        name = "Region") +
  scale_color_manual(values = c("#CC5500", "#f4c78d", "#916a92", "#780116","#008148", "#0F1108"), name = "Region") +
  theme_minimal() +
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("Coverage by Age Group for NHPI (Alone or in Combination) Populations - 2010")+
  scale_x_discrete(labels = agegrp_labels)
  # annotate("text",x=18.1, y=5, label="overcount", size=2.5, color='grey') +
  # annotate("text",x=18.1, y=-5, label="undercount", size=2.5, color='grey')

# change age group labels 
v2_line <- v2_line + theme(axis.text.x = element_text(angle=45)) 

ggsave(filename = "../../AAJC Vis/case_studies/hawaii/US_AND_HI_line_graph_coverage_by_agegrp_NHPI_AIC_2010.png",
       plot = v2_line, bg = "white", width =10.0, height = 5.47)




# ==========================
# add a scatterplot of response rate by % AA by tract in New York City
# ==========================

# read in self response data 
sr_2020 <- read.csv("../../Raw Data/2020/hawaii_selfresponse_rates_2020_by_tract.csv")


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

# store nhpi variables 
nhpi_vars <- vars_2020 %>% filter(grepl('Native Hawaiian', label) & concept == "RACE")
nhpi_vars <- nhpi_vars$name

# api call - pulling census data 
hi_census_tract_nhpi <- get_decennial(geography = "tract",
                                      variables = nhpi_vars,
                                      state = "HI",
                                      county = hi_counties,
                                      summary_var = 'P1_001N', # total pop. of a tract 
                                      year = 2020)
# -------
# Preparing SR and Census Data 
# --------

# fix GEOID column in self response DF to match and join later with census DF
sr_2020 <- sr_2020 %>% separate(GEO_ID, c('A','GEO_ID_tract'), sep = 'US') %>% select(-A)

# filtering self response data 
sr_2020_HI <- sr_2020 %>% filter(COUNTY %in% c(" Hawaii County", " Maui County", " Kalawao County", " Kauai County", " Honolulu County")
                                 & STATE == " Hawaii")


# -------
# Aggregating all 'in combination' race variables for census population 
# --------

# create new DFs for aic values and alone values 
hi_census_tract_aic <- hi_census_tract_nhpi %>%group_by(GEOID, summary_value) %>%
  summarise(value = sum(value)) %>% mutate(RACE = "NHPI_AIC") %>% rename('total_tract_pop' = summary_value)

hi_census_tract_a <- hi_census_tract_nhpi %>% mutate(RACE = case_when(variable == "P1_007N" ~ 'NHPI_A'), 
                                                     value = case_when(
                                                       variable == "P1_007N" ~ value)) %>%
  filter(!is.na(value)) %>% select(-NAME, -variable) %>% rename('total_tract_pop' = summary_value)

# combine the two DFs 
hi_census <- rbind(hi_census_tract_aic, hi_census_tract_a) %>% arrange(GEOID, RACE)


# -------
# Adding percentage calculations
# --------

# Compute % of Asian populations for scatter plot 
hi_census <- hi_census %>% mutate(pop_percentage = round((value/total_tract_pop)*100, 2))

# joining self response and census population data
sr_2020_HI <- sr_2020_HI %>% left_join(hi_census %>% select(GEO_ID_tract = GEOID, total_tract_pop, RACE, value, pop_percentage), by = 'GEO_ID_tract')


# -------
# Plot
# --------

scatter_response <- sr_2020_HI %>% filter(RACE == 'NHPI_A') %>%
  ggplot(aes(x = pop_percentage, y = CRRALL, size = total_tract_pop)) + 
  geom_point(color = "#e49d48", alpha = 0.7) + 
  theme_minimal() + 
  xlab("NHPI (Alone) Population (%)") + 
  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
  ggtitle("Response Rate by Percentage of NHPI Population by Census Tract - 2020")

ggsave(filename = "../../AAJC Vis/case_studies/hawaii/resp_by_tract_pop_scatter_NHPI_A_2020_SIZE.png",
       plot = scatter_response, bg = "white", width =9.07, height = 5.47)

# light or - e49d48
# dark or - ac550f
#      

# 2 options 
#   1) points are not sized according to census population & no alpha 
#   2) points sized by census population and alpha to make them lighter for readablity 









# ==========================
# map of response rate for 2020 by census tract for Harris County
# ==========================

# 1. 
# Get geospatial data for county by tract 

geo <- get_decennial(
  geography = "tract",
  state = "HI",
  county = hi_counties,
  variables = 'P1_001N', # total pop. of a tract 
  year = 2020,
  geometry = TRUE,
  cb = FALSE,
  resolution = "20m")

# merge geo data with sr data frame 
sr_2020_HI_geo <- sr_2020_HI %>% left_join(geo %>% select(GEO_ID_tract = GEOID, geometry), by = 'GEO_ID_tract')

splits <- c(0,25,50,75,100)

sr_2020_HI_geo <- sr_2020_HI_geo %>% 
  mutate(CRRALL_fctr = case_when(
    CRRALL < splits[1] ~ paste0("Less than ",splits[1],"%"),
    CRRALL >= splits[1] & CRRALL < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    CRRALL >= splits[2] & CRRALL < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    CRRALL >= splits[3] & CRRALL <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    CRRALL >= splits[4] & CRRALL <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    CRRALL > splits[4] ~ paste0("Greater than ", splits[5], "%")))

sr_2020_HI_geo$CRRALL_fctr <- as.factor(sr_2020_HI_geo$CRRALL_fctr)

# 2. 
# Plot 
sr_2020_HI_geo %>% 
  ggplot(aes(fill = CRRALL_fctr, geometry = geometry))+
  # geom_sf(data = nyc_county_overlay, fill = '#d7f2f9', color='black', size=.20) +
  geom_sf(color = "black", size = 0.06) +
  # geom_sf(data = nyc_county_overlay, color = 'black', size = 0.2) +
  scale_fill_brewer(palette = "PuOr") +
  # scale_fill_viridis_c(option = "magma") +
  ggtitle("          Response Rate by Census Tract - 2020") +
  labs(fill = "Cumulative Self-Response\nRate - Overall (%)") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1),
  ) +
  theme_void()
  # titles_upper()

# caption = "Census tracts shaded in white indicate no self responsedata reported"








# ==========================
#  map of % of AA community that is foreign born by tract within Hawaii
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
                        state = "HI",
                        county = hi_counties,
                        variables = fborn_vars, 
                        year = 2020,
                        geometry = TRUE,
                        cb = TRUE,
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
# max <- max(foreign_born_perc$percent_foreign)
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

# merge geo data with forein born data frame 
# foreign_born_perc <- foreign_born_perc %>% left_join(geo %>% select(GEOID, geometry), by = 'GEOID')


# ------
# Plot
# ------
fborn_map <- foreign_born_perc %>% 
  ggplot(aes(fill = percent_foreign_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  scale_fill_brewer(palette = "PuOr") + 
  ggtitle("Foreign Born NHPI Population - 2020") +
  labs(fill = "Percentage of Asian NHPI\nPopulation that is Foreign Born", size=1) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) +
  theme_void()

fborn_map

ggsave(filename = "../../AAJC Vis/case_studies/hawaii//foreign_born_A.png",
       plot = fborn_map, bg = "white")










# ==========================
#  scatterplot from above with dots colored by citizenship
# ==========================

# CALCULATION - % of Asian population that are citizens 

# Citizenship = [Over 18 Males (native) + Over 18 Males (naturalized citizen) + 
#                Under 18 Males (native) + Under 18 Males (naturalized citizen) +
#                Over 18 Females (native) + Over 18 Males (naturalized citizen) +
#                Under 18 Females (native) + Under 18 Males (naturalized citizen)] / Total Asian Alone Population 


nhpi_citizen_vars <- c("B05003E_001", "B05003E_009", "B05003E_011", "B05003E_004", "B05003E_006",
                       "B05003E_020", "B05003E_022", "B05003E_015", "B05003E_017")


# ------
# pull 5 year acs data 
# ------

citizenship <- get_acs(geography = "tract",
                       state = "Hawaii",
                       county = hi_counties,
                       variables = nhpi_citizen_vars, 
                       year = 2020)


# ------
# citizenship calculation 
# ------

# filter out total population data 
citizenship_totalpop <- citizenship %>% filter(variable == "B05003E_001") %>% mutate(variable = "total_race_pop")

# filter out total population data & sum the rest
citizenship <- citizenship %>% filter(variable != "B05003E_001") %>% group_by(GEOID, NAME) %>%
  summarise(citizen = sum(estimate))

# join total pop and total citizenship data
citizenship <- citizenship %>% left_join(citizenship_totalpop %>% select(GEOID, NAME, total_pop = estimate), by = c('GEOID', 'NAME'))

# calculation 
citizenship <- citizenship %>% mutate(citizenship_perc = round(((citizen/total_pop)*100),2) )

# inspect NAs 
nas <- citizenship[is.na(citizenship$citizenship_perc),]

# all 0s - 0 denominator issue - replace with 0
citizenship$citizenship_perc[is.na(citizenship$citizenship_perc)] <- 0


# ------
# join with self response data  
# ------

sr_2020_HI <- sr_2020_HI %>% left_join(citizenship %>% select(GEO_ID_tract = GEOID, citizenship_perc), by = 'GEO_ID_tract')


# turn into factor for plotting 
sr_2020_HI <- sr_2020_HI %>% 
  mutate(citizenship_perc_fctr = case_when(
    citizenship_perc < splits[1] ~ paste0("Less than ",splits[1],"%"),
    citizenship_perc >= splits[1] & citizenship_perc < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    citizenship_perc >= splits[2] & citizenship_perc < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    citizenship_perc >= splits[3] & citizenship_perc <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    citizenship_perc >= splits[4] & citizenship_perc <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    citizenship_perc > splits[4] ~ paste0("Greater than ", splits[5], "%")))

sr_2020_HI$citizenship_perc_fctr <- as.factor(sr_2020_HI$citizenship_perc_fctr)


# ------
# plot 
# ------
scatter_response_color <- sr_2020_HI %>% filter(RACE == 'NHPI_A') %>%
  ggplot(aes(x = pop_percentage, y = CRRALL, size = total_tract_pop, color = citizenship_perc_fctr)) + 
  geom_point(alpha = .8) + 
  scale_color_brewer(palette = "PuOr") +
  theme_minimal() + 
  xlab("NHPI (alone) Population (%)") + 
  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
  ggtitle("Response Rate by Percentage of NHPI Population and Citizenship Status",
          subtitle =  "Census Tract - 2020") + 
  labs(color = "Citizenship of NHPI\n(alone) Population (%)", size = 'Total Tract Population', size=2) + 
  theme(legend.title = element_text(size = 10), axis.title.y=element_text(size=10), axis.title.x=element_text(size=10))

scatter_response_color

ggsave(filename = "../../AAJC Vis/case_studies/hawaii/resp_by_citizenship_NHPI_A_2020_SCATTER.png",
       plot = scatter_response_color, bg = "white")










# ==========================
#  TABLE: with total population, AA Alone, AA Alone or in Combo and 
#   percentages for the city, and each county
# ==========================

# read in data 
esmr_2020 <- read.csv("../../Transformed Data/2020/ES_MR_comparison_2020.csv")


# all 5 (counties) in HI 
hi_counties <- c("Hawaii County", "Maui County", "Kalawao County", "Kauai County", "Honolulu County")

# total population of Hawaii
vars2020 <- load_variables(2020, "pl", cache = TRUE)

tot_hawaii <- get_decennial(geography = "state",
                            variables = c('P1_001N'),
                            state = "Hawaii",
                            year = 2020)


tot_hawaii = tot_hawaii$value
  
# aggregate county populations
esmr_2020_counties <- esmr_2020 %>% filter(CTYNAME %in% hi_counties & STNAME == "Hawaii") %>%
  filter(RACE %in% c('NHPI_A', 'NHPI_AIC')) %>% select(STNAME,CTYNAME,RACE,MR) %>%
  group_by(STNAME, CTYNAME, RACE) %>% 
  summarise(MR = sum(MR))

# aggreagte state pop
esmr_2020_st <- esmr_2020 %>% filter(STNAME == "Hawaii") %>%
  filter(RACE %in% c('NHPI_A', 'NHPI_AIC')) %>% select(STNAME,CTYNAME,RACE,MR) %>%
  group_by(STNAME, RACE) %>% 
  summarise(MR = sum(MR))

esmr_2020_st$CTYNAME <- 'Hawaii State'
esmr_2020_st <- esmr_2020_st %>% select(STNAME, CTYNAME, RACE,MR)

# join data 
esmr_2020_HI <- rbind(esmr_2020_counties, esmr_2020_st)

# get percentages
esmr_2020_HI %>% mutate(percent_nhpi = round(((MR/tot_hawaii)*100),3) )








