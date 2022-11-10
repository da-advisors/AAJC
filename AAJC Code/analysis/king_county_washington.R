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
# LINE CHART: overcounts and undercounts by age groups for LA County and for the United States
# as a whole for AA Alone and AA Alone or in Combination for 2010
# ==========================

# read data 
agegrp_2010 <- read.csv("../../Transformed Data/2010/ES_MR_AGEGRP_comparison_2010.csv")

agegrp_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                   "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")


# -------
# V2:
#   1 chart for AA comparing LA and US and 1 for AA_AIC
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
# Prepare King County & US data 
# using agegrp_2010_UNITED_STATES filter for race group -> A_A or A_AIC
agegrp_2010_UNITED_STATES$CTYNAME <- "United States"
agegrp_2010_UNITED_STATES <- agegrp_2010_UNITED_STATES %>% select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)


# filter KC data 
agegrp_2010_KC <- agegrp_2010 %>% filter(CTYNAME == "King County" & STNAME == "Washington") %>% 
  select(CTYNAME, AGEGRP, RACE, ESTIM, MR, PERC_DIFF, COVERAGE)

# join US and KC data 
agegrp_2010_KC_USA <- rbind(agegrp_2010_KC, agegrp_2010_UNITED_STATES)



# 3. 
# Plot
v2_line <- agegrp_2010_KC_USA %>% filter(RACE == "A_A") %>%
  ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = CTYNAME)) +
  geom_hline(yintercept = 0, linetype='dotted', col='grey')+
  geom_line(aes(color=CTYNAME), size=1) +
  scale_color_manual(values = c("#916a92", "#f4c78d"), name = "Region") +
  theme_minimal() +
  xlab("Age Group") + 
  ylab("Error of Closure (%)") + 
  ggtitle("Coverage by Age Group for Asian (Alone) Populations - 2010")+
  scale_x_discrete(labels = agegrp_labels) +
  annotate("text",x=17.7, y=1, label="overcount", size=2.5, color='grey') +
  annotate("text",x=17.7, y=-1, label="undercount", size=2.5, color='grey')

# change age group labels 
v2_line <- v2_line + theme(axis.text.x = element_text(angle=45))

ggsave(filename = "../../AAJC Vis/case_studies/king_county_washington//US_AND_KC_line_graph_coverage_by_agegrp_AA_2010.png",
       plot = v2_line, bg = "white", width =9.07, height = 5.47)




# ==========================
# add a scatterplot of response rate by % AA by tract in LA County
# ==========================

# read in self response data 
sr_2020 <- read.csv("../../Raw Data/2020/washington_selfresponse_rates_2020_by_tract.csv")


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
kc_census_tract_asian <- get_decennial(geography = "tract",
                                       variables = asian_vars,
                                       state = "WA",
                                       county = "King County",
                                       summary_var = 'P1_001N', # total pop. of a tract 
                                       year = 2020)

# -------
# Preparing SR and Census Data 
# --------

# fix GEOID column in self response DF to match and join later with census DF
sr_2020 <- sr_2020 %>% separate(GEO_ID, c('A','GEO_ID_tract'), sep = 'US') %>% select(-A)


# filtering self response data 
sr_2020_KC <- sr_2020 %>% filter(COUNTY == " King County" & STATE == " Washington")


# -------
# Aggregating all 'in combination' race variables for census population 
# --------

# create new DFs for aic values and alone values 
kc_census_tract_aic <- kc_census_tract_asian %>%group_by(GEOID, summary_value) %>%
  summarise(value = sum(value)) %>% mutate(RACE = "A_AIC") %>% rename('total_tract_pop' = summary_value)

kc_census_tract_a <- kc_census_tract_asian %>% mutate(RACE = case_when(variable == "P1_006N" ~ 'A_A'), 
                                                      value = case_when(
                                                        variable == "P1_006N" ~ value)) %>%
  filter(!is.na(value)) %>% select(-NAME, -variable) %>% rename('total_tract_pop' = summary_value)

# combine the two DFs 
kc_census <- rbind(kc_census_tract_aic, kc_census_tract_a) %>% arrange(GEOID, RACE)


# -------
# Adding percentage calculations
# --------

# Compute % of Asian populations for scatter plot 
kc_census <- kc_census %>% mutate(pop_percentage = round((value/total_tract_pop)*100, 2))

# joining self response and census population data
sr_2020_KC <- sr_2020_KC %>% left_join(kc_census %>% select(GEO_ID_tract = GEOID, total_tract_pop, RACE, value, pop_percentage), by = 'GEO_ID_tract')


# -------
# Plot
# --------

# Asian Alone 
scatter_response <- sr_2020_KC %>% filter(RACE == 'A_A') %>%
  ggplot(aes(x = pop_percentage, y = CRRALL , size = total_tract_pop)) + 
  geom_point(color = "#e49d48", alpha = 0.7) + 
  theme_minimal() + 
  xlab("Asian (Alone) Population (%)") + 
  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
  ggtitle("Response Rate by Percentage of Asian Population by Census Tract - 2020") + 
  scale_shape_manual(name = "Total Tract Population")

ggsave(filename = "../../AAJC Vis/case_studies/king_county_washington//resp_by_tract_pop_scatter_AA_2020_SIZE.png",
       plot = scatter_response, bg = "white", width =9.07, height = 5.47)

# light or - e49d48
# dark or - ac550f
#     , size = total_tract_pop, alpha = 0.7

# 2 options 
#   1) points are not sized according to census population & no alpha 
#   2) points sized by census population and alpha to make them lighter for readablity 





# ==========================
# map of response rate for 2020 by census tract for King County
# ==========================

# 1. 
# Get geospatial data for county by tract 

geo <- get_decennial(
  geography = "tract",
  state = "WA",
  county = "King County",
  variables = 'P1_001N', # total pop. of a tract 
  year = 2020,
  geometry = TRUE,
  resolution = "20m")

# merge geo data with sr data frame 
sr_2020_KC_geo <- sr_2020_KC %>% left_join(geo %>% select(GEO_ID_tract = GEOID, geometry), by = 'GEO_ID_tract')

splits <- c(0,25,50,75,100)

sr_2020_KC_geo <- sr_2020_KC_geo %>% 
  mutate(CRRALL_fctr = case_when(
    CRRALL < splits[1] ~ paste0("Less than ",splits[1],"%"),
    CRRALL >= splits[1] & CRRALL < splits[2] ~ paste0(splits[1], " to ", splits[2], "%"),
    CRRALL >= splits[2] & CRRALL < splits[3] ~ paste0(splits[2], " to ", splits[3], "%"),
    CRRALL >= splits[3] & CRRALL <= splits[4] ~ paste0(splits[3], " to ", splits[4], "%"),
    CRRALL >= splits[4] & CRRALL <= splits[5] ~ paste0(splits[4], " to ", splits[5], "%"),
    CRRALL > splits[4] ~ paste0("Greater than ", splits[5], "%")))

sr_2020_KC_geo$CRRALL_fctr <- as.factor(sr_2020_KC_geo$CRRALL_fctr)

# 2. 
# Plot 
reponse_map <- sr_2020_KC_geo %>% 
  ggplot(aes(fill = CRRALL_fctr, geometry = geometry))+
  geom_sf(color = "black", size = 0.04) +
  scale_fill_brewer(palette = "PuOr") + 
  ggtitle("          Response Rate by Census Tract - 2020") +
  labs(fill = "Cumulative Self-Response\nRate - Overall (%)",
       caption = "Census tracts shaded in white indicate no self responsedata reported") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) +
  theme_void() + 
  titles_upper()






# ==========================
#  map of % of AA community that is foreign born by tract within LA County
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
                        state = "WA",
                        county = "King County",
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

foreign_born_perc$percent_foreign_fctr <- as.factor(foreign_born_perc$percent_foreign_fctr)

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

ggsave(filename = "../../AAJC Vis/case_studies/king_county_washington/foreign_born_NHPIA.png",
       plot = fborn_map, bg = "white")






# ==========================
#  scatterplot from above with dots colored by citizenship
# ==========================





