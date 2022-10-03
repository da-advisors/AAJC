##############################
## READ IN 2000 CENSUS DATA ##
##############################

# d2000 <- load_variables(2000, 'sf1', cache = T)
options(tigris_use_cache = TRUE)

# Get necessary variables for census api
vars_2000 <- c(AA_TOT = "P007005",
               AAC_TOT = "P009005", # Total races tallied!!Asian alone or in combination with one or more other races
               NA_TOT = "P007006",
               NAC_TOT = "P009006") # Total races tallied!!Native Hawaiian and Other Pacific Islander alone or in combination with one or more other races
totalPop_2000 <- "P007001"


# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# get data - function to get data defined above
census2000 <- get_census_data(vars_2000, totalPop_2000, 2000)


# ============================================
# Prep Census Data for merge w/ estimates Data
# ============================================

# remove puerto rico 
census2000 <- census2000[!grepl("Puerto Rico", census2000$NAME), ]

# st_write(census2000, "../Raw Data/census_2000_county.csv", layer_options = "GEOMETRY=AS_WKT")


# splitting up name column into 2 columns - state and county names
census2000 <- census2000 %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

# drop duplicate geometry column
census2000 <- census2000[-2]

# Sum AA_TOT w/ NA_TOT (API_alone)    and      AAC_TOT w/ NAC_TOT (API_combo)
census2000_race_group_sums <- census2000 %>%
  mutate(census_TOT_POP = summary_value,
         CENSUS = value,
         RACE_GROUP = case_when(
           variable == "AA_TOT" ~ "API_alone",
           variable == "NA_TOT" ~ "API_alone",
           variable == "AAC_TOT" ~ "API_combo",
           variable == "NAC_TOT" ~ "API_combo"
         )) %>%
  group_by(STNAME, CTYNAME, RACE_GROUP) %>% 
  summarise(CENSUS = sum(CENSUS))


# Join census2000_race_group_sums with census data
census2000_tidy <- left_join(census2000_race_group_sums, census2000 %>% 
                               select(STNAME, CTYNAME, summary_value, geometry), by = c("STNAME" = "STNAME", "CTYNAME" = "CTYNAME"))

analytical <- census2000_tidy

# Remove all duplicate rows
analytical <- analytical %>% distinct()

# rename column
colnames(analytical)[5] <- "CENSUS_tot_pop"


# =============================
# Prep Estimates Data for merge
# =============================

'%!in%' <- function(x,y)!('%in%'(x,y))

# remove punct everywhere eccept alaska
analytical$CTYNAME[analytical$STNAME != "Alaska"] <- gsub('[[:punct:]]+','',analytical$CTYNAME[analytical$STNAME != "Alaska"])
# fix Louisiana parish counties
estim2000$CTYNAME[estim2000$STNAME == "Louisiana"] <- paste(estim2000$CTYNAME[estim2000$STNAME == "Louisiana"], "Parish", sep = " ")
estim2000$CTYNAME[estim2000$STNAME %!in% c("Louisiana", "Alaska")] <- paste(estim2000$CTYNAME[estim2000$STNAME %!in% c("Louisiana", "Alaska")], "County", sep = " ")
# extract first word from Alaska counties
estim2000$CTYNAME[estim2000$STNAME == "Alaska"] <- word(estim2000$CTYNAME[estim2000$STNAME == "Alaska"], 1)
analytical$CTYNAME[analytical$STNAME == "Alaska"] <- word(analytical$CTYNAME[analytical$STNAME == "Alaska"], 1)
# remove County suffix from VA
estim2000$CTYNAME[estim2000$STNAME == "Virginia" & grepl("City", estim2000$CTYNAME)] <- 
  gsub("\\s*\\w*$", "", estim2000$CTYNAME[estim2000$STNAME == "Virginia" & grepl("City", estim2000$CTYNAME)])
# census counties "City" isnt capitalized
estim2000$CTYNAME[estim2000$STNAME == "Virginia" & grepl("City", estim2000$CTYNAME)] <- str_replace_all(
  estim2000$CTYNAME[estim2000$STNAME == "Virginia" & grepl("City", estim2000$CTYNAME)], "City", "city")


# get geometry
estim2000 <- estim2000 %>%
  mutate(RACE_GROUP = "API_alone",
         ESTIMATE = as.integer(api)) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE) 



# =========================================================
# Merge Estimates Data and Census data into 1 analytical DF
# =========================================================

analytical <- merge(x = estim2000, y = analytical, by = c("CTYNAME", "STNAME", "RACE_GROUP"),
                    all.y = TRUE)


# ==================
# Handling NA Values
# ==================

# there are counties where values for both API_alone & API_combo are NA 
# filter rows by groups where all values are NA
nas <- analytical %>% 
  group_by(STNAME, CTYNAME) %>%
  filter(all(is.na(ESTIMATE)))

table(nas$STNAME)

# Alot of NAs are coming from specific states
# Fixed state problems above - hand coding the remaining 20 or so because of inconsistencies in the data (capitals, punc, etc.)
analytical$ESTIMATE[analytical$CTYNAME == "Charles City County" & analytical$RACE_GROUP == "API_alone"] <- 17
analytical$ESTIMATE[analytical$CTYNAME == "Colonial Heights city" & analytical$RACE_GROUP == "API_alone"] <- 569
analytical$ESTIMATE[analytical$CTYNAME == "James City County" & analytical$RACE_GROUP == "API_alone"] <- 917

analytical$ESTIMATE[analytical$CTYNAME == "DeKalb County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Missouri"] <- 62
analytical$ESTIMATE[analytical$CTYNAME == "St Louis city" & analytical$RACE_GROUP == "API_alone"] <- 4284
analytical$ESTIMATE[analytical$CTYNAME == "Ste Genevieve County" & analytical$RACE_GROUP == "API_alone"] <- 38

analytical$ESTIMATE[analytical$CTYNAME == "DeKalb County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Indiana"] <- 174
analytical$ESTIMATE[analytical$CTYNAME == "LaGrange County" & analytical$RACE_GROUP == "API_alone"] <- 172
analytical$ESTIMATE[analytical$CTYNAME == "LaPorte County" & analytical$RACE_GROUP == "API_alone"] <- 666

analytical$ESTIMATE[analytical$CTYNAME == "DeSoto County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Florida"] <- 193
analytical$ESTIMATE[analytical$CTYNAME == "DeSoto County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Mississippi"] <- 368
analytical$ESTIMATE[analytical$CTYNAME == "MiamiDade County" & analytical$RACE_GROUP == "API_alone"] <- 41138


analytical$ESTIMATE[analytical$CTYNAME == "DeKalb County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Illinois"] <- 2714
analytical$ESTIMATE[analytical$CTYNAME == "DuPage County" & analytical$RACE_GROUP == "API_alone"] <- 66051

analytical$ESTIMATE[analytical$CTYNAME == "DeKalb County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Alabama"] <- 117
analytical$ESTIMATE[analytical$CTYNAME == "DeKalb County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Georgia"] <- 30572
analytical$ESTIMATE[analytical$CTYNAME == "DeKalb County" &
                      analytical$RACE_GROUP == "API_alone" & 
                      analytical$STNAME == "Tennessee"] <- 24

analytical$ESTIMATE[analytical$CTYNAME == "District of Columbia" & analytical$RACE_GROUP == "API_alone"] <- 16983
analytical$ESTIMATE[analytical$CTYNAME == "OBrien County" & analytical$RACE_GROUP == "API_alone"] <- 64
analytical$ESTIMATE[analytical$CTYNAME == "Baltimore city" & analytical$RACE_GROUP == "API_alone"] <- 8327
analytical$ESTIMATE[analytical$CTYNAME == "Carson City" & analytical$RACE_GROUP == "API_alone"] <- 1129
analytical$ESTIMATE[analytical$CTYNAME == "LaMoure County" & analytical$RACE_GROUP == "API_alone"] <- 2

analytical$ESTIMATE[analytical$CTYNAME == "DeWitt County" & analytical$RACE_GROUP == "API_alone"] <- 41



##############################
## NUMERIC & % DIFFERENCES  ##
##############################

analytical <- analytical %>% mutate(
  # dummy column to store ESTIMATE values for subtraction for numeric_diff
  lag_estim = ESTIMATE) %>%
  # filling NA values - ESTIMATE has NAs because there is no API_combo data available 
  fill(lag_estim) %>%
  # get numeric & percent diffs
  mutate(
    # NUMERIC DIFFS
    NUMERIC_DIFF = round(lag_estim - CENSUS, 2),
    PERCENT_DIFF = round(((lag_estim - CENSUS) / ((lag_estim + CENSUS) / 2)) * 100 ,2),
    # comparison column 
    COMPARISON = case_when(
      RACE_GROUP == "API_alone" ~ "PES (alone) - DC (alone)",
      RACE_GROUP == "API_combo" ~ "PES (alone) - DC (combo)"
    )) %>%
  select(STNAME, CTYNAME, RACE_GROUP,ESTIMATE,CENSUS,COMPARISON, NUMERIC_DIFF, PERCENT_DIFF, CENSUS_tot_pop, geometry)

analytical <- analytical %>%
  arrange(STNAME, CTYNAME)

st_write(analytical, "../Transformed Data/PES_DC_comparison_2000.csv", layer_options = "GEOMETRY=AS_WKT")




#########
## 2010
#########

# !!! Commenting out below code because our comparisons are
#     concerned with Modified Race and Estimates !!!

# ##############################
# ## READ IN 2010 CENSUS DATA ##
# ##############################
# 
# # d2010 <- load_variables(2010, 'sf1', cache = T)
# 
# options(tigris_use_cache = TRUE)
# 
# # function to get data
# get_census_data <- function (vars, totalPop, year) {
#   get_decennial(
#     geography = "county",
#     variables = vars,
#     geometry = TRUE,
#     resolution = "20m",
#     summary_var = totalPop,  # total population in this instance 
#     year = year) %>%
#     shift_geometry()
# }
# 
# # 2010 vars: 
# # Asian Alone pop., Asian alone or in combination pop., NHPI pop., NHPI alone or in combo
# vars_2010 <- c(AA_TOT = "P003005",
#                AAC_TOT = "P006005",
#                NA_TOT = "P003006",
#                NAC_TOT = "P006006")
# totalPop_2010 <- "P003001"
# 
# # Anam's Key: 
# census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")
# 
# # get data
# census2010 <- get_census_data(vars_2010, totalPop_2010, 2010)
# 
# # remove puerto rico 
# census2010 <- census2010[!grepl("Puerto Rico", census2010$NAME), ]
# 
# # write.csv(census2010, "../Raw Data/census_2010_county.csv")
# 
# ###################################
# ## NUMERIC & PERCENT DIFFERENCES ##
# ###################################
# 
# # ===============================
# # MELT ESTIM2010 DF - TIDY FORMAT 
# # ===============================
# 
# # make a copy
# estim2010_tidy <- estim2010
# 
# # subset needed cols (totals for now)
# keepCols <- c("STATE", 
#               "COUNTY",
#               "STNAME",
#               "CTYNAME",
#               "TOT_POP",
#               "AA_TOT", 
#               "NA_TOT",
#               "AAC_TOT",
#               "NAC_TOT")
# 
# estim2010_tidy <- estim2010_tidy[, keepCols]
# 
# # melt 
# estim2010_tidy <- estim2010_tidy %>%
#   pivot_longer(cols = 'AA_TOT':'NAC_TOT', names_to = 'variable', values_to = 'value')
# 
# # estim DF and census DF now have the same num of rows  
# 
# # ================
# # Calc Percentages
# # ================
# 
# estim2010_tidy <- estim2010_tidy %>%
#   mutate(value_perc = round((value/TOT_POP) * 100, 2))
# 
# census2010 <- census2010 %>%
#   mutate(value_perc = round((value/summary_value) * 100, 2))
# 
# 
# # ========================================
# # Join ESTIM & CENSUS into 1 analytical DF
# # ========================================
# 
# # splitting up name column into 2 columns - state and county names
# census2010 <- census2010 %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")
# 
# # rearrange DF columns
# estim2010_tidy <- estim2010_tidy %>%
#   mutate(estim_value = value,
#          estim_TOT_POP = TOT_POP) %>% 
#   select(STNAME, CTYNAME, variable, estim_value, estim_TOT_POP, value_perc)
# 
# 
# # drop duplicate geometry column
# census2010 <- census2010[-2]
# 
# # rearrange DF columns
# census2010 <- census2010 %>%
#   mutate(census_TOT_POP = summary_value,
#          census_value = value) %>%
#   select(STNAME, CTYNAME, variable, census_value, census_TOT_POP, value_perc,geometry)
# 
# 
# # merge
# df <- merge(x = estim2010_tidy, y = census2010, by = c("CTYNAME", "STNAME", "variable"),
#            all.y = TRUE)
# 
# analytical <- df %>%
#   # changing col names for better readability 
#   mutate(ESTIMATE = estim_value,
#          CENSUS = census_value,
#          ESTIMATE_PERC = value_perc.x,
#          CENSUS_PERC = value_perc.y) %>%
#   select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, ESTIMATE_PERC, CENSUS_PERC, estim_TOT_POP, census_TOT_POP,geometry)
# 
# 
# # ==================================================
# # Numeric and % differences betw. estimates & census
# # ==================================================
# 
# analytical <- analytical %>%
#   mutate(
#     # estimates were ___ people higher(or lower) than census results
#     NUMERIC_DIFF = round(ESTIMATE - CENSUS, 2),
#     # estimates were __% higher(or lower) than census results
#     PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2),
#     # estimates show that County X would have __% more(or less) AAC pop. than it actually did in census results 
#     PERCENT_OF_COUNTY_DIFF = round(ESTIMATE_PERC - CENSUS_PERC,2)) %>%
#   # reorder columns for readability 
#   select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF,
#          ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, geometry)
#   
# # no calculation for total population difference. It could explain a really high jump in a county if needed
# 
# 
# # ==================
# # Handling 0% values
# # ==================
# 
# # ISSUES: 
# # 1. Cases in which estimates and census both reported 0, should indicate a 0% change but is instead NA
# # 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
# #    a county but the census reported 0 population)
# 
# analytical <- analytical %>% 
#         # If estimate & census both = 0, then PERCENT_DIFF should be 0
#   mutate(PERCENT_DIFF = replace(PERCENT_DIFF, ESTIMATE == 0 & CENSUS == 0, 0),
#          # Code undefined percentages to 0% 
#          PERCENT_DIFF = replace(PERCENT_DIFF, PERCENT_DIFF == Inf, 0),
#          
#          # Creating a flag for undefined percentages (ie. when census value is 0 but estimate value is not)
#          flag = case_when(ESTIMATE > 0 & CENSUS == 0 ~ 1,
#                           # no flag if condition not met 
#                           TRUE ~ 0),
#          flag_desc = case_when(flag == 1 ~ "Undefined % difference (census = 0 & estimate != 0)",
#                                TRUE ~ " ")) %>%
#   select(STNAME, CTYNAME, variable, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF,
#          ESTIMATE_PERC, CENSUS_PERC, PERCENT_OF_COUNTY_DIFF, estim_TOT_POP, census_TOT_POP, flag, flag_desc, geometry)
# 
# 
# # export table to "Transformed Data" folder
# # st_write(analytical, "../Transformed Data/estimates_census_comparison_2010.csv", layer_options = "GEOMETRY=AS_WKT")
# 
# 
# 
# 
# # ==================================
# # Getting summaries of percent diff.
# # ==================================
# 
# 
# # inspecting the PERCENT_DIFF column 
# summary(analytical$PERCENT_DIFF)
# 
# # A list of our race values (AA, AAC, NHPI, NHPIC)
# variables <- unique(analytical$variable)
# 
# # get % summaries for each race group
# for (i in variables){
#   dummy <- analytical %>%
#     filter(variable == i)
#   
#   cat(" --------\n", i, "\n", "--------\n")
#   percent_breakdown(dummy$PERCENT_DIFF)
#   
# }
# 
# 
# # Sample output for NHPI (alone or in combination) pop: 
# 
# # NAC_TOT
# #  Population                  Number of US Counties       Percentage of all US counties
# # 
# #  0%                                   241                           7.67 
# #  Between 1 & 25%                      472                           15.0  
# #  Between 25 & 50%                     415                           13.2  
# #  Between 50 & 75%                     227                           7.22 
# #  Greater than 75%                     671                           21.3  
# #  Less than 0%                         1106                          35.2  
# #  Less than 1%                         11                            0.350
# 


# 2020 Modified Race 
# ================================================================
# Combining all race groups into 4 categories --> AA, AAC, NA, NAC 
# ================================================================

# calculating AAC and NAC 
# AAC = AA + AAC       
# NAC = NA + NAC 

mr_2020 <- mr_2020 %>%
  group_by(STNAME, CTYNAME, race_group) %>%
  summarise(population = sum(MR))

# Create new DF that contains ONLY AIC populations
combo_pop <- mr_2020 %>%
  mutate(race_group = case_when(
    race_group == "AA" ~ "AAC", # if alone, set to AAC (alone + AIC = AIC)
    race_group == "AAC" ~ "AAC",
    race_group == "NA" ~ "NAC",
    race_group == "NAC" ~ "NAC"
  )) %>%
  group_by(STNAME, CTYNAME, race_group) %>%
  summarise(combo_pop = sum(population))


# see all counties with missing race_groups ie. 0 population for a given race group
missing_rg <- anti_join(combo_pop[-4], mr_2020[-4]) %>%
  mutate(population = 0)

# left join combination data with mr data
mr_2020 <- left_join(mr_2020, combo_pop, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'race_group' = 'race_group')) 

# replace combination population values with combo_pop values
mr_2020$population[mr_2020$race_group == "AAC" | mr_2020$race_group == "NAC" ] <- 
  mr_2020$combo_pop[mr_2020$race_group == "AAC" | mr_2020$race_group == "NAC"]

# drop combo_pop column
mr_2020 <- mr_2020[-5]

write.csv(mr_2020, "../Transformed Data/mr_county_2020.csv")


# ===============================================
# Merging modified race data with PES & DC Data
# ===============================================

# read data 
analytical <- read.csv("../Transformed Data/PES_DC_comparison_2020.csv")

# change "variable" column name and values in prep for a left join
colnames(analytical)[4] <- "race_group"
analytical <- analytical %>% mutate(race_group = case_when(
  race_group == "AA_TOT"~ "AA",
  race_group == "AAC_TOT" ~ "AAC",
  race_group == "NA_TOT" ~ "NA",
  race_group == "NAC_TOT" ~ "NAC"
))


analytical <- left_join(analytical, mr_2020, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'race_group' = 'race_group')) %>%
  select(STNAME, CTYNAME, race_group, ESTIMATE, CENSUS, MR = population,NUMERIC_DIFF, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, flag, flag_desc,geometry = WKT)

# Check for NAs due to county naming inconsistencies
dummy <- analytical[is.na(analytical$MR), ] # 4 rows - New mexico dona ana county 

# Entering these 4 missing values 
analytical$MR[analytical$CTYNAME == "Dona Ana County"] <- 
  mr_2020$population[mr_2020$CTYNAME == "Doña Ana County"]


# =========================
# Numeric and % differences
# =========================

analytical <- analytical %>%
  mutate(
    # Modified Race was ___ people higher(or lower) than census results
    NUMERIC_MR_DC = round(MR - CENSUS, 2),
    # Modified Race were __% higher(or lower) than census results
    PERCENT_DIFF_MR_DC = round(((MR - CENSUS) / ((MR + CENSUS) / 2)) * 100 ,2),
    # Estimates were ___ people higher(or lower) than Modified Race results
    NUMERIC_PES_MR = round(ESTIMATE - MR, 2),
    # Estimates were __% higher(or lower) than Modified Raceresults
    PERCENT_DIFF_MR_PES = round(((ESTIMATE - MR) / ((ESTIMATE + MR) / 2)) * 100 ,2)) %>%
  rename(NUMERIC_PES_DC = NUMERIC_DIFF, PERCENT_DIFF_PES_DC = PERCENT_DIFF, RACE_GROUP = race_group)%>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, NUMERIC_PES_DC, PERCENT_DIFF_PES_DC,NUMERIC_MR_DC, PERCENT_DIFF_MR_DC, 
         NUMERIC_PES_MR, PERCENT_DIFF_MR_PES, estim_TOT_POP, census_TOT_POP,flag, flag_desc, geometry)


# ==================
# Handling NA values
# ==================

# ISSUES: 
# 1. Cases in which both populations reported 0, should indicate a 0% change but is instead NA
# 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
#    a county but the census reported 0 population)

analytical <- analytical %>% 
  mutate(
    # If MR & census/estimate both = 0, then PERCENT_DIFF should be 0
    PERCENT_DIFF_MR_DC = replace(PERCENT_DIFF_MR_DC, MR == 0 & CENSUS == 0, 0),
    PERCENT_DIFF_MR_PES = replace(PERCENT_DIFF_MR_PES, MR == 0 & ESTIMATE == 0, 0)) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, NUMERIC_PES_DC, PERCENT_DIFF_PES_DC,NUMERIC_MR_DC, PERCENT_DIFF_MR_DC, 
         NUMERIC_PES_MR, PERCENT_DIFF_MR_PES, estim_TOT_POP, census_TOT_POP,flag, flag_desc,geometry)


# ============================
# Melt Dataframe - tidy format
# ============================
boxplot(analytical$PERCENT_DIFF_PES_DC, analytical$PERCENT_DIFF_MR_DC, analytical$PERCENT_DIFF_MR_PES)

num_diff <- analytical %>% 
  rename(PES_DC = NUMERIC_PES_DC, MR_DC = NUMERIC_MR_DC, PES_MR = NUMERIC_PES_MR) %>%
  pivot_longer(cols = c(PES_DC, MR_DC,PES_MR), 
               names_to = "COMPARISON", values_to = "NUMERIC_DIFF") %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, NUMERIC_DIFF, estim_TOT_POP, census_TOT_POP, geometry)

perc_diff <- analytical %>% 
  rename(PES_DC = PERCENT_DIFF_PES_DC, MR_DC = PERCENT_DIFF_MR_DC, PES_MR = PERCENT_DIFF_MR_PES) %>%
  pivot_longer(cols = c(PES_DC, MR_DC,PES_MR), 
               names_to = "COMPARISON", values_to = "PERCENT_DIFF") %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, PERCENT_DIFF)

analytical <- left_join(num_diff, perc_diff, by = c('STNAME' = 'STNAME', 'CTYNAME' = 'CTYNAME', 'RACE_GROUP' = 'RACE_GROUP',"MR"="MR",
                                                    "ESTIMATE" = "ESTIMATE", "CENSUS" = "CENSUS", "COMPARISON" = "COMPARISON")) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, MR, COMPARISON, NUMERIC_DIFF, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, geometry)


# write to csv
st_write(analytical, "../Transformed Data/PES_DC_MR_comparison_2020.csv", layer_options = "GEOMETRY=AS_WKT")



# 2020 munging code 
##############################
## READ IN 2020 CENSUS DATA ##
##############################

# d2020 <- load_variables(2020, 'pl', cache = T)
# function to get DC data - defined above 

# 2020 vars: 
# Asian Alone pop., NHPI pop.
# For Asian and NHPI combo, we need to get all the race variables with Asian/NHPI + another race and sum them 

# asian variables
vars_2020_a <- c(AA_TOT = "P1_006N",
                 AAC = c("P1_013N",'P1_017N','P1_020N','P1_023N','P1_024N','P1_028N','P1_031N','P1_034N','P1_035N','P1_037N',
                         'P1_040N','P1_041N','P1_043N','P1_044N','P1_046N','P1_048N','P1_051N','P1_052N','P1_054N','P1_055N',
                         'P1_057N','P1_058N','P1_059N','P1_061N','P1_062N','P1_064N','P1_065N','P1_067N','P1_068N','P1_069N',
                         'P1_071N'))

# nhpi variables
vars_2020_nhpi <- c(NHPI_TOT = "P1_007N",
                    NAC = c('P1_014N','P1_018N','P1_021N','P1_023N','P1_025N','P1_029N','P1_032N','P1_034N','P1_036N','P1_038N',
                            'P1_040N','P1_042N','P1_043N','P1_045N','P1_046N','P1_049N','P1_051N','P1_053N','P1_054N','P1_056N',
                            'P1_057N','P1_058N','P1_060N','P1_061N','P1_062N','P1_064N','P1_066N','P1_067N','P1_068N','P1_069N','P1_071N'))

totalPop_2020 <- "P1_001N"

# there is no hispanic variable. only total and not-hispanic. No aggregation needed - will use totals.

# Anam's Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# get data
census2020_a <- get_census_data(vars_2020_a, totalPop_2020, 2020)
census2020_n <- get_census_data(vars_2020_nhpi, totalPop_2020, 2020)

# remove puerto rico 
census2020_a <- census2020_a[!grepl("Puerto Rico", census2020_a$NAME), ]
census2020_n <- census2020_n[!grepl("Puerto Rico", census2020_n$NAME), ]

# pivot_wider so we can sum all of our combination races
census2020_a <- census2020_a %>% pivot_wider(names_from = variable, values_from = value) %>%
  rowwise() %>%
  mutate(AAC_TOT = sum(c_across(AA_TOT:AAC31))) %>%
  select(GEOID, NAME, AA_TOT, AAC_TOT, summary_value)

census2020_n <- census2020_n %>% pivot_wider(names_from = variable, values_from = value) %>%
  rowwise() %>%
  mutate(NAC_TOT = sum(c_across(NHPI_TOT:NAC31))) %>%
  select(GEOID, NAME, NA_TOT = NHPI_TOT, NAC_TOT, summary_value, geometry)

# merge the Asian and NHPI DFs together 
census2020 <- census2020_n %>% left_join(census2020_a, by = c("GEOID", "NAME")) %>%
  select(GEOID, NAME,AA_TOT,AAC_TOT, NA_TOT, NAC_TOT, census_TOT_POP = summary_value.x, geometry)


# ================
# Calc Percentages
# ================

# estim2010_tidy <- estim2010_tidy %>%
#   mutate(value_perc = round((value/TOT_POP) * 100, 2))
# 
# census2020 <- census2020 %>%
#   mutate(value_perc = round((value/summary_value) * 100, 2))


# ========================================
# Join ESTIM & CENSUS into 1 analytical DF
# ========================================

# splitting up name column into 2 columns - state and county names
census2020 <- census2020 %>% extract(NAME, c('CTYNAME', 'STNAME'), "([^,]+), ([^)]+)")

# melting both estimates and census data 
census2020 <- census2020 %>% pivot_longer(cols = "AA_TOT":"NAC_TOT", names_to = "RACE_GROUP", values_to = "CENSUS") %>%
  select(CTYNAME, STNAME, RACE_GROUP, CENSUS, census_TOT_POP,geometry)

estim2020 <- estim2020 %>% pivot_longer(cols = "AA_TOT":"NAC_TOT", names_to = "RACE_GROUP", values_to = "ESTIMATE") %>%
  select(CTYNAME, STNAME, RACE_GROUP, ESTIMATE, TOT_POP)


# merge
analytical <- merge(x = census2020, y = estim2020, by = c("CTYNAME", "STNAME", "RACE_GROUP"),
                    all.y = TRUE) %>%
  select(CTYNAME, STNAME, RACE_GROUP, ESTIMATE, CENSUS, estim_TOT_POP = TOT_POP, census_TOT_POP, geometry)


# Doña Ana county,New Mexico error due to special character 
analytical$CENSUS[is.na(analytical$CENSUS)] <- c(2700, 4203, 195, 502)
analytical$CTYNAME[is.na(analytical$census_TOT_POP)] <- rep("Dona Ana County", 4)
analytical$census_TOT_POP[is.na(analytical$census_TOT_POP)] <- rep(219561, 4)


# ==================================================
# Numeric and % differences betw. estimates & census
# ==================================================

analytical <- analytical %>%
  mutate(
    # estimates were ___ people higher(or lower) than census results
    NUMERIC_DIFF = round(ESTIMATE - CENSUS, 2),
    # estimates were __% higher(or lower) than census results
    PERCENT_DIFF = round(((ESTIMATE - CENSUS) / ((ESTIMATE + CENSUS) / 2)) * 100 ,2)) %>%
  # reorder columns for readability 
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF, estim_TOT_POP, census_TOT_POP, geometry)

# no calculation for total population difference. It could explain a really high jump in a county if needed


# ==================
# Handling 0% values
# ==================

# ISSUES: 
# 1. Cases in which estimates and census both reported 0, should indicate a 0% change but is instead NA
# 2. There are undefined percentages (ie. cases where there were non-zero estimated population values for
#    a county but the census reported 0 population)

analytical <- analytical %>% 
  # If estimate & census both = 0, then PERCENT_DIFF should be 0
  mutate(PERCENT_DIFF = replace(PERCENT_DIFF, ESTIMATE == 0 & CENSUS == 0, 0),
         # Code undefined percentages to 0% 
         PERCENT_DIFF = replace(PERCENT_DIFF, PERCENT_DIFF == Inf, 0),
         
         # Creating a flag for undefined percentages (ie. when census value is 0 but estimate value is not)
         flag = case_when(ESTIMATE > 0 & CENSUS == 0 ~ 1,
                          # no flag if condition not met 
                          TRUE ~ 0),
         flag_desc = case_when(flag == 1 ~ "Undefined % difference (census = 0 & estimate != 0)",
                               TRUE ~ " ")) %>%
  select(STNAME, CTYNAME, RACE_GROUP, ESTIMATE, CENSUS, NUMERIC_DIFF, PERCENT_DIFF,estim_TOT_POP, census_TOT_POP,
         flag, flag_desc, geometry)

analytical <- analytical %>%
  arrange(STNAME, CTYNAME)

st_write(analytical, "../Transformed Data/PES_DC_comparison_2020.csv", layer_options = "GEOMETRY=AS_WKT")



