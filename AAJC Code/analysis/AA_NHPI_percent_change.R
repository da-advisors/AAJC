library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
source("AAJC_theme.R")

# Anam's Census API Key: 
census_api_key("0d3f6eaad6d4d9ffb24d6b420e4deccd7fe7f780")

# Import theme created for AAJC Analysis in "AAJC Code/AAJC_theme.R"
theme_AAJC <- readRDS('theme_AAJC.rds')


# [Map] Change in #3 between 2000 and 2010, 2000 and 2020, 2010 and 2020.
#       How would we express this as a map? 
#       should we do these with ACS data since its every year? We'll have > 3 data points
# [Graph / Table] Top 10 change between 2000 and 2010, 2000 and 2020, 2010 and 2020 for numeric and percent changes.



# Percentage change: 
# -----------------
#   • Table with top N states with the highest growth rate 
#       • Can create line charts of these states growth rates
#
#   • Line charts of TOTAL US growth rates 
# -----------------
options(tigris_use_cache = TRUE)

# ------------
# GETTING DATA
# ------------

# function to get data
get_census_data <- function (vars, totalPop, year) {
  get_decennial(
    geography = "state",
    variables = vars,
    resolution = "20m",
    summary_var = totalPop,  # total population in this instance 
    year = year)
}
  
# 2000 + 2010 vars: 
# Asian Alone pop., Asian alone or in combination pop., NHPI pop.
vars_2000 <- c(AA_alone = "P007005",
               AA_alone_combination = "P009005",
               NHPI_alone = "P007006")
totalPop_2000 <- "P007001"

vars_2010 <- c(AA_alone = "P003005",
               AA_alone_combination = "P006005",
               NHPI_alone = "P003006")
totalPop_2010 <- "P003001"

# 2020 vars: 
# Asian Alone pop., NHPI pop.   (could not find AA alone or in combination for 2020)
vars_2020 <- c(AA_alone = "P1_006N",
               NHPI_alone = "P1_007N")
totalPop_2020 <- "P1_001N"

# Possible solution - We can use the ACS 2020 estimates
#   From the ACS 2020 ESTIMATES:
#   ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE GROUPS
vars_2020_acs <- c(AA_alone_combination = "B02011_001")
estimate_totalPop_2020 <- "B01001_001"


# GETTING DATA 
d2000 <- get_census_data(vars_2000, totalPop_2000, 2000)
d2010 <- get_census_data(vars_2010, totalPop_2010, 2010)
d2020 <- get_census_data(vars_2020, totalPop_2020, 2020)


# GETTING AA alone or in combination data from ACS 2020 estimates data 

a2020 <- get_acs(
  geography = "state",
  variables = vars_2020_acs,
  resolution = "20m",
  summary_var = estimate_totalPop_2020,  # total population in this instance 
  year = 2020)


# --------------
# TRANSFORM DATA
# --------------

# 1. Creating a DF for percentage change of entire US 
#   *note that AA alone or in combination for the year 2020 is an ESTIMATE from ACS
prcnt_change_US <- data.frame(
  year = rep(c(2000, 2010, 2020), 3),
  population = rep(c("AA Alone", "AA alone or in Combination", "NHPI Alone"), each = 3),
  tot_pop = c(sum(d2000$value[d2000$variable == "AA_alone"]),
              sum(d2010$value[d2010$variable == "AA_alone"]),
              sum(d2020$value[d2020$variable == "AA_alone"]),
              sum(d2000$value[d2000$variable == "AA_alone_combination"]),
              sum(d2010$value[d2010$variable == "AA_alone_combination"]),
              sum(a2020$estimate),
              sum(d2000$value[d2000$variable == "NHPI_alone"]),
              sum(d2010$value[d2010$variable == "NHPI_alone"]),
              sum(d2020$value[d2020$variable == "NHPI_alone"]))
)

# Adding percent change col
prcnt_change_US <- prcnt_change_US %>%
  mutate(prcnt_change = (tot_pop/lag(tot_pop) - 1) * 100)

# Removing coerced values 
prcnt_change_US$prcnt_change[prcnt_change_US$year == 2000] <- NA


# 2. Creating a DF for percentage change of all states in the US
#   *note that AA alone or in combination for the year 2020 is an ESTIMATE from ACS

d2000$year <- 2000
d2010$year <- 2010
d2020$year <- 2020
a2020$year <- 2020

# drop GEOID column
d2000 <- d2000[-1]
d2010 <- d2010[-1]
d2020 <- d2020[-1]
a2020 <- a2020[-1]
a2020_drop <- c(4,6)
  a2020 <- a2020[-a2020_drop]
  colnames(a2020)[3:4] <- c("value", "summary_value")

prcnt_change_states <- rbind(d2000, d2010, d2020, a2020) %>% 
  arrange(NAME, variable) %>%
  mutate(prcnt_change = (value/lag(value) - 1) * 100) 

prcnt_change_states <- prcnt_change_states %>%
  mutate(numeric_change = (value-lag(value)))

# Removing coerced values 
prcnt_change_states$prcnt_change[prcnt_change_states$year == 2000] <- NA
prcnt_change_states$numeric_change[prcnt_change_states$year == 2000] <- NA

# Calculating percent change from 2000 to 2020
dummyTbl <- prcnt_change_states[prcnt_change_states$year == 2000 | 
                                   prcnt_change_states$year == 2020, ] %>%
  mutate(prcnt_change = (value/lag(value) -1) * 100) %>%
  mutate(numeric_change = (value-lag(value)))



# Isolating percent change from 2000 to 2020
prcnt_change_2000_to_2020 <- dummyTbl$prcnt_change[dummyTbl$year == 2020]
num_change_2000_to_2020 <- dummyTbl$numeric_change[dummyTbl$year == 2020]

# plugging in the above variable into the main prcnt_change_states table 
prcnt_change_states$numeric_change[prcnt_change_states$year == 2000] <- num_change_2000_to_2020

# new column to clarify the prcnt_change column (specifies the period of time)
prcnt_change_period <- c("2000 to 2020", "2000 to 2010", "2010 to 2020")
prcnt_change_period <- rep(prcnt_change_period, 156)
prcnt_change_states$prcnt_change_period <- prcnt_change_period


# 3. Getting top 10 states by percentage change 
top_10_prcnt_chng <- prcnt_change_states %>%
  arrange(desc(prcnt_change)) %>%
  group_by(variable, prcnt_change_period) %>%
  slice(1:10)

write.csv(top_10_prcnt_chng, "top10_percent_change_STATES_by_period.csv")


# 3. Getting top 10 states by numeric change 
top_10_numeric_chng <- prcnt_change_states %>%
  arrange(desc(numeric_change)) %>%
  group_by(variable, prcnt_change_period) %>%
  slice(1:10)

colnames(top_10_numeric_chng)[7] <- "numeric_change_period"
write.csv(top_10_numeric_chng, "top10_numeric_change_STATES_by_period.csv")













# -------------
# PLOTTING DATA
# -------------

# 1. Top 10 states % change (Asian American Alone) for all 3 periods of time
# -------------------------------------------------

lt1_color <- "#fffae5"
betw_1_25_color <- "#efb48e"
gt25_color <- "#e2733a"
gt50_color <- "#884523"
# 2000 - 2010 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2010 , ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change, fill = variable)) + 
  geom_bar(stat="identity") + 
  facet_grid(~variable, scale="free_x", drop=T) +
  scale_fill_manual(values = c(betw_1_25_color, gt25_color,gt50_color)) +
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in Asian American Population by State",
       subtitle = "From 2000 to 2010") 

# 2010 - 2020 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2020 & 
                    top_10_prcnt_chng$variable == "AA_alone", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in Asian American Population by State",
       subtitle = "From 2010 to 2020")

# 2000 - 2020 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2000 & 
                    top_10_prcnt_chng$variable == "AA_alone", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in Asian American Population by State",
       subtitle = "From 2000 to 2020")


# 2. Top 10 states % change (Asian American Alone or in Combination) for all 3 periods of time
# -------------------------------------------------------------------

# 2000 - 2010
top_10_prcnt_chng[top_10_prcnt_chng$year == 2010 & 
                    top_10_prcnt_chng$variable == "AA_alone_combination", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in Asian American (alone or in combination) Population by State",
       subtitle = "From 2000 to 2010")

# 2010 - 2020 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2020 & 
                    top_10_prcnt_chng$variable == "AA_alone_combination", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in Asian American (alone or in combination) Population by State",
       subtitle = "From 2010 to 2020")

# 2000 - 2020 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2000 & 
                    top_10_prcnt_chng$variable == "AA_alone_combination", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in Asian American (alone or in combination) Population by State",
       subtitle = "From 2000 to 2020")


# 3. Top 10 states % change (NHPI) for all 3 periods of time
# ---------------------------------

# 2000 - 2010
top_10_prcnt_chng[top_10_prcnt_chng$year == 2010 & 
                    top_10_prcnt_chng$variable == "NHPI_alone", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in NHPI Population by State",
       subtitle = "From 2000 to 2010")

# 2010 - 2020 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2020 & 
                    top_10_prcnt_chng$variable == "NHPI_alone", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in NHPI Population by State",
       subtitle = "From 2010 to 2020")

# 2000 - 2020 
top_10_prcnt_chng[top_10_prcnt_chng$year == 2000 & 
                    top_10_prcnt_chng$variable == "NHPI_alone", ] %>%
  ggplot(aes(x= reorder(NAME, -prcnt_change), y=prcnt_change)) + 
  geom_bar(stat="identity", fill = "#e2733a") + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=40, hjust=1)) + 
  xlab("State") + 
  ylab("Increase in Population (%)") + 
  labs(title ="Top 10 Changes in NHPI Population by State",
       subtitle = "From 2000 to 2020")
  
options(scipen = 10000)
prcnt_change_US[1:2,] %>%
  ggplot(aes(as.integer(year), tot_pop)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(2000,2010)) +
  ylab("Asian American population") + 
  xlab("Year") + 
  labs(title = "Change in AA alone population", 
       subtitle = "From 2000 to 2010") +
  geom_label(x=2009.5, y=15000000, label = "43.22%", color="darkgreen") +
  ylim(10000000, 15487883)
  

# Vintage Estimates: 
# ----------------
#   • Line graph showing differences between Vintage Estimates of AA/NHPI populations and 
#     actual Census numbers. 
#       • Can pick specific states as well (maybe states with highest % change)

