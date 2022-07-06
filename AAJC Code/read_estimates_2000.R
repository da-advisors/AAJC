library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tigris)
library(plotly)


# Link to 2000s estimates: https://www2.census.gov/programs-surveys/popest/datasets/1990-2000/counties/asrh/co-99-10.zip

# Population Estimates for Counties:
# Race By Hispanic Origin Annual Time Series
# July 1, 1990 to July 1, 1999
    # Column names were removed from data file in order to read it in 

# Good article for Pacific Islander information: 
    # https://www.census.gov/library/publications/2001/dec/c2kbr01-14.html#:~:text=The%20term%20%E2%80%9CNative%20Hawaiian%20and,differ%20in%20language%20and%20culture.


# ================
# Reading the Data
# ================

# Read Etstimates Data 
dat <- read.table("../Raw Data/county_estimates_1990_1999.txt")

# Read FIPS code data 
fips <- read.csv("../Raw Data/county_FIPS_codes.csv")

# column names 
    # ai/an - American Indian and Alaska Native
    # api - Asian and Pacific Islander
    # nh - Non-Hispanic

colnames <- c("year", "FIPS", "white_nh", "black_nh", "ai/an_nh", "api_nh", "white", "black", "ai/an", "api")

colnames(dat) <- colnames


# ==============
# Preparing Data
# ==============


# Adding county and st names using FIPS
# -------------------------------------

# merge on FIPS Code
dat <- left_join(dat, fips, by = "FIPS") %>%
  select(year, CTYNAME, STNAME, "white_nh", "black_nh", "ai/an_nh", "api_nh", "white", "black", "ai/an", "api")


# Aggregating all non-hispanic and hispanic asian columns together
# ----------------------------------------------------------------

# dropping these race columns - white, black, ai/an
# new DF for only Asian & Pacific Islander data 

dat_api <- dat %>%
  mutate(api = api + api_nh) %>%
  select(year, CTYNAME, STNAME, api)

# Kalawao County did not get read in from FIPS - manually fixing NAs
    # FIPS Code - 15005
dat_api$CTYNAME[is.na(dat_api$CTYNAME)] <- "Kalawao"
dat_api$STNAME[is.na(dat_api$STNAME)] <- "Hawaii"


# Explore data for linear interpolation - linear regression model 
# ---------------------------------------------------------------
dat %>% 
  group_by(year) %>%
  summarise(total = sum(api_nh)) %>%
  ggplot(aes(x=as.factor(year), y=total))+ 
  geom_point() +
  ggtitle("Total US API (non-hispanic) population by year") +
  xlab("Year") + 
  ylab("population")

dat_api %>% 
  group_by(year) %>%
  summarise(total = sum(api)) %>%
  ggplot(aes(x=as.factor(year), y=total))+ 
  geom_point() +
  ggtitle("Total US API population by year") +
  xlab("Year") + 
  ylab("population")


# Creating a new 2000s DF 
# -----------------------

# create 1 row for the year 2000 for every county
missing_2000 <- dat_api %>%
  filter(year == 1999) %>%
  mutate(year = 2000, api = NA) %>%
  select(year, CTYNAME, STNAME, api)



# ====================
# Linear Interpolation 
# ====================

# https://stackoverflow.com/questions/48563436/linear-interpolation-in-time-series-in-r
# https://www.statology.org/linear-interpolation-in-r/

# checking data types
str(dat_api)


# Interpolation Loop 
# ------------------

# for each State
for (st in unique(dat_api$STNAME)) {
  
  # create a dummy DF that contains ONLY that state's population data
  state <- dat_api %>%
    filter(STNAME == st)
  
  # and for each county within that state
  for (cty in unique(state$CTYNAME)){
    
    # fit a linear regression model based off of that counties api values 
    model <- lm(api ~ year, data = dat_api %>%
                  filter(STNAME == st, CTYNAME == cty),
                na.action = na.omit)
    
    # make prediction for that county's 2000 population
    interp_val <- predict(model, newdata = missing_2000 %>%
                      filter(STNAME == st & CTYNAME == cty))
    
    # add our interpolated values back into missing_2000s DF
    missing_2000$api[missing_2000$STNAME == st & missing_2000$CTYNAME == cty] <- round(interp_val, 2)
  }
}


# checking different counties
test <- dat_api %>%
  filter(STNAME == "Florida", CTYNAME == "Broward")
plot(test$year, test$api, xlim = c(1990,2000), ylim = c(16000, 36000))
points(missing_2000[323,1], missing_2000[323,4], col = "red")


# ================================================================
# Merge Interpolated Population Values with 1990-1999 estimates DF 
# ================================================================

# combining interp vals (missing_2000 DF) w/ estimates DF (dat_api)
analytical <- rbind(dat_api, missing_2000) %>%
  group_by(STNAME,CTYNAME,year)

# # write to csv to fix grouping error 
# write.csv(analytical,"../Transformed Data/county_estimates_1990_to_2000.csv", row.names = F)
# # read csv back in after fixed
# analytical <- read.csv("../Transformed Data/county_estimates_1990_to_2000.csv")


# Remove the 90s
analytical <- analytical %>%
  filter(year == 2000)

# save 2000s ONLY data into Transformed Data folder   
write.csv(analytical,"../Transformed Data/county_estimates_2000_interpolated.csv", row.names = F)
  