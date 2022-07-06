## About Dataset

This dataset contains numeric and percent differences between: 

  * 2010 Population Estimates data
  
  * 2010 Decennial Census Population data 
  
  * 2010 Modified Race Data 
  

## Description of the data

| Column | Description | 
| ------- | ----------- | 
| `STNAME` | State name | 
| `CTYNAME` | County name | 
| `RACE_GROUP` | Race category. There are only 4 possible values <br><br> `AA_TOT` : Asian American (alone) <br> `AAC_TOT` : Asian American (alone or in combination) <br> `NA_TOT` : Native Hawaiian and Pacific Islander (alone) <br> `NAC_TOT` : Native Hawaiian and Pacific Islander (alone or in combination) |
| `ESTIMATE` | Estimated population per county (num residents) | 
| `CENSUS` | Census population per county (num residents) | 
| `MOD_RACE` | Modified Race population per county (num residents) | 
| `COMPARISON` | Specifies the data sources from which populations are being compared <br><br> `PES_DC` : Estimates vs Census Data <br> `PES_MR`: Estimates vs Modified Race Data <br> `MR_DC` : Modified Race vs Estimates Data   | 
| `NUMERIC_DIFF` | The numeric difference between the population data specified in `COMPARISON`. Calculated as: <br><br> `ESTIMATE` - `CENSUS` <br><br> *estimates were ___ residents higher(or lower) than census results for County X* | 
| `PERCENT_DIFF` | The percent difference between the population data specified in `COMPARISON`. Calculated as: <br><br> ((`ESTIMATE` - `CENSUS`) / (`ESTIMATE` + `CENSUS`)/2) * 100 <br><br> *estimates were ___ \% higher(or lower) than census results for County X* |
| `ESTIMATE_PERC` | Estimated percent of county population. Calculated as: <br><br> `ESTIMATE`/`estim_TOT_POP` <br><br>| 
| `CENSUS_PERC` |  Census percent of county population. Calculated as: <br><br> `CENSUS`/`census_TOT_POP` <br><br> | 
| `PERCENT_OF_COUNTY_DIFF` | The difference between estimated and census percent of county population. Calculates as: <br><br> `ESTIMATE_PERC` - `CENSUS_PERC` <br><br> *estimates show that County X was expected to have ___ \% more(or less) AAC pop. than it actually did in census results* | 
| `estim_TOT_POP` | Total Population of a county from estimates | 
| `census_TOT_POP` | Total Population of a county from census | 
| `flag` | A non-zero value indicates a flag. As per Chris email - flag to see how many times we have an issue | 
| `flag_desc` | Description of flag. <br> 1: There are undefined percentages (ie. cases where there were non-zero **estimated** population values reported for a county but the **census** reported 0 population. | 
| `geometry` | geospatial data for mapping | 




###  Raw Data Links

* [DataRepository](https://www2.census.gov/programs-surveys/popest/datasets/2010/2010-eval-estimates/cc-est2010-alldata.csv) - Link to the Estimates data.

* [DataRepository](https://github.com/da-advisors/AAJC/blob/main/Raw%20Data/.estimates_2010_county.csv.icloud) - Link to the Census data.

* [DataRepository](https://www.census.gov/programs-surveys/popest/technical-documentation/research/modified-race-data.html) - Link to the Modfied Race data.



