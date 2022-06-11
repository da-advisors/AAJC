---
title: "Vis prototyping"
output:
  html_document: 
    toc: true
    toc_float: true
    toc_collapsed: true
    theme: yeti
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<br> 

# Asian Alone population
Decennial Census 2000, 2010, 2020

<br> 

## State analysis 

<br> 

### 2010 - Single color
 

![](AA_alone_2000_STATE_MAP_one_color.png)


### 2010 - 2 color scale
![](AA_alone_2010_STATE_MAP_two_color.png)

### 2020
![](AA_alone_2020_STATE_MAP_one_color.png)

## District analysis 
<br> 

### 2000

Using R's built in color palette: 

![](AA_alone_2000_COUNTY_MAP.png)


Messing around with colors picked from AAJC sample report: 

![](AA_alone_2000_COUNTY_MAP_aajc_colors.png)

<u> Maps using discrete scales (binning) </u> 

* There are no counties with % greater than 48 so "Greater than 25%" was used as the highest bin/factor

<br>

![](AA_alone_2000_COUNTY_MAP_factored_colors.png)
<br> 

Exact match to AAJC report: 

![](AA_alone_2000_COUNTY_MAP_factored_colors_exact_match.png)

<br>

Exact match to AAJC report w/ zero percentages ie. showing counties with no asian alone population colored in as white: 

![](AA_alone_2000_COUNTY_MAP_factored_colors_exact_match_zeroPercent.png)



<br> 

# Asian Alone or in Combination

Decennial Census 2000, 2010 

__had trouble finding this variable for the year 2020__ 

<br> 

## State analysis 

### 2000

![](AA_alone_combination_2000_STATE_MAP_one_color.png)

### 2010

![](AA_alone_combination_2010_STATE_MAP_one_color.png)

## District Analysis 

### 2000

![](AA_alone_combination_2000_COUNTY_MAP_factored_colors.png)


<br> 
<br> 

# NHPI Alone

Decennial Census 2000, 2010 


<br> 

## State analysis 

### 2000

![](NHPI_alone_2000_STATE_MAP_one_color.png)

### 2010

![](NHPI_alone_2010_STATE_MAP_one_color.png)

### 2020

![](NHPI_alone_2020_STATE_MAP_one_color.png)


## District Analysis 

### 2000

  * 13% of US counties have 0% NHPI pop. which is a lot higher than AA alone and AA in combination. So, 0% category was included 

![](NHPI_alone_2000_COUNTY_MAP_factored_colors_exact_match_zeroPercent.png)

![](NHPI_alone_2000_COUNTY_MAP_factored_colors.png)


<br>
<br>
Notes: 

* will have to export as svg. To combat pixelation in AAJC's sample report and in my pdf export
* District maps are missing for 2010 & 2020. Can lock down a style first and then just have to plug in years and variable names to produce missing maps
