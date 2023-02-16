library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(showtext)
library(stringr)

## THEME IDEAS ## 
## Based off examples from: 
## https://www.advancingjustice-aajc.org/sites/default/files/2019-07/1153_AAJC_Immigration_Final_0.pdf

# TITLE FONT - Open Sans bold
#     color - black
# SUBTITLE FONT - Open Sans bold
#     color - black
# Y-AXIS FONT - ie. the countries
#     color - gray

# AAJC Colors: 
# -----------
# Color picked from sample report pdf using a color picker: 
# • Red - #EF6D59

# • Orange - #E37232
#     • 80% tint --> #f9e3d6 (MOST READABLE)

# • Purple - #3F3875
#     might want to get rid of the red. Makes the map pretty unclear or keep just one
#     color like orange for a state by state analysis 

red <- '#EF6D59'
orange <- '#E37232'
orange_80t <- '#f9e3d6'
purple <- '#3F3875'


# Importing the font: 
# font_add(family = "Open Sans", "OpenSans-Bold.ttf") 
showtext_auto()


# CREATING THEME 
# --------------
#   building off theme_void() - its almost exactly what we need (i think) 

theme_aajc <- theme_void() +
  theme(
    # Similar font to AAJC report &
    # positioning Title, subtitle, and Legend away from map for Alysha
    plot.title = element_text(face = "bold",
                              vjust = 5,
                              size=15),
    plot.subtitle = element_text(vjust = 7),
    
    # making legend text match with report example 
    legend.title = element_text(face = "bold",size = 8),
    
    # increasing space between legend and map 
    legend.margin = margin(10,10,10,10),
    legend.box.margin = margin(10,10,10,10))

# Save theme as RDS for future use 
theme_aajc %>% saveRDS('theme_AAJC.rds')

# Remove and re-add saved theme (sanity check)
rm(theme_aajc)
theme_AAJC <- readRDS('theme_AAJC.rds')




# Function to uppercase title and subtitles on ggplots
#   could not find a way to bake this into the custom theme 
#   Will have to include source("AAJC_theme.R") with library imports 

ggplot_add.my_theme <- function(object, plot, object_name){
  # plot$theme <- ggplot2:::update_theme(plot$theme, object)
  plot$labels$title <- stringr::str_to_upper(plot$labels$title)
  plot$labels$subtitle <- stringr::str_to_upper(plot$labels$subtitle)
  plot
}

titles_upper <- function(){
  out <- theme_AAJC
  
  class(out) <- c("my_theme", class(out))
  
  return(out)
}


# Theme trial & error   
# AA_alone_2000_MAP <- AA_alone %>% 
#   mutate(percent = 100 * (value/summary_value)) %>%
#   ggplot(aes(fill = percent)) + 
#   geom_sf(color = "white") +
#   theme_AAJC + 
#   scale_fill_gradient(low = orange_80t, high = orange) + 
#   labs(fill = "% of Asian     \npopulation     ",
#        title = str_to_upper(
#          "     Percentage of Asian Alone Population"
#        ),
#        subtitle =  str_to_upper("       2000 Decennial Census"))
# 
# 
# AA_alone_2000_MAP


# AAJC Color Fill: 

# Me testing some stuff : 

# Only orange gradient (better for state by state analysis)
# AA_alone_2000_MAP + 
#   scale_fill_gradient(low = "#fcf1eb", high = "#E37232") + 
#   theme(
#     plot.title = element_text(family = "Open Sans", face = "bold", size=15)
#   )     # most readable
# 
# # Orange to purple 
# AA_alone_2000_MAP + 
#   scale_fill_gradientn(colours = c("#e68047", "#524c83"))


