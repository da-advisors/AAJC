# # -------
# AA_alone_2000_MAP <- AA_alone_2000 %>% 
#   mutate(percent = 100 * (value/summary_value)) %>%
#   
#   ggplot(aes(fill = percent)) + 
#   geom_sf(color = "black", size = 0.01) +
#   geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
#   theme_AAJC +    # Custom theme for AAJC
#   # scale_fill_distiller(palette = "PuOr", direction = 1, end = .85) +
#   scale_fill_viridis_c(end = .90, direction = -1, option = "magma") +
#   labs(fill = "% of Asian     \npopulation     ",
#        title ="     Percentage of Asian Alone Population",
#        subtitle = "       2000 Decennial Census") +
#   titles_upper()
# 
# ggsave("../AAJC Vis/AA_alone_2000_COUNTY_MAP.png",
#        plot = AA_alone_2000_MAP, bg = "white")
# 
# 
# # MAP 2 - Continuous Color Scale
# #         using aajc colors
# # ------------------------------
# AA_alone_2000_MAP <- AA_alone_2000 %>% 
#   mutate(percent = 100 * (value/summary_value)) %>%
#   
#   ggplot(aes(fill = percent)) + 
#   geom_sf(color = NA) +
#   theme_AAJC +    # Custom theme for AAJC
#   scale_fill_gradient(low = orange_80t, high = purple) + 
#   labs(fill = "% of Asian     \npopulation     ",
#        title ="     Percentage of Asian Alone Population",
#        subtitle = "       2000 Decennial Census") +
#   titles_upper()
# 
# ggsave("../AAJC Vis/AA_alone_2000_COUNTY_MAP_aajc_colors.png",
#        plot = AA_alone_2000_MAP, bg = "white")


# MAPS with discrete color scale (binning): 
# -----------------------------------------

# PERCENTS CALCs
# --------------
AA_alone_2000_percents <- AA_alone_2000 %>% 
  mutate(percent = 100 * (value/summary_value))

summary(AA_alone_2000_percents$percent)

totCounties <- nrow(AA_alone_2000_percents) 

# Number of US counties with AA alone populations = 0% 
zero <- nrow(AA_alone_2000_percents[AA_alone_2000_percents$percent == 0.0, ]) #37
# percent of ALL counties w/ AA alone pops = 0%
zero/totCounties # 0.01149425

# AA Alone or in Combo - 0.008387698
# NHPI - 13.7%

# Number of US counties with AA alone populations < 1% 
less_than_1 <- nrow(AA_alone_2000_percents[AA_alone_2000_percents$percent > 0.0 &
                                             AA_alone_2000_percents$percent < 1.0, ]) # 2666

# percent of ALL counties w/ AA alone pops < 1%
less_than_1/totCounties # 82%

# AA Alone or in Combo - 77%
# NHPI - 99% but NHPI > 0 and < 1 - 86%


# Number of US counties with AA alone populations between 1 - 25%
betw_1_and_25 <- nrow(AA_alone_2000_percents[AA_alone_2000_percents$percent >= 1.0 & 
                                               AA_alone_2000_percents$percent < 25.0 , ])

betw_1_and_25/totCounties # 17%

# AA Alone or in Combo - 22%
# NHPI - 0.002174588

# Number of US counties with AA alone populations between 25 - 50%
# betw_25_and_50 <- nrow(AA_alone_2000_percents[AA_alone_2000_percents$percent >= 25.0 & 
#                                                AA_alone_2000_percents$percent < 50.0 , ])
# 
# betw_25_and_50/totCounties # 0.001863933

# Number of US counties with AA alone populations > 25%
grtr_than_25 <- nrow(AA_alone_2000_percents[AA_alone_2000_percents$percent >= 25.0, ])
# grtr_than_50 <- nrow(AA_alone_2000_percents[AA_alone_2000_percents$percent >= 50.0, ])

grtr_than_25/totCounties # 0.002174588
# AA Alone or in Combo - 0.000621311
# NHPI - 0.0003106555

# Max % of AA Alone pop - Honolulu county 
max(AA_alone_2000_percents$percent)

#                       |      % of counties 
# -----------------------------------------------
# AA pop = 0%           |         0.01149425
# AA pop. < 1%          |         82%
# 1% < AA pop. < 25%    |         16.9%
# AA pop. > 25%         |         0.0022%

# Creating new column for factor data 
AA_alone_2000_percents$percent_fctr <- NA
AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent == 0.0] <- "0%"
AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent > 0.0 &
                                      AA_alone_2000_percents$percent < 1.0] <- "Less than 1%"
AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 1.0 & 
                                      AA_alone_2000_percents$percent < 25.0 ] <- "1 to 25%"
AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 25.0] <- "Greater than 25%"
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 25.0 & 
#                                       AA_alone_2000_percents$percent < 50.0] <- "25 to 50%"
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 50.0] <- "Greater than 50%"


# MAP 3 - based off MAP 2 in AAJC Report
# --------------------------------------
# these colors were taken from the sample report MAP 2
# https://www.advancingjustice-aajc.org/sites/default/files/2019-07/1153_AAJC_Immigration_Final_0.pdf
lt1_color <- "#fffae5"
betw_1_25_color <- "#efb48e"
gt25_color <- "#e2733a"
gt50_color <- "#884523"

AA_alone_2000_MAP_factors <- AA_alone_2000_percents %>%
  
  ggplot(aes(fill = percent_fctr)) + 
  geom_sf(color = "black", size = 0.01) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) + 
  theme_AAJC +    # Custom theme for AAJC
  scale_fill_manual(values = c("Less than 1%" = lt1_color,
                               "1 to 25%" = betw_1_25_color,
                               "Greater than 25%" = gt25_color),
                    na.value = lt1_color) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     Asian Alone Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

ggsave("../AAJC Vis/NHPI_alone_2000_COUNTY_MAP_factored_colors.png",
       plot = AA_alone_2000_MAP_factors, bg = "white")


# MAP 4 - exact match of MAP 2 in AAJC Report
# -------------------------------------------
AA_alone_2000_MAP_factors_exact_match <- AA_alone_2000_percents %>%
  
  ggplot(aes(fill = percent_fctr)) + 
  geom_sf(color = "black", size = 0.01) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) + 
  theme_AAJC +    # Custom theme for AAJC
  scale_fill_manual(values = c("Less than 1%" = lt1_color,
                               "1 to 25%" = betw_1_25_color,
                               "Greater than 25%" = gt25_color),
                    na.value = lt1_color) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     Percentage of Asian Alone Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

ggsave("../AAJC Vis/AA_alone_2000_COUNTY_MAP_factored_colors_exact_match.png",
       plot = AA_alone_2000_MAP_factors_exact_match, bg = "#cbe0f5")


# MAP 5 - exact match with AAJC
#         With 0% data shown in map 
#         (counties with no AA pop. are white)
# --------------------------------------

AA_alone_2000_MAP_factors_exact_match_zero <- AA_alone_2000_percents %>%
  
  ggplot(aes(fill = percent_fctr)) + 
  geom_sf(color = "black", size = 0.01) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) + 
  theme_AAJC +    # Custom theme for AAJC
  scale_fill_manual(values = c("0%" = "white",
                               "Less than 1%" = lt1_color,
                               "1 to 25%" = betw_1_25_color,
                               "Greater than 25%" = gt25_color),
                    na.value = lt1_color) +
  labs(fill = "% of Asian     \npopulation     ",
       title ="     Asian Alone Population",
       subtitle = "       2000 Decennial Census") +
  titles_upper()

ggsave("../AAJC Vis/AA_alone_2000_COUNTY_MAP_factored_colors_exact_match_zeroPercent.png",
       plot = AA_alone_2000_MAP_factors_exact_match_zero, bg = "#cbe0f5")


# MAP 1 - Continuous Color Scale
#         using R builtin colors
# ------------------------------

# head(d2000)
# d2000_MAP_faceted <- d2000 %>%
#   mutate(percent = 100 * (value/summary_value)) %>%
#   
#   ggplot(aes(fill = percent)) +
#   facet_wrap(~variable) + 
#   geom_sf(color = "black", size = 0.01) +
#   geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
#   theme_AAJC +
#   scale_fill_viridis_c(end = .90, direction = -1, option = "magma") +
#   labs(fill = "% of Asian     \npopulation     ",
#        title ="     AA/NHPI United States Population",
#        subtitle = "       2000 Decennial Census") +
#   titles_upper()
# 
# ggsave("../AAJC Vis/AANHPI_2000_COUNTY_MAP_faceted.png",
#        plot = d2000_MAP_faceted, bg = "white")
# 
# 
# # Creating new column for factor data 
# AA_alone_2000_percents$percent_fctr <- NA
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent == 0.0] <- "0%"
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent > 0.0 &
#                                       AA_alone_2000_percents$percent < 1.0] <- "Less than 1%"
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 1.0 & 
#                                       AA_alone_2000_percents$percent < 25.0 ] <- "1 to 25%"
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 25.0] <- "Greater than 25%"
# # AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 25.0 & 
#                                       AA_alone_2000_percents$percent < 50.0] <- "25 to 50%"
# AA_alone_2000_percents$percent_fctr[AA_alone_2000_percents$percent >= 50.0] <- "Greater than 50%"


# # Defining color variables (see AAJC_them.R for info on these colors)
# red <- '#EF6D59'
# orange <- '#E37232'
# orange_80t <- '#f9e3d6'
# orange_10t <- "#e68047"
# orange_50t <- "#f1b999"
# purple <- '#3F3875'

