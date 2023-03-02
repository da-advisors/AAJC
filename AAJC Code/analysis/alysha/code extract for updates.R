#########################################################
#### CODE EXTRACT FOR UPDATES IN CASE STUDY ANALYSES ####
#########################################################

#### UPDATED CODE EXTRACTS FOR CHARTS WITHOUT GRID LINES ####

# updated charts w/o gridlines --> change from theme(minimal) to:
## AIC ** MAKE SURE THAT UPDATED CHART FOR "ALONE" HAS BEEN ADDED **

scatter_response2 <- sr_2020_LA %>% filter(RACE == 'NHPI_A') %>%
#  ggplot(aes(x = pop_percentage, y = CRRALL, size = total_tract_pop)) + 
#  geom_point(color = "#e49d48", alpha = 0.7) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey")) + 
#  xlab("NHPI (Alone or in Combination) Population (%)") + 
#  ylab("Cumulative Self-Response\nRate - Overall (%)") + 
#  ggtitle("Response Rate by Percentage of NHPI Population by Census Tract - 2020")

# add "_2" at end of filename
ggsave(filename = "././AAJC Vis/case_studies/los_angeles/resp_by_tract_pop_scatter_NHPI_AIC_2020_SIZE_2.png",
       plot = scatter_response2, bg = "white", width =9.07, height = 5.47)



#### UPDATED CODE EXTRACTS HAWAII-- CHANGE LINES AND COLOR ####
### MAKE LINE CHANGES TO NYC (INVERSE DASHES AND SOLIDS)

#v2_line <- agegrp_2010_HI_USA %>% filter(RACE == "NHPI_AIC") %>%
#  ggplot(aes(x =as.factor(AGEGRP), y=PERC_DIFF, group = CTYNAME, linetype = CTYNAME)) +
#  geom_hline(yintercept = 0, linetype='dotted', col='grey')+
#  geom_line(aes(color=CTYNAME), size=.7, alpha=.7) +
  scale_linetype_manual(values = c("dashed",  "solid",  "solid",
                                   "solid", "solid",
                                   "dashed"),
                        name = "Region") +
  scale_color_manual(values = c("#CC5500", "#E69C0C", "#916a92", "#780116","#008148", "#0F1108"), name = "Region") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "grey")) + 
#  xlab("Age Group") + 
#  ylab("Error of Closure (%)") + 
#  ggtitle("Coverage by Age Group for NHPI (Alone or in Combination) Populations - 2010")+
#  scale_x_discrete(labels = agegrp_labels)

# change age group labels and add "2" for v2
v2_line2 <- v2_line2 + theme(axis.text.x = element_text(angle=45)) 

# add "2" to plot and filename
ggsave(filename = "../../AAJC Vis/case_studies/hawaii/US_AND_HI_line_graph_coverage_by_agegrp_NHPI_AIC_2010_2.png",
       plot = v2_line2, bg = "white", width =10.0, height = 5.47)




## UPDATED CODE EXTRACTS FOR SUBETHNICITY DATA ##
## updatED subethnicity data to create percentages of total &
## merge into one dataframe & export to folder ##

###
### aa -- add before "remove total nhpi population count"

# keep top subethnicities and arrange in descending order (for v2)
subethnicity_aa_20_2 <- subethnicity_aa_20 %>% top_n(11, wt=estimate) %>% arrange(desc(estimate))
subethnicity_aa_20_NATIONAL_2 <- subethnicity_aa_20_NATIONAL %>% arrange(desc(estimate))

# save total estimates count
subethnicity_aa_20_total <- subethnicity_aa_20_2[grep("Total",subethnicity_aa_20$label), ]
subethnicity_aa_20_total <- subethnicity_aa_20_total$estimate

subethnicity_aa_20_NATIONAL_total <- subethnicity_aa_20_NATIONAL_2[grep("Total",subethnicity_aa_20_NATIONAL$label), ]
subethnicity_aa_20_NATIONAL_total <- subethnicity_aa_20_NATIONAL_total$estimate

# add total estimates as column
subethnicity_aa_20_2$total_asn_pop <- c(subethnicity_aa_20_total)

subethnicity_aa_20_NATIONAL_2$total_asn_pop <- c(subethnicity_aa_20_NATIONAL_total)

# add percentage of total asian population column
subethnicity_aa_20_2 <- subethnicity_aa_20_2 %>% mutate(percent_region=estimate/total_asn_pop,
                                                        percent_region=percent_region*100)
subethnicity_aa_20_NATIONAL_2 <- subethnicity_aa_20_NATIONAL_2 %>% mutate(percent_us=estimate/total_asn_pop,
                                                                          percent_us=percent_us*100)
# merge data into 1 df
subethnicity_aa_full <- merge(
  subethnicity_aa_20_2, subethnicity_aa_20_NATIONAL_2, by="label")

# Save for Alysha
write.csv(subethnicity_aa_full, "././Transformed Data/data for viz_alysha/case_studies/aa_subethnicities_losangeles.csv")


###
### nhpi

# aggregate all counties (if needed), limit to top 6 subethnicities, descending order for national (for v2)
subethnicity_nhpi_20_2 <- subethnicity_nhpi_20 %>% group_by(label) %>% summarise(estimate = sum(estimate))

subethnicity_nhpi_20_2 <- subethnicity_nhpi_20 %>% top_n(6, wt=estimate) %>% arrange(desc(estimate))
subethnicity_nhpi_20_NATIONAL_2 <- subethnicity_nhpi_20_NATIONAL %>% arrange(desc(estimate))

# save total estimates count
subethnicity_nhpi_20_total <- subethnicity_nhpi_20_2[grep("Total",subethnicity_nhpi_20$label), ]
subethnicity_nhpi_20_total <- subethnicity_nhpi_20_total$estimate

subethnicity_nhpi_20_NATIONAL_total <- subethnicity_nhpi_20_NATIONAL_2[grep("Total",subethnicity_nhpi_20_NATIONAL$label), ]
subethnicity_nhpi_20_NATIONAL_total <- subethnicity_nhpi_20_NATIONAL_total$estimate

# add total estimates as column
subethnicity_nhpi_20_2$total_asn_pop <- c(subethnicity_nhpi_20_total)

subethnicity_nhpi_20_NATIONAL_2$total_asn_pop <- c(subethnicity_nhpi_20_NATIONAL_total)

# add percentage of total asian population column
subethnicity_nhpi_20_2 <- subethnicity_nhpi_20_2 %>% mutate(percent_region=estimate/total_asn_pop,
                                                            percent_region=percent_region*100)
subethnicity_nhpi_20_NATIONAL_2 <- subethnicity_nhpi_20_NATIONAL_2 %>% mutate(percent_us=estimate/total_asn_pop,
                                                                          percent_us=percent_us*100)

# merge data into 1 df
subethnicity_nhpi_full <- merge(
  subethnicity_nhpi_20_2, subethnicity_nhpi_20_NATIONAL_2, by="label")

# Save for Alysha 
write.csv(subethnicity_nhpi_full, "././Transformed Data/data for viz_alysha/case_studies/nhpi_subethnicities_hawaii.csv")

