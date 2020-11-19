library("tidyverse")
setwd("~/QMSS G5052 Practicum")

mobility_11_17 <- read.csv("2020_US_Region_Mobility_Report_11_17.csv", header = T)

mobility_11_17_ave <- mobility_11_17 %>% 
  subset(country_region_code = "US") %>% 
  group_by(date, sub_region_1) %>%
  summarize(mean(retail_and_recreation_percent_change_from_baseline, na.rm = T),
            mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = T),
            mean(parks_percent_change_from_baseline, na.rm = T),
            mean(transit_stations_percent_change_from_baseline, na.rm = T),
            mean(workplaces_percent_change_from_baseline, na.rm = T),
            mean(residential_percent_change_from_baseline, na.rm = T))

colnames(mobility_11_17_ave) <- c("Date", "Province_State",
                                  "retail_recreation", "grocery", "parks",
                                  "transit", "work", "residential")

us_state_level_clean_2020_11_17 <- read.csv("us_state_level_clean_2020-11-17.csv",
                                            header = T)

US_11_17_final <- merge(us_state_level_clean_2020_11_17, mobility_11_17_ave, 
                        by = c("Province_State", "Date"))
                        
write.csv(US_11_17_final, "US_11_17_final.csv")
