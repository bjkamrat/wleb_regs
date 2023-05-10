# Author: Brock Kamrath
# Date: 05 May 2023
# Objective: When do extreme events occur?

# Libraries
library(tidyverse)
library(ggplot2)
library(rio)
library(here)

#          
# source functions
source(here("code", "functions", "01_data_processing.R"))

# First, I uploaded the flow-weighted daily concentration data.
raw_df <- import(here("data","processed","HoneySRP.csv"))

# Next, Load in monthly values for circular statistics
mon_stat <- import(here("data","raw","circular_stats_monthly.csv"))

# Exploratory plotting
# distribution of SRP values for each interval of 5 years
raw_df %>%
  mutate(year =  water_year(Date),
         interval = as.numeric(cut(year,6))) %>%
  ggplot()+
  geom_histogram(aes(x = conc)) +
  facet_wrap(~interval)

# create a dataset with intervals and months
df <- raw_df %>%
  mutate(day = yday(Date),
         year =  water_year(Date),
         interval = as.numeric(cut(year,8)),
         month = month(Date))

#exploratory analysis for missing data
# Summarise data by water year
year_count <- raw_df %>%
  mutate(year =  water_year(Date)) %>%
  group_by(year) %>%
  summarise(n = sum(!is.na(conc)),
            mean = round(mean(conc, na.rm = TRUE),3),
            median = round(median(conc, na.rm = TRUE),3),
            max = round(max(conc,na.rm = TRUE),3))

########################################################################
# Conduct a seasonal analysis of monthly DRP concentrations using monthly averages
#############################################################################
# calculate geometric means for each month within each interval
month_int_df <- df %>%
  filter(conc >= 0.001) %>%
  drop_na(conc) %>%
  group_by(interval, month) %>%
  summarise(n = sum(!is.na(conc)),
            mean = geomMean(conc))
######################################################################
#combine the two datasets
month_int_df <- left_join(month_int_df, mon_stat, by = "month")

###############################
# try to plot
##############################
ggplot(month_int_df, aes(month, mean)) +
  geom_line(aes(group = as.factor(interval), color = as.factor(interval))) +
  coord_polar() +
  scale_x_continuous(limits = c(1, 13), breaks=0:12)+
  scale_color_brewer(palette="Spectral")


# write to csv  
#write.csv(month_int_df, file = here("data","processed","monthlySRP.csv"), row.names = FALSE)

# calculate S and C for each interval
month_int_df %>%
  group_by(interval) %>%
  summarise(Pm = sum(mean),
            S = sum(mean*sin),
            C = sum(mean*cosine)) %>%
  mutate(Pr = (S^2 + C^2)^0.5,
         theta = ifelse(S >0 & C>0,atan(S/C),ifelse(C < 0, atan(S/C)+180,atan(S/C)+360)),
         Is = Pr/Pm)

    
# Conclusion: 
# Mean monthly values are not very seasonal in the River
  


##############################################################################
#  Conduct a seasonal analysis of extreme values using monthly counts 
#  of high concentration days (SRP >= 0.1 mg/L)
##############################################################################
# count the number of extreme events each year
# count number of extreme events per month within each interval
df %>%
  filter(conc >= 0.10) %>%
  drop_na(conc) %>%
  group_by(year, month) %>%
  summarise(n = sum(!is.na(conc))) %>%
  filter(year >= 2010) %>%
  mutate(period = ifelse(year >=2015, "post","pre")) %>%
  ggplot(aes(x = month, y = n, color = period))+
  geom_point()+
  geom_smooth(span = 0.25)+
  scale_x_continuous(limits = c(1, 13), breaks=0:12)


# count number of extreme events per month within each interval
ee_count <- df %>%
  filter(conc >= 0.15) %>%
  drop_na(conc) %>%
  group_by(interval, month) %>%
  summarise(n = sum(!is.na(conc)))

# Sum the number of extreme events in each interval
ee_ann_count <- ee_count %>%
  group_by(interval) %>%
  summarise(total = sum(n))

# join the data frames , so that I can calculate the normalized monthly values within each interval.
ee_count <- left_join(ee_count, ee_ann_count,by = "interval")
ee_count <- ee_count %>%
  mutate(norm_n = n/total)

#combine the two datasets
ee_count <- left_join(ee_count, mon_stat, by = "month")

###############################
# try to plot
##############################
ee_count %>% 
  filter(interval >= 4) %>%
ggplot(aes(month, norm_n)) +
  geom_line(aes(group = as.factor(interval), color = as.factor(interval))) +
  scale_x_continuous(limits = c(1, 13), breaks=0:12)+
  scale_color_brewer(palette="Spectral")+
  theme_bw()


# write to csv  
#write.csv(month_int_df, file = here("data","processed","monthlySRP.csv"), row.names = FALSE)

rad2deg <- function(rad) {(rad * 180) / (pi)}
# calculate S and C for each interval
ee_count %>%
  group_by(interval) %>%
  summarise(Pm = sum(norm_n),
            S = sum(norm_n*sin),
            C = sum(norm_n*cosine)) %>%
  mutate(Pr = (S^2 + C^2)^0.5,
         theta = ifelse(S >0 & C>0,rad2deg(atan(S/C)),ifelse(C < 0, rad2deg(atan(S/C))+180,rad2deg(atan(S/C))+360)),
         Is = Pr/Pm)


# Monthly extreme events suggest that the number of extreme events is becoming less seasonal, Mainly due to 
# the appearance of extreme events in May.