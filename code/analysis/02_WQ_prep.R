# Author: Brock Kamrath
# Date: 05/03/2023
# Objective: Prep WQ data for WRTDS analysis

# read in library
library(tidyverse)
library(readxl)
library(ggplot2)
library(here)
library(knitr)
library(rio)

source(here("code","functions","01_data_processing.R"))

# read in concentration data
raw_df_conc <- read_excel(here("data","raw","2023_05_03_Raw-Honey-Conc.xlsx"))

# count the number of samples per year for each pollutant
count_samp <- raw_df_conc %>%
  select(DateTime,tss,tp,srp,no3) %>%
  mutate(Date = as.Date(DateTime),
         wy = water_year(Date)) %>%
  filter(wy >= 1981 & wy <= 2021) %>%
  select(!DateTime) %>%
  gather(key = "pol", value = "conc", -Date,-wy) %>%
  group_by(wy, pol) %>%
  summarise(n = sum(!is.na(conc)))

write.csv(count_samp, file = here("data","processed","EDA","10_05_2023-annual_count_all_samples.csv"))

#### Prep Concentration
# select SRP values and clip water years to 1992 to 2021
pollutants <- c("srp","no3","tp","tss")
output <- list()

for(i in 1:length(pollutants)){
  df <- raw_df_conc %>%
    select(DateTime, q_cfs, !!pollutants[i]) %>%
    mutate(Date = as.Date(DateTime),
           wy = water_year(Date)) %>%
    filter(wy >= 1981 & wy <= 2021) %>%
    rename("conc_mgL" = pollutants[i])
  
  # convert to daily flow-weighted average concentration
  output[[i]] <- calc_daily_fwmc(df)
  
  #write to csv the daily flow-weighted avearges
  write.csv(output[[i]], here("data","processed",paste("Honey_",pollutants[i],".csv",sep ="")), row.names = FALSE)
}

# add NA values to include all days in period
#create a dataset of all dates in between values.
 val <- nrow(output[[1]])
 days <- data.frame(Date = seq(output[[1]]$Date[1], output[[1]]$Date[val], "day"))
 wq_data <- left_join(days,output[[1]], by = c("Date"))

write.csv(wq_data, here("data","processed","HoneySRP_wNA.csv"), row.names = FALSE)


