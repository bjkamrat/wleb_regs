
# Read in packages
require("ContDataQC")
library(tidyverse)
library(ggplot2)
library(here)
library(rio)


# read in flow data 
data <- import(here("data","processed","HoneyQ.csv"))

# add a water year column to the flow data
data <- data %>%
  mutate(year = water_year(date))

# Create a vector of the unique water years (i.e., all water years)
years <- unique(data$year)

# Create a blank list output 
output <- list()

# For loop to clip the data data frame into single water years then calculate the Richards
# - Baker index for that year
for(i in 1:length(years)){
  wy <-  years[i]
  
  inputs <- data %>%
    filter(data$year == wy)
  
  RBI <- RBIcalc(inputs$Qdaily)
  
  output[[i]] <- data.frame(wy,RBI)
}

# convert the output list to a data frame so that it can be summarized and plotted
df <-  as.data.frame(do.call(rbind, output))

# summarize the output data
summary(df)

# plot the output data
ggplot(data = df)+
  geom_point(aes(x = wy, y = RBI))+
  lims(y = c(0,1))+
  theme_classic()

# the results show that honey creek has been within the upper middle quartile for its watershed area.
# with a range of values from 0.41 to 0.57 and a mean value of 0.49.