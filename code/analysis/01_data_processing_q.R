library(rio)
library(here)
library(tidyverse)

df <- import(here("data","raw","2023_05_02_Raw-Honey-Q.txt"))

df <- df %>%
  mutate(Date = as.Date(datetime, format = "%Y-%m-%d")) %>%
  rename("q_cfs" = "00060") %>%
  select(Date,q_cfs,qual)

write.csv(df, file = here("data","raw","2023_05_02_Raw-Honey-Q.csv"), row.names = FALSE)
  
