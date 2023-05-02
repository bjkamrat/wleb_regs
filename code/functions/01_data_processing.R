# R/functions

get_data <- function(file){
  read_csv(file)
}

raw_long <- function(file){
  %>%
    rename("Date" = "INT.date") %>%
    select(Date, SRP, TP, TSS, NO23) %>%
    pivot_longer(!Date,names_to = "pol",values_to = "flux_kgd" )
}
df <- 

write.csv(df,file = here("raw_data", "2023_05_02_Raw_Honey_Wide.csv"))

get_data(here("raw_data","2023_05_02_Raw_Honey_Wide.csv"))

process_data <- function(){
  
}