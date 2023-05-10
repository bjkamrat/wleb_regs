ncwqr_wide <- get_data(here("data","raw","2023_05_02_Raw-Honey-Wide.csv"))
ncwqr <- raw_long(ncwqr_wide)

yearly <- ncwqr %>%
  filter(pol == "SRP") %>%
  mutate(Year = water_year(as.Date(Date, format = "%m/%d/%Y")),
         flux_kgd = flux_kgyr/365.25) %>%
  group_by(Year) %>%
  summarise(flux = sum(flux_kgd)/10^6)

yearly_wrtds <- yearly_wrtds %>%
  rename("flux_kg" = "Flux [10^6kg/yr]")


joined_data <- left_join(yearly,yearly_wrtds,by="Year")


joined_data %>%
  ggplot()+
  geom_point(aes(x = flux_kg, y = flux))+
  geom_abline(slope = 1, yintercept = 0)
