library(tidyverse)
library(ggplot2)
library(sf)

shp_boroughs <- read_sf("data/2_auxilliary_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
#shp_wards <- read_sf("data/2_auxilliary_data/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")

ggplot() + geom_sf(data=shp_boroughs$geometry)

# get London characteristics incl location
dta_chracteristics_london <- read_csv("/Users/wolfgang/Projects/high_street_analysis/data/3_cleaned/dta_chracteristics_uk.csv") %>%
  filter(city == "london") %>%
  filter(county == "Greater London")

# get status data
dta_jan <- read_csv("data/3_cleaned/dta_jan.csv") %>%
  filter(city == "london")

# make wide
#dta_jan_wide <- dta_jan %>%
#  select(id,month_year,status) %>%
#  pivot_wider(id_cols=id,names_from=month_year,values_from=status,names_prefix="status_")

# merge to characteristics
#dta_chracteristics_london2 <- dta_chracteristics_london %>%
#  inner_join(dta_jan_wide)

# London boroughs
dta_locations <- st_as_sf(dta_chracteristics_london %>% 
                            select(id,longitude,latitude) %>% 
                            filter(!is.na(longitude)),
                          coords = c("longitude", "latitude"), crs = 4326)

dta_locations <- st_transform(dta_locations, crs=st_crs(shp_boroughs))

dta_locations <- dta_locations %>%
  mutate(intersection = as.integer(st_intersects(geometry, shp_boroughs$geometry))) %>%
  mutate(borough = if_else(is.na(intersection), '', shp_boroughs$NAME[intersection])) %>%
  select(-intersection)

dta_boroughs <- dta_jan %>%
  inner_join(as.data.frame(dta_locations) %>% select(id,borough)) %>%
  filter(borough != "") %>%
  group_by(month_year,master_category,borough) %>%
  summarise(total = n(),
            usual_status = mean(status == "usual_status"),
            temporarily_closed = mean(status == "temporarily_closed"),
            permanently_closed = mean(status == "permanently_closed"))

# save data for app
write_csv(dta_boroughs,"high-street-app-maps/london_boroughs.csv")


# merge to shape file
shp_boroughs2 <- shp_boroughs %>%
  left_join(dta_boroughs, by=c("NAME" = "borough"))

# save data for app
#write_csv(shp_boroughs2,"high-street-app-maps/london_boroughs.csv")

# make maps
london_map <- ggplot() +
  geom_sf(data=shp_boroughs2 %>% filter(master_category == "restaurants", month_year=="01_2021"),
          aes(colour = temporarily_closed,fill = temporarily_closed)) +
  scale_fill_gradient(low="#66a182",high="#F0E442") +
  scale_colour_gradient(low="#66a182",high="#F0E442") +
  ggtitle("Share of Temporarily Closed Restaurants \n by London Boroughs Jan 2021")
print(london_map)

png("output/london_boroughs_restaurants_temporarily_closed.png", width = 20, height=20, units="cm", res=400)
print(london_map)
dev.off()

london_map <- ggplot() +
  geom_sf(data=shp_boroughs2 %>% filter(master_category == "restaurants", month_year=="01_2021"),
          aes(colour = permanently_closed,fill = permanently_closed)) +
  scale_fill_gradient(low="#66a182",high="#d1495b") +
  scale_colour_gradient(low="#66a182",high="#d1495b") +
  ggtitle("Share of Permanently Closed Restaurants \n by London Boroughs Jan 2021")
print(london_map)

png("output/london_boroughs_restaurants_permanently_closed.png", width = 20, height=20, units="cm", res=400)
print(london_map)
dev.off()
