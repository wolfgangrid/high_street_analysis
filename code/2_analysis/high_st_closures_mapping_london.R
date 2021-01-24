library(tidyverse)
library(ggplot2)
library(sf)

shp_boroughs <- read_sf("data/2_auxilliary_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
shp_wards <- read_sf("data/2_auxilliary_data/statistical-gis-boundaries-london/ESRI/London_Ward_CityMerged.shp")

ggplot() + geom_sf(data=shp_boroughs$geometry)

dta_chracteristics_london <- read_csv("/Users/wolfgang/Projects/high_street_analysis/data/3_cleaned/dta_chracteristics_uk.csv") %>%
  filter(city == "london")

dta_chracteristics_london2 <- dta_chracteristics_london %>%
  filter(county == "Greater London")

dta_locations <- st_as_sf(dta_chracteristics_london2 %>% select(name,longitude,latitude) %>% filter(!is.na(longitude)),
                    coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

london_map <- ggplot() + geom_sf(data=shp_boroughs$geometry) + 
  geom_sf(data=dta_locations,size=0.5,colour="blue")

print(london_map)

dta_dec <- read_csv("data/3_cleaned/dta_dec.csv") %>%
  filter(city == "london",month=="12")

dta_chracteristics_london3 <- dta_chracteristics_london2 %>%
  left_join(dta_dec) %>%
  filter(!is.na(status)) %>%
  filter(master_category == "restaurants") # restaurants only ! ---------------------------------------------

dta_locations2 <- st_as_sf(dta_chracteristics_london3 %>% select(name,longitude,latitude,status) %>% filter(!is.na(longitude)),
                           coords = c("longitude", "latitude"), crs = 4326, agr = "constant")


london_map <- ggplot() + geom_sf(data=shp_boroughs$geometry) + 
  geom_sf(data=dta_locations2,size=0.1, aes(colour=status))

print(london_map)



dta_locations3 <- st_as_sf(dta_chracteristics_london3 %>% select(name,longitude,latitude,status) %>% filter(!is.na(longitude)),
                           coords = c("longitude", "latitude"), crs = 4326)

dta_locations3 <- st_transform(dta_locations3, crs=st_crs(shp_wards))

dta_locations3 <- dta_locations3 %>%  
  mutate(intersection = as.integer(st_intersects(geometry, shp_wards$geometry))) %>%
  mutate(ward = if_else(is.na(intersection), '', shp_wards$NAME[intersection]))

dta_wards <- as.data.frame(dta_locations3) %>%
  select(-geometry,-intersection) %>%
  group_by(ward) %>%
  summarise(total = n(), usual_status = mean(status == "usual_status"), temporarily_closed = mean(status == "temporarily_closed"), permanently_closed = mean(status == "permanently_closed"))

shp_wards2 <- shp_wards %>%
  left_join(dta_wards, by = c("NAME" = "ward"))


london_map <- ggplot() + geom_sf(data=shp_wards2, aes(fill = permanently_closed))
print(london_map)

# ---
dta_locations3 <- st_as_sf(dta_chracteristics_london3 %>% select(name,longitude,latitude,status) %>% filter(!is.na(longitude)),
                           coords = c("longitude", "latitude"), crs = 4326)

dta_locations3 <- st_transform(dta_locations3, crs=st_crs(shp_boroughs))

dta_locations3 <- dta_locations3 %>%  
  mutate(intersection = as.integer(st_intersects(geometry, shp_boroughs$geometry))) %>%
  mutate(NAME = if_else(is.na(intersection), '', shp_boroughs$NAME[intersection]))

dta_boroughs <- as.data.frame(dta_locations3) %>%
  select(-geometry,-intersection) %>%
  group_by(NAME) %>%
  summarise(total = n(), usual_status = mean(status == "usual_status"), temporarily_closed = mean(status == "temporarily_closed"), permanently_closed = mean(status == "permanently_closed"))

shp_boroughs2 <- shp_boroughs %>%
  left_join(dta_boroughs)

london_map <- ggplot() + geom_sf(data=shp_boroughs2, aes(colour = temporarily_closed,fill = temporarily_closed))
london_map <- london_map + ggtitle("Share of Temporarily Closed Restaurants \n by London Boroughs")
print(london_map)

ggsave(
  "output/london_boroughs_restaurants_temporarily_closed.png",
  london_map,
  width = 3.25,
  height = 3.25,
  dpi = 1200
)

png("output/london_boroughs_restaurants_temporarily_closed.png", width = 20, height=20, units="cm", res=400)
print(london_map)
dev.off()



london_map <- ggplot() + geom_sf(data=shp_boroughs2, aes(colour = permanently_closed,fill = permanently_closed))
london_map <- london_map + ggtitle("Share of Permanently Closed Restaurants \n by London Boroughs")
print(london_map)

png("output/london_boroughs_restaurants_permanently_closed.png", width = 20, height=20, units="cm", res=400)
print(london_map)
dev.off()
