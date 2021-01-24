
cities <- c("Berlin","Edinburgh","Glasgow","London","Madrid","Manchester","Milan","Tyneside","Newcastle","Paris","Rome","Stockholm")

shp_europe <- read_sf("/Users/wolfgang/Downloads/data_FUA_database 2/Fua_geometries/fua_geometries.shp")
#source: https://data.gov.uk/dataset/11302ddc-65bc-4a8f-96a9-af5c456e442c/counties-and-unitary-authorities-december-2016-full-clipped-boundaries-in-england-and-wales

list <- read.csv("/Users/wolfgang/Downloads/data_FUA_database 2/Correspondance_table/fua_mua_lau2_20111207.csv", sep=";", header = T)

list_sel <- list %>%
  distinct(fua,id_fua) %>%
  filter(grepl(paste(cities,collapse = "|"),fua,ignore.case = F)) %>%
  filter(fua != "Londonderry")

shp_sel <- shp_europe %>%
  inner_join(list_sel) %>%
  group_by(fua) %>%
  mutate(fua1 = str_split(fua," ")[[1]][1]) %>%
  ungroup()


city_maps <- lapply(c(1:nrow(shp_sel)), function(i) ggplot() + geom_sf(data=shp_sel$geometry[i]) + ggtitle(paste(shp_sel$fua[i])) )

for(i in c(1:length(city_maps))) {
  print(city_maps[[i]])  
}  
rm(i)


# get restaurants data for uk
dta_dec <- read_csv("data/3_cleaned/dta_dec.csv") %>%
  filter(month=="12")

dta_chracteristics <- read_csv("/Users/wolfgang/Projects/high_street_analysis/data/3_cleaned/dta_chracteristics_uk.csv")

dta_dec2 <- dta_dec %>%
  inner_join(dta_chracteristics %>% select(id,longitude,latitude))

# map to city shapes
dta_dec3 <- st_as_sf(dta_dec2 %>% select(name,city,longitude,latitude,status) %>% filter(!is.na(longitude)),
                           coords = c("longitude", "latitude"), crs = 4326)
dta_dec3 <- st_transform(dta_dec3, crs=st_crs(shp_sel))

dta_dec3 <- dta_dec3 %>%  
  mutate(intersection = as.integer(st_intersects(geometry, shp_sel$geometry))) %>%
  mutate(fua = if_else(is.na(intersection), '', shp_sel$fua[intersection])) %>%
  filter(!is.na(intersection)) %>%
  select(-intersection) %>%
  group_by(fua) %>%
  mutate(fua1 = str_split(fua," ")[[1]][1]) %>%
  ungroup()

unique_fua <- unique(dta_dec3$fua1)

map1 <- ggplot() + geom_sf(data=shp_sel$geometry[which(shp_sel$fua1 == unique_fua[1])])
map1 <- map1 + geom_sf(data= dta_dec3 %>% filter(fua1 == unique_fua[1], status == "usual_status"), col="green",size=0.1)
map1 <- map1 + geom_sf(data= dta_dec3 %>% filter(fua1 == unique_fua[1], status == "temporarily_closed"), col="yellow",size=1)
map1 <- map1 + geom_sf(data= dta_dec3 %>% filter(fua1 == unique_fua[1], status == "permanently_closed"), col="red",size=1)
map1 <- map1 + ggtitle(paste(unique_fua[1]))

print(map1)

maps <- list()
for(f in unique_fua) {
  map_tmp <- ggplot() + geom_sf(data=shp_sel$geometry[which(shp_sel$fua1 == f)])
  map_tmp <- map_tmp + geom_sf(data= dta_dec3 %>% filter(fua1 == f, status == "usual_status"), col="green",size=0.1)
  map_tmp <- map_tmp + geom_sf(data= dta_dec3 %>% filter(fua1 == f, status == "temporarily_closed"), col="yellow",size=0.5)
  map_tmp <- map_tmp + geom_sf(data= dta_dec3 %>% filter(fua1 == f, status == "permanently_closed"), col="red",size=1)
  map_tmp <- map_tmp + ggtitle(paste(f))
  maps <- append(maps,list(map_tmp))
}
rm(f,map_tmp)
names(maps) <- unique_fua

print(maps[["Tyneside"]])

#save
for(f in unique_fua) {
  png(paste0("output/city_map_dots_",f,".png"))
  print(maps[[f]])
  dev.off()
}
  
print(myplot)
dev.off()