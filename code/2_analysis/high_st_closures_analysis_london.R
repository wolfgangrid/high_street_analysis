
# # # END of CLEANING ?

city_summary <- dta5 %>%
  group_by(master_category,month) %>%
  summarise(mean(status == "permanently_closed")) %>%
  rename(closed = starts_with("mean")) %>%
  ungroup() %>%
  mutate(city = paste(city))

rm(dta1,dta2,dta3)

london_postcodes <- read_csv("data/2_auxilliary_data/London postcodes.csv")

#london_postcode_district <- london_postcodes %>% distinct(`Postcode district`) %>% pull()
london_postcode_areas <- london_postcodes %>% distinct(`Postcode area`) %>% pull()

get_postcode <- function(i_address) {
  split <- str_split(i_address," ")[[1]]
  len_split <- length(split)
  i_postcode <- paste(split[(len_split-1)],split[len_split])
  return(i_postcode)
} 

get_district <- function(i_address) {
  split <- str_split(i_address," ")[[1]]
  district <- split[which(split %in% london_postcode_district)]
  return(district)
}

dta_chracteristics <- dta[[1]] %>%
  select(id,name,master_category,address,reviews_06_2020,average_review_06_2020,price_06_2020,full_description_06_2020) %>%
  mutate(postcode = sapply(address, function(i) get_postcode(i) )) %>%
  left_join(london_postcodes %>% select(Postcode,`Postcode area`,`Postcode district`,`London zone`),
            by = c("postcode" = "Postcode")) %>%
  rename(area = `Postcode area`, district = `Postcode district`, zone = `London zone`)

#  mutate(district = sapply(address, function(i) get_district(i) )) %>%
#  mutate(district = case_when(district != "character(0)" ~ district, TRUE ~ list(NA_character_))) %>%
#  mutate(area = sapply(district, function(i) str_split(i,"[0-9]")[[1]][1] ))

# get districts that are at least partly in zone 1
central_districts <- london_postcodes %>%
  rename(district = `Postcode district`, zone = `London zone`) %>%
  distinct(district,zone) %>%
  filter(zone == 1) %>%
  pull()


district_summary <- dta5 %>%
  left_join(dta_chracteristics %>% select(id,district), by="id") %>%
  filter(!is.na(district)) %>%
  group_by(district,month) %>%
  summarise(total = n(),closed = mean(status == "permanently_closed")) %>%
  mutate(diff = closed - lag(closed)) %>%
  ungroup() %>%
  filter(total > 100) %>%
  mutate(central_district = district %in% central_districts)

ggplot(data = district_summary, aes(x=month,y=closed,group=district,color=central_district)) + geom_line()


central_areas <- c("WC","EC","NW","N","E","SE","SW","W")

very_central <- c("WC","EC")
semi_central <- c("NW","N","E","SE","SW","W")

area_summary <- dta5 %>%
  left_join(dta_chracteristics %>% select(id,area,zone), by="id") %>%
  filter(area %in% london_postcode_areas) %>%
  group_by(area,month) %>%
  summarise(total = n(),
            closed = mean(status == "permanently_closed"),
            temp_closed = mean(status == "temporarily_closed")) %>%
  #mutate(diff = closed - lag(closed)) %>%
  ungroup() %>%
  filter(total > 400) %>%
  mutate(central_area = area %in% central_areas) %>%
  mutate(area_centrality = case_when(area %in% very_central ~ "very_central",
                                     area %in% semi_central ~ "semi_central",
                                     TRUE ~ "greater_london"))


#ggplot(data = area_summary, aes(x=month,y=closed,group=area,color=area)) + geom_line()

ggplot(data = area_summary, aes(x=month,y=closed,group=area,color=central_area)) + geom_line()


label_if_closed <- quantile(area_summary$closed[area_summary$month==max(months)],probs=0.75)

ggplot(data = area_summary, aes(x=month,y=closed,group=area,color=area_centrality)) + geom_line() +
  geom_text(data=subset(area_summary, month == max(months) & closed > label_if_closed),
            aes(x=month,y=closed,label=area), show.legend = FALSE)

ggplot(data = area_summary, aes(x=month,y=temp_closed,group=area,color=area_centrality)) + geom_line() +
  geom_text(data=subset(area_summary, month == max(months) & closed > label_if_closed),
            aes(x=month,y=temp_closed,label=area), show.legend = FALSE)


ggplot(data = area_summary, aes(x=month,y=closed,group=area,color=central_area)) + geom_line()
ggplot(data = area_summary, aes(x=month,y=temp_closed,group=area,color=central_area)) + geom_line()


zones_summary <- dta5 %>%
  left_join(dta_chracteristics %>% select(id,zone), by="id") %>%
  filter(!is.na(zone)) %>%
  group_by(zone,month) %>%
  summarise(total = n(),
            closed = mean(status == "permanently_closed"),
            temp_closed = mean(status == "temporarily_closed")) %>%
  ungroup() %>%
  filter(total > 100)

ggplot(data = zones_summary, aes(x=month,y=closed,group=zone,color=as.character(zone))) + geom_line()
ggplot(data = zones_summary, aes(x=month,y=temp_closed,group=zone,color=as.character(zone))) + geom_line()


dta6 <- dta5 %>%
  left_join(dta_chracteristics %>% select(id,area), by="id")

