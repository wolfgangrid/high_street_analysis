
library(tidyverse)

cities <- c("berlin","detroit","glasgow","la","london","madrid","manchester","milan","newcastle","nyc","paris","rome","stockholm")
first_month <- "06"

get_characteristics_city <- function(data_city) {
  dta_chracteristics <- data_city %>%
    select(id,name,master_category,address,reviews_06_2020,average_review_06_2020,price_06_2020,full_description_06_2020)
  return(dta_chracteristics)
}

dta_chracteristics <- data.frame()
for(city in cities) {
  dta_city <- read_csv(paste0("data/1_raw/",city,"_",first_month,"_2020.csv"))
  dta_chracteristics_city <- get_characteristics_city(dta_city) %>% mutate(city = city)
  dta_chracteristics <- bind_rows(dta_chracteristics,dta_chracteristics_city)
}
rm(city,dta_chracteristics_city,dta_city)

write_csv(dta_chracteristics,"data/3_cleaned/dta_chracteristics.csv")

# - - - UK

get_postcode_uk <- function(i_address) {
  split <- str_split(i_address," ")[[1]]
  len_split <- length(split)
  i_postcode <- paste(split[(len_split-1)],split[len_split])
  return(i_postcode)
} 

get_characteristics_city_uk <- function(data_city) {
  dta_chracteristics <- data_city %>%
    select(id,name,master_category,address,reviews_06_2020,average_review_06_2020,price_06_2020,full_description_06_2020) %>%
    mutate(postcode = sapply(address, function(i) get_postcode_uk(i) ))
  return(dta_chracteristics)
}

cities_uk <- c("london","manchester","newcastle","edinburgh","glasgow")
first_month <- "06"

dta_chracteristics_uk <- data.frame()
for(city in cities_uk) {
  dta_city <- read_csv(paste0("data/1_raw/",city,"_",first_month,"_2020.csv"))
  dta_chracteristics_city <- get_characteristics_city_uk(dta_city) %>% mutate(city = city)
  dta_chracteristics_uk <- bind_rows(dta_chracteristics_uk,dta_chracteristics_city)
}
rm(city,dta_chracteristics_city,dta_city)

# load postcode data
england_postcodes_data <- read_csv("data/2_auxilliary_data/England postcodes.csv")
scotland_postcodes_data <- read_csv("data/2_auxilliary_data/Scotland postcodes.csv")

uk_postcodes <- england_postcodes_data %>%
  select(Postcode,Longitude,Latitude,County,District,Ward) %>%
  bind_rows(scotland_postcodes_data %>% select(Postcode,Longitude,Latitude,County,District,Ward)) %>%
  rename_with(tolower)

#rm(england_postcodes_data,scotland_postcodes_data)

#join to uk characteristics data
dta_chracteristics_uk2 <- dta_chracteristics_uk %>%
  left_join(uk_postcodes, by="postcode") %>%
  mutate(postcode = case_when(!is.na(longitude) ~ postcode)) # keep only postcodes in official list


write_csv(dta_chracteristics_uk2,"data/3_cleaned/dta_chracteristics_uk.csv")

