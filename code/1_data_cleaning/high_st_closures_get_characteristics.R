
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
  dta_chracteristics_city <- get_characteristics_city(dta_city)
  dta_chracteristics <- bind_rows(dta_chracteristics,dta_chracteristics_city)
}

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
  dta_chracteristics_city <- get_characteristics_city_uk(dta_city)
  dta_chracteristics_uk <- bind_rows(dta_chracteristics_uk,dta_chracteristics_city)
}

write_csv(dta_chracteristics_uk,"data/3_cleaned/dta_chracteristics_uk.csv")

