library(tidyverse)

data_files <- list.files("data/1_raw")

#cities <- c("berlin","edinburgh","glasgow","la","london","madrid","manchester","milan","newcastle","nyc","paris","rome","stockholm")
cities <- c("berlin","london","la")

dta_long <- data.frame()

for(city in cities) {

  city1 <- paste0(city,"_")
  months <- data_files[grep(city1,data_files)]
  months <- gsub(city1,"",months)
  months <- gsub(".csv","",months)

  source("code/1_data_cleaning/high_st_closures_data_cleaning_city.R")

  dta_long <- bind_rows(dta_long,dta_city)
  
}

rm(dta_city,city)

write_csv(dta_long,"data/3_cleaned/dta_jan.csv")
