library(tidyverse)

months <- c("06","07","08","09","10","11","12")

#cities <- c("berlin","detroit","glasgow","la","london","madrid","manchester","milan","newcastle","nyc","paris","rome","stockholm")
cities <- c("london","manchester","newcastle","edinburgh","glasgow","berlin","madrid","rome","paris")
#cities <- c("manchester","newcastle","edinburgh","glasgow")


dta_long <- data.frame()
for(city in cities) {
  if(city == "edinburgh") {
    months1 <- months
    months <- months[months != "10"]
    source("code/1_data_cleaning/high_st_closures_data_cleaning_city.R")
    months <- months1
    rm(months1)
  }
  else{
    source("code/1_data_cleaning/high_st_closures_data_cleaning_city.R")
  }
  dta_long <- bind_rows(dta_long,dta_city)
}
rm(dta_city,city)

write_csv(dta_long,"data/3_cleaned/dta_dec.csv")
