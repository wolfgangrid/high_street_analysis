library(tidyverse)

months <- c("06","07","08","09","10","11","12")

dta <- read_csv("data/3_cleaned/dta_dec.csv")

dta_cities <- dta %>%
  group_by(city,master_category,month) %>%
  summarise(temporarily_closed = mean(status == "temporarily_closed"),
            permanently_closed = mean(status == "permanently_closed")) %>%
  ungroup() %>%
  pivot_longer(cols=c(temporarily_closed,permanently_closed), names_to = "status", values_to = "frac_closed")

cities <- unique(dta_cities$city)
