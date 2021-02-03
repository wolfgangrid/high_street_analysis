library(tidyverse)

months <- read_csv("data/months.csv", col_names = FALSE) %>% pull()

dta <- read_csv("data/3_cleaned/dta_dec.csv")

dta_cities <- dta %>%
  group_by(city,master_category,month) %>%
  summarise(temporarily_closed = mean(status == "temporarily_closed"),
            permanently_closed = mean(status == "permanently_closed")) %>%
  ungroup() %>%
  pivot_longer(cols=c(temporarily_closed,permanently_closed), names_to = "status", values_to = "frac_closed")

#cities <- unique(dta_cities$city)

rm(dta)

write.csv(dta_cities,"high-street-app/dta_app.csv", row.names = FALSE)

#save.image("~/Projects/high_street_analysis/app/high_st_app_data.RData")

