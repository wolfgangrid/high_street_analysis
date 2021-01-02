library(tidyverse)

months <- c("06","07","08","09","10","11","12")

dta <- read_csv("data/3_cleaned/dta_dec.csv")

dta_cities <- dta %>%
  group_by(city,master_category,month) %>%
  summarise(temporarily_closed = mean(status == "temporarily_closed"),
            permanently_closed = mean(status == "permanently_closed")) %>%
  ungroup()



ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
       aes(x=month,y=temporarily_closed,group=city,colour=city)) + geom_line()
  

ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
       aes(x=month,y=permanently_closed,group=city,colour=city)) + geom_line() +
  geom_text(data=dta_cities %>% filter(master_category == "restaurants", month==max(months)),
            aes(x=month,y=permanently_closed,label=city), show.legend = FALSE)


ggplot(data = dta_cities,
       aes(x=month,y=permanently_closed,group=interaction(city,master_category),colour=master_category)) + geom_line()

ggplot(data = dta_cities,
       aes(x=month,y=temporarily_closed,group=interaction(city,master_category),colour=master_category)) + geom_line()

dta_cities_wider <- dta_cities %>%
  select(-temporarily_closed) %>%
  pivot_wider(names_from = master_category, values_from = permanently_closed, names_prefix = "permanently_closed_") %>%
  left_join(dta_cities %>% select(-permanently_closed) %>% pivot_wider(names_from = master_category, values_from = temporarily_closed, names_prefix = "temporarily_closed_") )

ggplot(data = dta_cities_wider,
       aes(x=permanently_closed_shopping,y=permanently_closed_restaurants,color=city)) + geom_point()

ggplot(data = dta_cities_wider,
       aes(x=temporarily_closed_shopping,y=temporarily_closed_restaurants,color=city)) + geom_point()

# by shop / restaurant
dta_characteristics <- read_csv("data/3_cleaned/dta_chracteristics.csv")

# last month
dta_last <- dta %>%
  filter(month == max(months)) %>%
  left_join(dta_characteristics %>% select(id,reviews_06_2020,average_review_06_2020), by="id")

ggplot(data = dta_last %>% filter(master_category == "restaurants"),
       aes(x=status, y=reviews_06_2020)) +
  geom_boxplot()

View( dta_last %>% group_by(city,status) %>% summarise(avg_review = mean(average_review_06_2020,na.rm=T)) )
dta_last %>% group_by(status) %>% summarise(n_reviews = mean(reviews_06_2020,na.rm=T))


# - - - UK
dta_characteristics_uk <- read_csv("data/3_cleaned/dta_chracteristics_uk.csv")
