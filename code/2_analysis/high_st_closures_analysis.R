library(tidyverse)
library(gridExtra)

dta <- read_csv("data/3_cleaned/dta_jan.csv")

dta_cities <- dta %>%
  group_by(city,master_category,month,year) %>%
  summarise(temporarily_closed = mean(status == "temporarily_closed"),
            permanently_closed = mean(status == "permanently_closed")) %>%
  ungroup() %>%
  arrange(city,year,month)

ggplot(data = dta_cities %>% filter(master_category == "shopping"),
       aes(x=reorder(month,year),y=temporarily_closed,group=city,colour=city)) + geom_line() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
       aes(x=reorder(month,year),y=temporarily_closed,group=city,colour=city)) + geom_line() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
       aes(x=reorder(month,year),y=permanently_closed,group=city,colour=city)) + geom_line() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = dta_cities %>% filter(master_category == "shopping"),
       aes(x=reorder(month,year),y=permanently_closed,group=city,colour=city)) + geom_line() +
  theme(axis.text.x = element_text(angle = 90))

p1 <- ggplot(data = dta_cities %>% filter(master_category == "shopping"),
             aes(x=reorder(month,year),y=temporarily_closed,
                 group=city,
                 colour=city)) +
  geom_line() +
  ylim(0,0.4) +
  theme(plot.title = element_text(size = 16),
        legend.position = "none",
        text = element_text(size=14),
        aspect.ratio=8/10) +
  ggtitle("Temporarily Closed Shops") +
  xlab("Month") +
  ylab("")

p2 <- ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
             aes(x=reorder(month,year),y=temporarily_closed,
                 group=city,
                 colour=city)) +
  geom_line() +
  ylim(0,0.4) +
  theme(plot.title = element_text(size = 16),
        legend.justification = c(-0.05, -1.2),
        legend.position = c(0,0),
        text = element_text(size=14),
        aspect.ratio=8/10) +
  ggtitle("Temporarily Closed Restaurants") +
  xlab("Month") +
  ylab("")

#p1
#p2
png("output/temp_closed_2021_01.png", width = 20, height=10, units="cm", res=400)
grid.arrange(p1,p2, nrow=1)
dev.off()

p1 <- ggplot(data = dta_cities %>% filter(master_category == "shopping"),
             aes(x=reorder(month,year),y=permanently_closed,
                 group=city,
                 colour=city)) +
  geom_line() +
  ylim(0,0.04) +
  theme(plot.title = element_text(size = 16),
        legend.position = "none",
        text = element_text(size=14),
        aspect.ratio=8/10) +
  ggtitle("Temporarily Closed Shops") +
  xlab("Month") +
  ylab("")

p2 <- ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
             aes(x=reorder(month,year),y=permanently_closed,
                 group=city,
                 colour=city)) +
  geom_line() +
  ylim(0,0.04) +
  theme(plot.title = element_text(size = 16),
        legend.justification = c(-0.05, -1.2),
        legend.position = c(0,0),
        text = element_text(size=14),
        aspect.ratio=8/10) +
  ggtitle("Temporarily Closed Restaurants") +
  xlab("Month") +
  ylab("")

p1
p2

png("output/perm_closed_2021_01.png", width = 20, height=10, units="cm", res=400)
grid.arrange(p1,p2, nrow=1)
dev.off()


# ----
# 2020 Data
# ----
months <- read_csv("data/months.csv", col_names = FALSE) %>% pull()

dta <- read_csv("data/3_cleaned/dta_dec.csv")

dta_cities <- dta %>%
  group_by(city,master_category,month) %>%
  summarise(temporarily_closed = mean(status == "temporarily_closed"),
            permanently_closed = mean(status == "permanently_closed")) %>%
  ungroup()



#ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
#       aes(x=month,y=temporarily_closed,group=city,colour=city)) + geom_line()
  

#ggplot(data = dta_cities %>% filter(master_category == "restaurants"),
#       aes(x=month,y=permanently_closed,group=city,colour=city)) + geom_line() +
#  geom_text(data=dta_cities %>% filter(master_category == "restaurants", month==max(months)),
#            aes(x=month,y=permanently_closed,label=city), show.legend = FALSE)


p1 <- ggplot(data = dta_cities,
             aes(x=month,y=temporarily_closed,group=interaction(city,master_category),colour=master_category)) +
  geom_line() +
  ylim(0,0.4) +
  theme(plot.title = element_text(size = 16),
        legend.position = "none",
        text = element_text(size=14),
        aspect.ratio=8/10) +
  ggtitle("Temporarily Closed") +
  xlab("Month") +
  ylab("")

#p1

p2 <- ggplot(data = dta_cities,
             aes(x=month,y=permanently_closed,group=interaction(city,master_category),colour=master_category)) +
  geom_line() +
  ylim(0,0.1) +
  theme(plot.title = element_text(size = 16),
        legend.justification = c(-0.05, -1.7),
        legend.position = c(0,0),
        text = element_text(size=14),
        aspect.ratio=8/10) +
  ggtitle("Permanently Closed") +
  xlab("Month") +
  ylab("")

#p2

png("output/restaurants_vs_shops.png", width = 20, height=10, units="cm", res=400)
grid.arrange(p1,p2, nrow=1)
dev.off()

for(cat in c("restaurants","shopping")) {
  
  p1 <- ggplot(data = dta_cities %>% filter(city %in% c("stockholm","london","paris","berlin"),
                                               master_category == cat)) +
    geom_line(aes(x=month,y=temporarily_closed,group=city,colour=city)) +
    theme(plot.title = element_text(size = 16),
          legend.position = "none",
          text = element_text(size=14),
          aspect.ratio=8/10) +
    ggtitle(paste0("Temporarily Closed ",cat)) +
    xlab("Month") +
    ylab("")

#p1

  p2 <- ggplot(data = dta_cities %>% filter(city %in% c("stockholm","london","paris","berlin"),
                                                 master_category == cat)) +
    geom_line(aes(x=month,y=permanently_closed,group=city,colour=city)) +
    theme(plot.title = element_text(size = 16),
          legend.justification = c(-0.05, -0.8),
          legend.position = c(0,0),
          text = element_text(size=14),
          aspect.ratio=8/10) +
    ggtitle(paste0("Permanently Closed ",cat)) +
    xlab("Month") +
    ylab("")

#p2

  png(paste0("output/temp_vs_perm_",cat,".png"), width = 20, height=10, units="cm", res=400)
  grid.arrange(p1,p2, nrow=1)
  dev.off()
}

  


# # # 

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

dta_last %>% group_by(status) %>% summarise(n_reviews = mean(reviews_06_2020,na.rm=T))
dta_last %>% group_by(status) %>% summarise(n_reviews = mean(average_review_06_2020,na.rm=T))


# - - - UK
dta_characteristics_uk <- read_csv("data/3_cleaned/dta_chracteristics_uk.csv")

