library(tidyverse)

dta <- lapply(months, function(m) read_csv(paste0("data/1_raw/",city,"_",m,".csv")))
names(dta) <- months

dta1 <- dta[["06_2020"]][ ,c("id","master_category","name","name_06_2020","address_06_2020","status_06_2020")]
months2 <- months[which(months != "06_2020")]
for(m in months2) {
  dta1 <- merge(dta1, dta[[m]][ ,c("id",paste0("name_",m),paste0("status_",m))], by="id", all.x=T)  
}
rm(m,months2)

# drop those where name is not the same across months (prob found the wrong establishment)
# and pivot to long format
dta_names <- dta1 %>%
  select(id,master_category,name,starts_with("name_")) %>%
  pivot_longer(cols = starts_with("name_"), names_to = "month", names_prefix = "name_", values_to = "name_month") %>%
  group_by(id,master_category,name) %>%
  filter(!is.na(name_month)) %>%
  mutate(n_names = n_distinct(name_month)) %>%
  filter(n_names == 1) %>%
  filter(name_month != "not_found") %>%
  select(-n_names) %>%
  # complete months
  complete(month = months) %>%
  ungroup()

# get status data in long format
dta_status <- dta1 %>%
  select(id,master_category,name,starts_with("status_")) %>%
  pivot_longer(cols = starts_with("status_"), names_to = "month", names_prefix = "status_", values_to = "status")

# past together names and status in long format
dta2 <- dta_names %>%
  left_join(dta_status) %>%
  select(-name_month)

rm(dta_names,dta_status)

dta3 <- dta2 %>%
  rename(month_year = month) %>%
  mutate(year = as.numeric(sapply(month_year, function(i) str_split(i,"_")[[1]][2]))) %>%
  mutate(month = as.numeric(sapply(month_year, function(i) str_split(i,"_")[[1]][1]))) %>%
  arrange(id,year,month) %>%
  group_by(id,master_category,name) %>%
  mutate(lag_status = lag(status)) %>%
  mutate(lead_status = lead(status)) %>%
  mutate(status1 = status) %>%
  # fill in permanently closed forward (if permanently closed once and then na, take as permanently closed)
  mutate(status1 = case_when(is.na(status1) & lag_status == "permanently_closed" ~ "permanently_closed", TRUE ~ status1)) %>%
  # fill in usual status backward (if na and then usual status, take as usual status)
  mutate(status1 = case_when(is.na(status1) & lead_status == "usual_status" ~ "usual_status", TRUE ~ status1))
  
dta4 <- dta3 %>%
  select(-status,-lag_status,-lead_status) %>%
  rename(status = status1) %>%
  filter(!is.na(status)) %>%
  group_by(id,master_category,name) %>%
  mutate(n_months = n()) %>%
  ungroup() %>%
  filter(n_months == length(months)) %>%
  select(-n_months)

# drop if already permanently closed at beginning of sample
dta5 <- dta4 %>%
  mutate(closed_06 = (status == "permanently_closed" & month_year == "06_2020" )) %>%
  group_by(id) %>%
  mutate(closed_at_start = any(closed_06 == TRUE)) %>%
  filter(closed_at_start == FALSE) %>%
  select(-closed_06,-closed_at_start) %>%
  # permanently closed first and then open again, change to temporarily closed
  mutate(lead_status = lead(status)) %>%
  mutate(status1 = case_when(status == "permanently_closed" & lead_status != "permanently_closed" ~ "temporarily_closed",
                             TRUE ~ status)) %>%
  mutate(resurrected = any(status != status1)) %>%
  # if resurrected, change all permanently_closed to temporarily_closed for that ID
  mutate(status2 = case_when(resurrected == TRUE & status == "permanently_closed" ~ "temporarily_closed",
                             TRUE ~ status)) %>%
  ungroup() %>%
  select(-status,-lead_status,-status1,-resurrected) %>%
  rename(status = status2)

dta_city <- dta5 %>%
  #mutate(year = substr(month, 4, 8)) %>%
  mutate(city = city) %>%
  arrange(id,year,month)

rm(dta,dta1,dta2,dta3,dta4,dta5)


