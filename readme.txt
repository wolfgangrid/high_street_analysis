data/1_raw
- needs to contain scraped files for each city and each month
- scrape the same list of shops and restaurants from google maps once per month
- list: all shops and restaurants with reviews on Yelp that we find on google maps

data/2_auxilliary_data
- UK postcode data from https://www.doogal.co.uk/PostcodeDownloads.php
- London shapefiles from https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london

data/3_cleaned
run `code/1_data_cleaning/high_st_closures_data_cleaning_all.R`
which `calls code/1_data_cleaning/high_st_closures_data_cleaning_city.R`
- input: data from data/1_raw
- keep shops and restaurants if they have the same name in every month
- drop if shown as permanently closed at the beginning of the sample
- definition of permanently closed: shows as permanently closed in month t and then either (i) in all subsequent months also shows as permanently closed or (ii) is not found anymore some time after first shown as permanently closed


code/1_data_cleaning/high_st_closures_data_cleaning_all.R
- calls `code/1_data_cleaning/high_st_closures_data_cleaning_city.R` for each city