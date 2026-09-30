# compile_occurrences
# 30 sep 2026
# charles thrift

setwd("~/ground_nest/scripts")

library(tidyverse)
library(vroom) # read large occurrence data
library(sf) # spatial
library(rnaturalearth) # shape file data

# 1. Bee Library
beelib <- read_csv("../data/occ_beelib_30sep2026.csv")
beelib <- beelib %>% 
  select(scientificName,catalogNumber,taxonRank,eventDate,
         year,month,day,decimalLatitude,decimalLongitude) %>% 
  drop_na(eventDate,year,month,day,decimalLatitude,decimalLongitude) %>% # remove if no location/date recorded
  filter(month > 0, # if month is 0, then it wasn't recorded
         day > 1) # if day is 0, it wasn't recorded, if it is 1, it is possible that it wasn't recorded but assigned there (see Methods)

# 2. Dorey et al., 2023
dorey <- vroom("../data/dorey_input/05_cleaned_database_2024-02-15_v1_3_4.csv")
dorey1 <- dorey %>% 
  select(scientificName,catalogNumber,taxonRank,eventDate,
         year,month,day,decimalLatitude,decimalLongitude,
         family,genus,specificEpithet)
dorey2 <- dorey1 %>% filter(family == "Apidae")
dorey3 <- dorey2 %>% filter(genus == "Melissodes")
dorey4 <- dorey3 %>% filter(specificEpithet %in% c("tepida","tepidus"))
dorey5 <- dorey4 %>% 
  drop_na(eventDate,year,month,day,decimalLatitude,decimalLongitude) %>% 
  filter(month > 0, # if month is 0, then it wasn't recorded
         day > 1) # if day is 0, it wasn't recorded, if it is 1, it is possible that it wasn't recorded but assigned there (see Methods)

write.csv(dorey5, "../data/occ_dorey_filtered_30sep2026.csv") # writing this to avoid repeating computationally-intensive filtering steps above

# 3. Merge and remove duplicates
occ <- bind_rows(beelib, dorey5)
occ <- occ %>% 
  distinct(catalogNumber, .keep_all = TRUE)

## 3a. Remove records not from United States (for later climate data)
occ_sf <- st_as_sf(occ, 
                   coords = c("decimalLongitude","decimalLatitude"),
                   crs = 4326,
                   remove = FALSE)

area_of_interest <- ne_states(country = "United States of America",
                              returnclass = "sf") %>% 
  filter(name %in% c("California",
                     "Arizona",
                     "Utah",
                     "Nevada",
                     "Idaho",
                     "Oregon"
                     ))

occ_sf_clipped <- occ_sf %>% 
  st_filter(area_of_interest)

# 4. Filter for Phenology
data <- occ_sf_clipped
data <- data %>% 
  mutate(ymd = lubridate::make_date(year = year,
                                    month = month,
                                    day = day),
         doy = lubridate::yday(ymd)) %>% 
  select(-family,-genus,-specificEpithet)

data1 <- data %>% # retain only 1 record per date/location
  distinct(ymd,geometry,.keep_all = TRUE)

## 4c. Only retain the earliest record from a location in a year (since we are tracking emergence or onset, not the whole flight window)
data2 <- data1 %>% 
  group_by(year,geometry) %>% 
  slice_min(doy, with_ties = FALSE) %>% 
  ungroup()
  
# 5. Questions: 

## 5a. How many occurrence records were there (after step 3)
# 937

## 5b. What is the spread of first occurrence records (range of DOY after step 4c)
summary(data2$doy)
# 123 to 292, with median 191.5, mean 190.
data2$species <- "Melissodes tepidus"
ggplot(data2, aes(species,doy))+
  geom_boxplot()+
  geom_jitter(width = 0.3, height = 0, alpha = 0.3)+
  scale_y_continuous(limits = c(1,366))+
  labs(y = "Julian day (1 to 366)")+
  geom_hline(yintercept = 110,
             linetype = "dashed",
             color = "red")+
  theme_bw()
## 5c. How early is our observed April 20 record to the others? (compare DOY = 110 to spread)
191.5-110
# 81.5 days earlier than median, new earliest record, next closest was doy 123 which is May 5

