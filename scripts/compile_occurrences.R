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
# NOTE: NEED TO ADD THE OTHER UNDIGITIZED RECORDS SINCE THEY AREN'T IN BEE LIBRARY YET, THEN RE-RUN THIS
# but for now, manually add the 4 missing records:

beelib_additional <- read_csv("../data/temporary_occurrences_1oct2026.csv")
beelib_additional$eventDate <- as.character(beelib_additional$eventDate)
beelib <- bind_rows(beelib,beelib_additional)

# 2. Dorey et al., 2023
dorey <- vroom("../data/dorey_input/05_cleaned_database_2024-02-15_v1_3_4.csv")
dorey2 <- dorey %>% filter(family == "Apidae")
dorey3 <- dorey2 %>% filter(genus == "Melissodes")
dorey4 <- dorey3 %>% filter(specificEpithet %in% c("tepida","tepidus"))
dorey5 <- dorey4 
write.csv(dorey5, "../data/occ_dorey_filtered_30sep2026.csv") # writing this to avoid repeating computationally-intensive filtering steps above

#dorey5 <- read_csv("../data/occ_dorey_filtered_30sep2026.csv")

# 3. Merge and remove duplicates
beelib1 <- beelib |> 
  select(scientificName,catalogNumber,taxonRank,eventDate,
         year,month,day,decimalLatitude,decimalLongitude,
         institutionCode)
dorey6 <- dorey5 |> 
  select(scientificName,catalogNumber,taxonRank,eventDate,
         year,month,day,decimalLatitude,decimalLongitude,
         institutionCode)
occ <- bind_rows(beelib1, dorey6) #593 beelib; 725 dorey; 1318 total
occ1 <- occ |> 
  drop_na(eventDate,year,month,day, # need to do this first in case duplicated records don't have identical metadata
          decimalLatitude,decimalLongitude) # leaves 1246
occ2 <- occ1 %>% 
  distinct(catalogNumber, .keep_all = TRUE) # 998 total


## 3a. Remove records not from United States (for later climate data), and make data spatial
occ_round <- occ2 %>% # rounding for future climate analyses
  mutate(decimalLongitude = round(decimalLongitude, digits = 2),
         decimalLatitude = round(decimalLatitude, digits = 2))

occ_sf <- st_as_sf(occ_round, 
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
  st_filter(area_of_interest) #972 records remain


# filter this after

occ_sf_clipped_1 <- occ_sf_clipped |> 
  filter(month > 0, # if month is 0, then it wasn't recorded
         day > 1) # if day is 0, it wasn't recorded, if it is 1, it is possible that it wasn't recorded but assigned there (see Methods)
nrow(occ_sf_clipped)-nrow(occ_sf_clipped_1) # 57 records got dropped in the above step


# 4. Filter for Phenology
data <- occ_sf_clipped_1
data <- data %>% 
  mutate(ymd = lubridate::make_date(year = year,
                                    month = month,
                                    day = day),
         doy = lubridate::yday(ymd))

data1 <- data %>% # retain only 1 record per date/location
  distinct(ymd,geometry,.keep_all = TRUE) # leaves 381 records

## 4c. Only retain the earliest record from a location in a year (since we are tracking emergence or onset, not the whole flight window)
data2 <- data1 %>% 
  group_by(year,geometry) %>% 
  slice_min(doy, with_ties = FALSE) %>% 
  ungroup() # leaves 248 records

data3 <- data2 |> 
  st_drop_geometry()

data4 <- data3 |> 
  filter(year > 1894) # PRISM climate data starts in 1895

write_csv(data4, "../data/occ_filtered_21oct2026.csv")



# 5. Questions: 

## Which institutions house these data?
institutions <- data4 |> 
  drop_na(institutionCode) |> 
  group_by(institutionCode) |> 
  summarise(count = n())

## What is the spread of first occurrence records (range of DOY after step 4c)
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
## How early is our observed April 20 record to the others? (compare DOY = 110 to spread)
191.5-110
# 81.5 days earlier than median, new earliest record, next closest was doy 123 which is May 5




#### Compare the resulting 233 from here with the existing climate data ones (247)

testing <- anti_join(data,occ_data,by = "catalogNumber") # 14 observations
write_csv(testing, "../data/exclude_14_records.csv")
