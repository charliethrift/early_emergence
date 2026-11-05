# Explaining an early ground-nesting bee emergence with climate and occurrence data: Temperature, not precipitation, advances onset
[Charles N. Thrift](https://orcid.org/0000-0002-4257-6951), [Madeleine M. Ostwald](https://orcid.org/0000-0002-9869-8835), [Jennifer Schlauch Saiyawong](https://orcid.org/0009-0004-2201-2077), & [Katja C. Seltmann](https://orcid.org/0000-0001-5354-6048)

_In prep_

Please contact Charles Thrift for questions regarding the code or data (cnthrift@ucsb.edu)

## Abstract 
Understanding the drivers of bee phenology is important for predicting how climate change may alter the temporal overlap between bee activity and plant flowering, with consequences for pollination. We documented unusually early emergence of Melissodes tepidus timberlakei Cockerell, 1926 at a perennial nesting site in spring 2026 and examined the relationship between emergence and climatic conditions with (1) five years of nesting observations at a single site, and (2) 119 years of M. tepidus occurrence records from across the Western United States. We found warming temperatures were associated with earlier emergence, whereas precipitation was not, and we emphasize the utility of monitoring ground nests to understand emergence drivers in bees during ongoing climate change.

## Methods Summary
1. Compare emergence dates in the previous five years at one nesting site to weather during development for _Melissodes tepidus timberlakei_
2. Compare emergence dates with 117 years of occurrence data to climate conditions for _Melissodes tepidus_


# Repository Directory

## Description of data

| File Name | Description |
| :------- | :------ |
| Table_S1_full_dataset_w_occ_data.csv | Full dataset of occurrence data (proxying emergence) and climate variables |
| occ_beelib_30sep2026.csv | Occurrence data from Bee Library |
| occ_climate_all_df_1oct2026.csv | Filtered occurrence data with climate variables |
| occ_dorey_filtered_30sep2026.csv | Occurrence data filtered from Dorey et al., 2023 |
| prism_1991_2021_daily.csv | Weather data from PRISM for 1991-2021 for 30 year normals and 2022-2025 for last 5 year analyses | 
| prism_2025-26_data.csv | Weather data from PRISM during 2026 development year |


## Scripts: Contains code for data analysis in R

| File Name | Description |
| :------- | :------ |
| compile_occurrences.R | Read and filter occurrence data for _M. tepidus_ from Bee Library and Dorey et al., 2023 |
| extract_climate_data.R | Assemble monthly climate data (mean temp, max temp, precipitation) for occurrences with PRISM |
| analyze_climate_doy.R | Analyze relationship between emergence (doy = day of year) and climate |
| analyze_last5yrs_nest.R | Analyze difference in climatic variables during the development year relative to 30-year normals  |





