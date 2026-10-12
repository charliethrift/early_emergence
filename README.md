# Explaining an early ground-nesting bee emergence with climate and occurrence data: Temperature, not precipitation, advances onset
[Charles N. Thrift](https://orcid.org/0000-0002-4257-6951), Jennifer Schlauch Saiyawong, & [Katja C. Seltmann](https://orcid.org/0000-0001-5354-6048)

_In prep_

Please contact Charles Thrift for questions regarding the code or data (cnthrift@ucsb.edu)

## Abstract 
To be added

## Methods Summary
1. Compare observed emergence date to other collection events of this subspecies at or near this nesting site
2. Compare observed emergence date to broader occurrence data for species _Melissodes tepidus_
3. Analyze difference in climatic variables during the development year relative to 30-year normals


# Repository Directory

## Description of data

| File Name | Description |
| :------- | :------ |
| Mel_tep_occ.csv | Occurrence data from Bee Library |
| Dorey_Mel_tep_occ.csv | Occurrence data filtered from Dorey et al., 2023 |
| prism_2025-26_data.csv | Weather data from PRISM during development year |
| prism_1991_2021_daily.csv | Weather data from PRISM for 1991-2021 for 30 year normals | 
| occ_climate_all_df_1oct2026.csv | Filtered occurrence data with climate variables |


## Scripts: Contains code for data analysis in R

| File Name | Description |
| :------- | :------ |
| compile_occurrences.R | Read and filter occurrence data for _M. tepidus_ from Bee Library and Dorey et al., 2023 |
| extract_climate_data.R | Assemble monthly climate data (mean temp, max temp, precipitation) for occurrences with PRISM |
| analyze_climate_doy.R | Analyze relationship between emergence (doy = day of year) and climate |
| weather.R | Analyze difference in climatic variables during the development year relative to 30-year normals  |
| phenology.R | Visualize and compare other occurrence data |





