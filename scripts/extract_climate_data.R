# extract_climate_data
# charles thrift
# 30 sep 2026

setwd("~/ground_nest/scripts")

library(tidyverse)
library(sf)

# Read in occurrence data

occ_data <- read_csv("../data/occ_filtered_21oct2026.csv")
occ_data <- occ_data %>% 
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"),
           crs = 4326,
           remove = FALSE)
data <- occ_data %>% 
  mutate(row_id = row_number())

# Define function to extract temperature data
prism_extract <- function(data, variable) {
  
  # --------------------------------------------------
  # Create the 18 climate months needed for each record
  # --------------------------------------------------
  
  data_months <- data %>%
    sf::st_drop_geometry() %>%
    select(row_id, year) %>%
    distinct() %>%
    mutate(
      start_date = as.Date(
        paste0(year - 1, "-07-01")
      ),
      end_date = as.Date(
        paste0(year, "-12-01")
      )
    ) %>%
    rowwise() %>%
    mutate(
      climate_date = list(
        seq.Date(
          start_date,
          end_date,
          by = "month"
        )
      )
    ) %>%
    ungroup() %>%
    tidyr::unnest(climate_date) %>%
    mutate(
      
      # Actual calendar year of the climate data
      climate_year = as.integer(
        format(climate_date, "%Y")
      ),
      
      # Actual calendar month
      climate_month = as.integer(
        format(climate_date, "%m")
      ),
      
      # Name used for downstream analyses
      #
      # Previous-year July-Dec:
      #   prev7, prev8, ..., prev12
      #
      # Occurrence-year January-Dec:
      #   1, 2, ..., 12
      climate_period = if_else(
        climate_year < year,
        paste0("prev", climate_month),
        as.character(climate_month)
      ),
      
      # PRISM download identifier
      ym = format(
        climate_date,
        "%Y%m"
      )
    )
  
  
  # --------------------------------------------------
  # Unique PRISM months that need to be downloaded
  # --------------------------------------------------
  
  months_needed <- unique(
    data_months$ym
  )
  
  results <- vector(
    "list",
    length(months_needed)
  )
  
  
  # --------------------------------------------------
  # Loop through each unique month
  # --------------------------------------------------
  
  for (i in seq_along(months_needed)) {
    
    this_ym <- months_needed[i]
    
    this_year <- substr(
      this_ym,
      1,
      4
    )
    
    message(
      "Downloading PRISM ",
      variable,
      " for ",
      this_ym,
      " (",
      i,
      "/",
      length(months_needed),
      ")"
    )
    
    
    # ------------------------------------------------
    # Construct PRISM download URL
    # ------------------------------------------------
    
    url <- paste0(
      "https://data.prism.oregonstate.edu/time_series/us/an/4km/",
      variable,
      "/monthly/",
      this_year,
      "/prism_",
      variable,
      "_us_25m_",
      this_ym,
      ".zip"
    )
    
    
    # ------------------------------------------------
    # Download ZIP
    # ------------------------------------------------
    
    zipfile <- tempfile(
      fileext = ".zip"
    )
    
    tryCatch({
      
      curl::curl_download(
        url,
        zipfile,
        mode = "wb",
        quiet = TRUE
      )
      
    }, error = function(e) {
      
      message(
        "  DOWNLOAD ERROR: ",
        e$message
      )
      
    })
    
    
    # ------------------------------------------------
    # Check download
    # ------------------------------------------------
    
    if (!file.exists(zipfile) ||
        file.size(zipfile) < 100000) {
      
      message(
        "  Download appears invalid: ",
        file.size(zipfile),
        " bytes"
      )
      
      next
    }
    
    
    # ------------------------------------------------
    # Unzip
    # ------------------------------------------------
    
    outdir <- tempfile()
    dir.create(outdir)
    
    tryCatch({
      
      unzip(
        zipfile,
        exdir = outdir
      )
      
    }, error = function(e) {
      
      message(
        "  UNZIP ERROR: ",
        e$message
      )
      
    })
    
    
    # ------------------------------------------------
    # Find GeoTIFF
    # ------------------------------------------------
    
    tif <- list.files(
      outdir,
      pattern = "\\.tif$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(tif) == 0) {
      
      message(
        "  No .tif file found."
      )
      
      unlink(zipfile)
      unlink(
        outdir,
        recursive = TRUE
      )
      
      next
    }
    
    message(
      "  Raster: ",
      basename(tif[1])
    )
    
    
    # ------------------------------------------------
    # Read raster
    # ------------------------------------------------
    
    r <- tryCatch(
      
      terra::rast(tif[1]),
      
      error = function(e) {
        
        message(
          "  RASTER ERROR: ",
          e$message
        )
        
        return(NULL)
      }
    )
    
    if (is.null(r)) {
      
      unlink(zipfile)
      unlink(
        outdir,
        recursive = TRUE
      )
      
      next
    }
    
    
    # ------------------------------------------------
    # Select occurrence points needing this month
    # ------------------------------------------------
    
    d_info <- data_months %>%
      filter(
        ym == this_ym
      )
    
    
    # ------------------------------------------------
    # Reattach geometry
    # ------------------------------------------------
    
    d <- data %>%
      select(
        row_id,
        geometry
      ) %>%
      left_join(
        d_info,
        by = "row_id"
      ) %>%
      sf::st_as_sf()
    
    
    # ------------------------------------------------
    # Extract PRISM values
    # ------------------------------------------------
    
    pts <- terra::vect(d)
    
    vals <- terra::extract(
      r,
      pts,
      method = "simple"
    )
    
    
    # ------------------------------------------------
    # Store results
    # ------------------------------------------------
    
    results[[i]] <- d %>%
      sf::st_drop_geometry() %>%
      mutate(
        value = vals[[2]]
      ) %>%
      select(
        row_id,
        year,
        climate_period,
        climate_year,
        climate_month,
        value
      )
    
    
    # ------------------------------------------------
    # Clean up
    # ------------------------------------------------
    
    unlink(zipfile)
    
    unlink(
      outdir,
      recursive = TRUE
    )
  }
  
  
  # --------------------------------------------------
  # Remove failed months
  # --------------------------------------------------
  
  results <- results[
    !vapply(
      results,
      is.null,
      logical(1)
    )
  ]
  
  
  # --------------------------------------------------
  # Check whether anything was extracted
  # --------------------------------------------------
  
  if (length(results) == 0) {
    
    stop(
      "No PRISM data were successfully extracted."
    )
  }
  
  
  # --------------------------------------------------
  # Combine all months
  # --------------------------------------------------
  
  output <- dplyr::bind_rows(
    results
  )
  
  
  # --------------------------------------------------
  # Rename extracted values
  # --------------------------------------------------
  
  names(output)[
    names(output) == "value"
  ] <- variable
  
  
  # --------------------------------------------------
  # Return result
  # --------------------------------------------------
  
  output
}







# Use the prism_extract function,
# for mean monthly temperaturess and maximum monthly temperatures

tmax_data <- prism_extract(data, "tmax")

tmax_data1 <- tmax_data %>% 
  drop_na(year)

#write_csv(tmax_data1, "../data/tmax_data_1oct2026.csv")
#testing123 <- read_csv("../data/tmax_data_1oct2026.csv")

tmean_data <- prism_extract(data, "tmean")

tmean_data1 <- tmean_data %>% 
  drop_na(year)

#write_csv(tmean_data1, "../data/tmean_data_1oct2026.csv")
#testing123 <- read_csv("../data/tmean_data_1oct2026.csv")


# combine the tmean and tmax into one dataframe, with occurrence data, and write it as csv.

monthly_climate <- left_join(tmax_data1,tmean_data1,
                             by = c("row_id","year",
                             "climate_period","climate_year",
                             "climate_month"))
month_clim_wide <- monthly_climate %>% 
  pivot_wider(
    id_cols = c(row_id, year),
    names_from = climate_period,
    values_from = c(tmax, tmean),
    names_glue = "{.value}_m.{climate_period}"
  )

occ_climate <- left_join(data,month_clim_wide,
                         by = c("row_id","year"))
occ_climate_df <- st_drop_geometry(occ_climate)

#write_csv(occ_climate_df, "../data/occ_climate_df_1oct2026.csv")


#### add in the precipitation data for these months as well

prism_ppt_extract <- function(data) {
  
  results <- list()
  
  # Create the 12 climate months needed for each record
  climate_months <- data %>%
    dplyr::select(row_id, year, geometry) %>%
    dplyr::distinct(row_id, .keep_all = TRUE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      climate_date = list(
        seq.Date(
          as.Date(paste0(year - 1, "-07-01")),
          as.Date(paste0(year, "-06-01")),
          by = "month"
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(climate_date) %>%
    dplyr::mutate(
      climate_year = as.integer(format(climate_date, "%Y")),
      climate_month = as.integer(format(climate_date, "%m")),
      
      # July-Dec of previous year = prev7-prev12
      # Jan-Jun of occurrence year = 1-6
      climate_period = dplyr::if_else(
        climate_year < year,
        paste0("prev", climate_month),
        as.character(climate_month)
      ),
      
      ym = sprintf("%04d%02d", climate_year, climate_month)
    )
  
  # Only download each unique PRISM month once
  months_needed <- unique(climate_months$ym)
  
  for (this_ym in months_needed) {
    
    this_year  <- as.integer(substr(this_ym, 1, 4))
    this_month <- as.integer(substr(this_ym, 5, 6))
    
    message("Processing PRISM precipitation: ", this_ym)
    
    # PRISM 4-km AN monthly precipitation
    url <- paste0(
      "https://data.prism.oregonstate.edu/time_series/us/an/4km/ppt/monthly/",
      this_year,
      "/prism_ppt_us_25m_",
      this_ym,
      ".zip"
    )
    
    zipfile <- file.path(
      "prism_ppt",
      paste0("prism_ppt_", this_ym, ".zip")
    )
    
    # Download if not already present
    if (!file.exists(zipfile)) {
      tryCatch(
        download.file(url, zipfile, mode = "wb", quiet = TRUE),
        error = function(e) {
          warning("Could not download ", this_ym, ": ", e$message)
        }
      )
    }
    
    # Check that download worked
    if (!file.exists(zipfile) ||
        is.na(file.info(zipfile)$size) ||
        file.info(zipfile)$size < 10000) {
      warning("Could not download: ", this_ym)
      next
    }
    
    # Records needing this climate month
    d <- climate_months %>%
      dplyr::filter(ym == this_ym)
    
    # Temporary extraction folder
    tempdir <- tempfile()
    dir.create(tempdir)
    
    unzip(zipfile, exdir = tempdir)
    
    # Find the raster
    tif <- list.files(
      tempdir,
      pattern = "\\.(tif|tiff)$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    
    if (length(tif) == 0) {
      warning("No TIFF found for ", this_ym)
      unlink(tempdir, recursive = TRUE)
      next
    }
    
    r <- terra::rast(tif[1])
    
    # Extract precipitation at occurrence locations
    pts <- terra::vect(d)
    
    values <- terra::extract(r, pts)[[2]]
    
    # Store results
    results[[length(results) + 1]] <- d %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(
        ppt = values
      ) %>%
      dplyr::select(
        row_id,
        year,
        climate_period,
        climate_year,
        climate_month,
        ym,
        ppt
      )
    
    unlink(tempdir, recursive = TRUE)
  }
  
  # Combine all months
  bind_rows(results)
}

# run the function to get precip values
ppt_data <- prism_ppt_extract(data)

ppt_data_wide <- ppt_data %>% 
  pivot_wider(
    id_cols = c(row_id, year),
    names_from = climate_period,
    values_from = ppt,
    names_glue = "{.value}_m.{climate_period}"
  )

occ_climate_both <- left_join(occ_climate,ppt_data_wide,
                         by = c("row_id","year"))

occ_climate_both_df <- st_drop_geometry(occ_climate_both)


write_csv(occ_climate_both_df, "../data/occ_climate_all_df_1oct2026.csv")
