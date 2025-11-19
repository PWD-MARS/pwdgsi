# caclulating Relative and Absolute Storage Utilizatin Used
# Ryan F. Ebrahimi

library(readxl)
library(tidyverse)
library(RPostgres)
library(pool)
library(fpp2) # working with time series data
library(zoo) # working with time series data
library(patchwork)
library(data.table)
library(fuzzyjoin)
library(data.table)


# Connect
conn <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user = Sys.getenv("admin_uid"),
  password = Sys.getenv("admin_pwd"),
  timezone = NULL
)

# Absolute
aspu_percent <- function(waterlevel_ft, storage_depth_ft) {
  # Defensive check: empty or all-NA input
  if (length(waterlevel_ft) == 0 || all(is.na(waterlevel_ft))) {
    return(0)
  }
  
  # Starting water level (minimum 0)
  starting_level <- 0
  
  # Maximum water level during the event
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  # Calculate event peak relative to starting level
  event_max_water_level <- max_water_level - starting_level
  max_storage <- storage_depth_ft - starting_level
  
  # Peak storage utilization (bounded between 0 and 100)
  peak_util <- (event_max_water_level / max_storage) * 100
  peak_util <- pmin(pmax(peak_util, 0), 100)
  
  return(round(peak_util, 4))
}


# Relative
rspu_percent <- function(waterlevel_ft, storage_depth_ft) {
  # Defensive check: empty or all-NA input
  if (length(waterlevel_ft) == 0 || all(is.na(waterlevel_ft))) {
    return(0)
  }
  
  # Starting water level (minimum 0)
  starting_level <- pmax(waterlevel_ft[1], 0)
  
  # Maximum water level during the event
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  # Calculate event peak relative to starting level
  event_max_water_level <- max_water_level - starting_level
  max_storage <- storage_depth_ft - starting_level
  
  # Peak storage utilization (bounded between 0 and 100)
  peak_util <- (event_max_water_level / max_storage) * 100
  peak_util <- pmin(pmax(peak_util, 0), 100)
  
  return(round(peak_util, 4))
}


