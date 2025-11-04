# caclulating instantaneous recession rates>
# recession rate function caclulates rates and assign it to rain timeseries through fkeys
# metadata function attaches sump depth, orifice depth, and rain meta data to the output of the recession rate function
library(readxl)
library(tidyverse)
library(RPostgres)
library(pool)
library(fpp2) # working with time series data
library(zoo) # working with time series data
library(patchwork)
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

# recession function calculates instantaneous slopes (both positive and negative) and joins with rain data at 15-min intervals
# 4 input arguments: DB conn, Level TS, Dtime Index, and OW UID

recession_rate <- function(ow_uid, dtime, level_ft) {
  # If the level_ft and dtime do not match return NAs
  if (length(dtime) != length(level_ft)) {
    print(paste("Level and dtime timeseries have different lengths!"))
    return(NA)
  }
  
  # both ow_uid and level must be numeric and ow_uid must be numeric
  if (!is.numeric(ow_uid) & (length(ow_uid) == 1 | length(ow_uid) == length(level_ft))) {
    print(paste("OW UID must be a numeric value!"))
    return(NA)
  }
  
  if (!is.numeric(level_ft)) {
    print(paste("Level data must be numeric!"))
    return(NA)
  }

  # join dtime and level_ft
  level_ft_df <- data.frame(ow_uid = ow_uid, dtime = as.POSIXct(dtime), level_ft = level_ft)

  # moving ave smoothing
  level_ft_df$level_ft_smoothed <- rollmean(level_ft_df$level_ft, k = 5, fill = NA)

  output_df <- level_ft_df %>%
    mutate(
      difftime_hr = as.numeric(dtime - lag(dtime)) / 60, # 15 minute steps
      difflevel_ft = level_ft_smoothed - lag(level_ft_smoothed),
      recession_rate_inhr = (difflevel_ft / difftime_hr) * 12
    ) %>%
    select(ow_uid, dtime, level_ft, recession_rate_inhr)
    
}


# meta data function to make TS regular at 15 min interval, add rainfall TS and event meta data, post rain meta data, orifice and sump values 
recession_rate_meta <- function(conn, ow_uid, dtime, level_ft, recession_rate_inhr){
  
  # If the level_ft and dtime do not match return NAs
  if (length(dtime) != length(level_ft)) {
    print(paste("Level and dtime timeseries have different lengths!"))
    return(NA)
  }
  
  # If the level_ft and recession_rate_inhr do not match return NAs
  if (length(recession_rate_inhr) != length(level_ft)) {
    print(paste("Level and recession rates timeseries have different lengths!"))
    return(NA)
  }
  
  # both ow_uid and level must be numeric and ow_uid must be numeric
  if (!is.numeric(ow_uid) & (length(ow_uid) == 1 | length(ow_uid) == length(level_ft))) {
    print(paste("OW UID must be a numeric value!"))
    return(NA)
  }
  
  if (!is.numeric(level_ft)) {
    print(paste("Level data must be numeric!"))
    return(NA)
  }
  
  if (!is.numeric(recession_rate_inhr)) {
    print(paste("Recession rate data must be numeric!"))
    return(NA)
  }
  
  # join dtime and level_ft
  level_recession_df <- data.frame(ow_uid = ow_uid, dtime = as.POSIXct(dtime), level_ft = level_ft, recession_rate_inhr = recession_rate_inhr)
  
  # filtering rain data
  boundaries <- group_by(level_recession_df, ow_uid) %>%
    summarize(
      start = min(dtime),
      end = max(dtime)
    )
  
  # Pull rain ts
  gage <- dbGetQuery(conn, paste0(
    "select * from admin.tbl_smp_gage left join fieldwork.tbl_ow using(smp_id) where ow_uid in (",
    paste0(unique(ow_uid), collapse = ", '"), ")"
  ))
  
  
  rain_ts <- dbGetQuery(conn, paste0("select * from data.viw_gage_rainfall 
    where gage_uid in (", paste(gage$gage_uid, collapse = ", "), ")")) %>%
    dplyr::filter(dtime >= boundaries$start & dtime <= boundaries$end)
  
  joined_df <- level_recession_df %>%
    left_join(rain_ts, by = c("dtime"))
  
  # Create 15-min interval grid
  time_grid <- data.frame(dtime = seq(
    floor_date(min(level_recession_df$dtime, na.rm = TRUE), "15 mins"),
    ceiling_date(max(level_recession_df$dtime, na.rm = TRUE), "15 mins"),
    by = "15 mins"
  ))
  
  # Join with the time grid to enforce 15-min intervals ---
  result <- time_grid %>%
    left_join(joined_df, by = "dtime") %>%
    select(ow_uid, dtime, level_ft, recession_rate_inhr, gage_rain_uid, gage_uid, rainfall_in, gage_event_uid) %>%
    filter(!is.na(recession_rate_inhr))
  
  return(result)
  
}
