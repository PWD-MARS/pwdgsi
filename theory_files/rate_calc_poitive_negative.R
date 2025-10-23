# caclulating instantaneous recession rates across water
library(tidyverse)
library(RPostgres)
library(pool)
library(fpp2)           # working with time series data
library(zoo)            # working with time series data
library(patchwork)
library(data.table)


#Connect
marsDBCon <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user= Sys.getenv("admin_uid"),
  password = Sys.getenv("admin_pwd"),
  timezone = NULL)


#Slope function
FarshadSlope <- function(level_dtime, series, rain_dtime, rain_depth_in , sump_depth_ft, orrifice_elev_ft){
  #Goals: 
  #Find the instantaneous slope at each timestep there
  
  rain_df <- data.frame(rain_dtime = as.POSIXct(rain_dtime, tz = "UTC"), rain_depth_in)
  joined_df  <- data.frame(level_dtime = as.POSIXct(level_dtime, tz = "UTC"), series) %>%
    full_join(rain_df, by = c("level_dtime" = "rain_dtime"))
  
  # Create 15-min interval grid
  time_grid <- data.frame(datetime = seq(
    floor_date(min(level_dtime, na.rm = TRUE), "15 mins"),
    ceiling_date(max(level_dtime, na.rm = TRUE), "15 mins"),
    by = "15 mins"
  ))
  
  # Join with the time grid to enforce 15-min intervals ---
  result <- time_grid %>%
    left_join(joined_df, by = c("datetime" = "level_dtime"))
  
  # moving ave smoothing
  result$series_smoothed <- rollmean(result$series, k = 5, fill = NA)
  
  #Find the descending limb
  output_df <- result %>%
    mutate(difftime_hr = as.numeric(datetime - lag(datetime))/60, #15 minute steps
           difflevel_ft = series_smoothed - lag(series_smoothed),
           rawslope_inhr = (difflevel_ft / difftime_hr) * 12,
           sump_depth_ft = sump_depth_ft,
           orrifice_elev_ft = orrifice_elev_ft) %>%
    mutate(below_sump = ifelse(series < sump_depth_ft, T, F)) %>%
    mutate(below_orrifice = ifelse(series < orrifice_elev_ft, T, F))
  
  
  output_final <- output_df %>%
    select(dtime = datetime, level_ft = series, rain_depth_in, rawslope_inhr, sump_depth_ft, orrifice_elev_ft, below_sump, below_orrifice)
  return(output_final)
  
}


smp_id <- "14-1-2"
ow_uid <- "OW1"
sump_depth_ft <- 1
orrifice_elev_ft <- 0.84


sites <- dbGetQuery(marsDBCon, paste("select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow where smp_id in ('", smp_id, "') and ow_suffix = '", ow_uid, "'", sep = ""))

owdata <- dbGetQuery(marsDBCon, paste0("select ow_uid, dtime, greatest(0, level_ft) as level_ft from data.tbl_ow_leveldata_raw
    where ow_uid in (", paste(sites$ow_uid, collapse = ", "), ")"))


boundaries <- group_by(owdata, ow_uid) %>%
  summarize(n = n(),
            start = min(dtime),
            end = max(dtime)) %>%
  left_join(sites)


#Pull events
cells <- dbGetQuery(marsDBCon, paste0("select * from admin.tbl_smp_gage where smp_id in ('", 
                                      paste0(sites$smp_id, collapse = "', '"), "')"))

events <- dbGetQuery(marsDBCon, paste0("select * from data.tbl_gage_event 
    where gage_uid in (", paste(cells$gage_uid, collapse = ", "), ")")) 

rain_ts <- dbGetQuery(marsDBCon, paste0("select * from data.tbl_gage_rain 
    where gage_uid in (", paste(cells$gage_uid, collapse = ", "), ")")) %>%
  dplyr::filter(dtime >= boundaries$start & dtime <= boundaries$end)


# populate for the usage in the function
level_dtime <-owdata$dtime
series <- owdata$level_ft
rain_dtime <- rain_ts$dtime
rain_depth_in <- rain_ts$rainfall_in

# calulate rates

rates <- FarshadSlope(level_dtime, series, rain_dtime, rain_depth_in , sump_depth_ft, orrifice_elev_ft)

