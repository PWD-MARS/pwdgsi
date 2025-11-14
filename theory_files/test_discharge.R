# estimate discharge
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#DBI
library(DBI)
#tidyverse for data manipulations
library(tidyverse)
#lubridate to work with dates
library(lubridate)
# smoothing library
library(forecast)
# plot
library(ggplot2)
library(splines)
library(patchwork)




# DB connections & functions
poolConn <- dbPool(RPostgres::Postgres(),
                   host = "PWDMARSDBS1.pwd.phila.local",
                   port = 5434,
                   dbname = "mars_prod",
                   user = Sys.getenv("admin_uid"),
                   password = Sys.getenv("admin_pwd"),
                   timezone = NULL
)

# smp-id 1359-8-2 is a trench with prior deployments. The orifice sits at 12 inches above the stone with 0.5 inch diameter and storage volume: 833 ft3
# storage depth = 3.5 ft
depth.to.vol <- function(maxdepth_ft, maxvol_cf, depth_ft){
  return(maxvol_cf[1] * depth_ft/maxdepth_ft[1])
}

vol.to.depth <- function(maxdepth_ft, maxvol_cf, vol_cf){
  return(maxdepth_ft[1] * vol_cf/maxvol_cf[1])
}


marsUnderdrainOutflow_cf <- function(dtime_est, 
                                     waterlevel_ft, 
                                     orifice_height_ft,
                                     orifice_diam_in,
                                     #DEFAULT VALUES
                                     discharge_coeff = 0.62){ #Orifice discharge coefficient
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       depth_ft = waterlevel_ft)#, #observed data
  #elapsed_time_hr = 0, 
  #WL_above_orifice_ft = 0,
  #slow_release_vol_ft3 = 0) 
  
  #2. Calculate Orifice Outflow
  # Orifice equation:
  # Q_orifice = C * Area * sqrt(2 * gravity * depth) * time
  
  #2.1 Calculate area of orifice (ft2)
  orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4 #area of orifice (ft2)
  
  df <- df %>%
    dplyr:: mutate(#2.2 calculate elapsed time (hrs) 
      elapsed_time_hr = difftime(dtime_est, dplyr::lag(dtime_est), units = "hours"), #difftime(lead(dtime_est), dtime_est, units = "hours"),
      
      #2.3 Calculate height of water above orifice (ft)
      WL_above_orifice_ft = depth_ft - orifice_height_ft[1],
      
      #2.4 Set height of water to 0 if below elevation of orifice
      WL_correction = ifelse(WL_above_orifice_ft < 0,0, WL_above_orifice_ft),
      
      #2.4 Calculate total discharge through orifice
      slow_release_ft3 = discharge_coeff*
        orifice_area_ft2*
        sqrt(2 * 32.2 * WL_correction) * 
        60*60 * #convert cfs to cfhr     
        as.numeric(elapsed_time_hr))
  
  return(df$slow_release_ft3)
}

# pull water leevl data
level_data <- dbGetQuery(poolConn, "SELECT * from data.tbl_ow_leveldata_raw 
                         where ow_uid = 1738")

# single out storm on Aug 9 2024
level_df <- level_data %>% 
  dplyr::filter(dtime > as.Date("2024/08/08") & dtime < as.Date("2024/08/19")) %>%
  dplyr::distinct()
level_df_storm <- level_data %>% 
  dplyr::filter(dtime > as.Date("2024/08/08") & dtime < as.Date("2024/08/10")) %>%
  dplyr::distinct()


plot(level_df$dtime, level_df$level_ft)

# calculate infiltration rate from 1.5 ft down to 1 ft above orifice
# rate in/hr 
rate_above <- (1.5-1)*12/(4)

# calculate infiltration rate from 1 ft down to 0.1 ft above orifice
# rate in/hr 

rate_below <- (1.1-0.1)*12/(8.5*24)

# it means infiltration is very low
orrifice_discharche_byslope_inhr <- rate_above-rate_below

elapsed_time_hr <- 4
WL_above_orifice_ft <- 0.5
# estimate discharge by formula

orifice_area_ft2 <- pi*((0.5/12)^2)/4 #area of orifice (ft2)

slow_release_ft3 = 0.62*
  orifice_area_ft2*
  sqrt(2 * 32.2 * WL_above_orifice_ft) * 
  60*60 * #convert cfs to cfhr     
  as.numeric(elapsed_time_hr)

# convert back to elevation
orrifice_discharche_byformula_inhr <- (slow_release_ft3/833)*3.5*12/4


####### Smoothing tests
# smoothing using LOESS
# If it's a datetime object
level_df_clean <- level_df
level_df_clean$dtime_num <- as.numeric(level_df_clean$dtime)
fit_loess <- loess(level_df_clean$level_ft ~ level_df_clean$dtime_num) 


# Predict smoothed values
smoothed_loess <- predict(fit_loess)


# using spline method

# Convert time to numeric (if it's POSIXct)
level_df_storm$time_num <- as.numeric(level_df_storm$dtime)

# Fit a natural spline model
spline_fit <- lm(level_ft ~ ns(time_num, df = 50), data = level_df_storm)

# Predict smoothed values
level_df_storm$level_spline <- predict(spline_fit)

# alternative method
# Convert time to numeric
time_num <- as.numeric(level_df_storm$dtime)

# Fit smoothing spline
spline_model <- smooth.spline(x = time_num, y = level_df_storm$level_ft, spar = 0.3)

# can also use cross-validation for picking spar
#spline_model <- smooth.spline(x = time_num, y = level_df_storm$level_ft, cv = TRUE)

# Predict smoothed values- spline
level_df_storm$level_spline <- predict(spline_model, x = time_num)$y

# Predict smoothed values- moving ave
level_df_storm$moving_a_five_level <- rollmean(level_df_storm$level_ft, k = 5, fill = NA)

# Calculate a 3-period center-aligned rolling median
# k specifies the window width (must be an odd integer for center alignment)
level_df_storm$moving_median_three_level <- rollmedian(level_df_storm$level_ft, k = 5, fill = NA)


spline <- ggplot(level_df_storm, aes(x = dtime)) +
            geom_point(aes(y = level_ft), color = "black") +
            geom_line(aes(y = level_spline), color = "blue") +
            #geom_line(aes(y = moving_a_five_level), color = "red") +
            #geom_line(aes(y = moving_median_three_level), color = "purple") +
            labs(title = "Spline Fit for Water Level", y = "Level (ft)", x = "Time")
          
ma5 <- ggplot(level_df_storm, aes(x = dtime)) +
            geom_point(aes(y = level_ft), color = "black") +
            #geom_line(aes(y = level_spline), color = "blue") +
            geom_line(aes(y = moving_a_five_level), color = "red") +
            #geom_line(aes(y = moving_median_three_level), color = "purple") +
            labs(title = "Moving Ave of 5 Fit for Water Level", y = "Level (ft)", x = "Time")
          

mm3 <- ggplot(level_df_storm, aes(x = dtime)) +
            geom_point(aes(y = level_ft), color = "black") +
            #geom_line(aes(y = level_spline), color = "blue") +
            #geom_line(aes(y = moving_a_five_level), color = "red") +
            geom_line(aes(y = moving_median_three_level), color = "purple") +
            labs(title = "Moving Median of 3 Fit for Water Level", y = "Level (ft)", x = "Time")


combined_smooth <- spline/ma5/mm3

