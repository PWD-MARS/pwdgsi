# marsWaterLevelBaseline_ft ---------------------------------------------------
#' Water Level Baseline
#'
#' Determine the level at which water level stabilizes following draindown
#'
#' @param dtime A vector of POSIXct date times, in ascending order
#' @param level_ft Observed water level data (ft)
#' @param max_infil_rate_inhr default = 1; maximum infiltration rate at the end of the timeseries that will allow for a "baseline" to be considered
#'
#' @return Output is either the last point in the dataset, or NA, if maximum infiltration rate is exceeded within the last 75 minutes of the time series
#' @export
#'

marsWaterLevelBaseline_ft <- function(dtime, level_ft, max_infil_rate_inhr = 1){
  #### The name of the function an its purpose is a bit confusing because it's not the base line but the water level at the end of the period
  #### Shouldn't the baseline be before some period?
  
  # Find time interval between dtime[n] and dtime[n-1] 
  #### We're only checking the steps based on the last step?
  step_diff <- as.numeric(difftime(dtime[length(dtime)], dtime[length(dtime) - 1], units = "mins"))
  #### We don't do any checks for this function. Rollmean can't handle NA's which we should check for.
  #### Should we be checking the the whole interval is the same?
  if(step_diff == 5){ # 5 minute interval
    steps <- 3
    # Apply a 15-minute simple moving average
    level_ft <- zoo::rollmean(level_ft, steps, fill = NA)
  }else{ #15 minute interval
    steps <- 1
  }
  # Create dataframe of dtime and level, and discard NAs formed at the edges during the moving average
  df <- data.frame(dtime, level_ft) %>% dplyr::filter(!is.na(level_ft))
  # Round the point to 4 places
  last_point <- round(df$level_ft[length(df$level_ft)], 4)
  
  # The last 1.25 hours of the time series
  test <- df %>% 
    dplyr::mutate(lag_1 = dplyr::lag(level_ft, steps),
                  diffr = abs(level_ft - lag_1)) %>%
    dplyr::arrange(desc(dtime)) %>%
    head(5)

  # Convert max_infil_rain_inhr to ft/(15 minutes)
  depth_change <- max_infil_rate_inhr*1/12*15/60

  # Return last level_ft if the smallest of the last 1.25 hours of change in level is larger than the max_infil
  if(min(test$diffr, na.rm = TRUE) > depth_change){
    return(NA)
  }else{
    return(round(last_point, 4))
  }

}


# marsInfiltrationRate_inhr ------------------------------------------
#' Infiltration Rate 
#' 
#' Estimated infiltration rate of a system based on observed data
#' 
#' @param  event                Rainfall event ID (grouping variable)
#' @param  dtime                A vector of POSIXct date times, in ascending order
#' @param  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est (in)
#' @param  dcia_ft2             Subsurface Directly Connected Impervious Area (sf)
#' @param  orifice_height_ft    Orifice height (ft)
#' @param  orifice_diam_in      Orifice diameter (in)
#' @param  storage_depth_ft     Maximum storage depth of system (ft)
#' @param  storage_vol_ft3      Maximum storage volume (pore space) of system, in cubic feet
#' @param  waterlevel_ft        Observed water level data (ft)
#' @param  depth_in             Depth at which to calculate infiltration rate (default 6in, which creates a range of 5in to 7in)
#' @param  discharge_coeff      Orifice discharge coefficient (default 0.62)
#' 
#' @return Output is estimated infiltration rate (in/hr). These outputs are codes for the following messages: 
#'  \describe{
#'        \item{\code{-900}}{Event does not include observation data that approximately equals specified water depth}
#'        \item{\code{-910}}{Code captures rising limb in event.}
#'        \item{\code{-920}}{Rainfall occurs during recession period in specified depth range.}
#'        \item{\code{-930}}{Infiltration rate is negligible; calculated infiltration rate is less than 0.1in/hr. }
#'  } 
#' 
#' @seealso \code{\link{marsUnderdrainOutflow_cf}}
#' 
#' @export
#' 
#' @examples
#' obs_250_fill %>%
#'   dplyr::filter(is.na(event) == FALSE) %>%
#'   dplyr::group_by(event) %>%
#'   dplyr::arrange(dtime)%>%
#'   dplyr::summarize( #Calculate performance metrics
#'     #Observed infiltration rate
#'     Infiltration_Rate_inhr = marsInfiltrationRate_inhr(event, dtime,
#'                                                rainfall_in,
#'                                                dcia_ft2,
#'                                                storage_depth_ft = storage_depth_ft,
#'                                                storage_vol_ft3 = storage_vol_ft3,
#'                                                orifice_diam_in,
#'                                                orifice_height_ft,
#'                                                waterlevel_ft = level_ft))
#' 

marsInfiltrationRate_inhr <- function(event,
                                      dtime,
                                      rainfall_in,
                                      dcia_ft2,
                                      orifice_height_ft = NA,
                                      orifice_diam_in = NA,
                                      storage_depth_ft,
                                      storage_vol_ft3,
                                      waterlevel_ft,
                                      depth_in = 6, 
                                      discharge_coeff = 0.62) {
  
  # Initialize data frame
  #### There is nothing making sure these inputs are the same size to create a tibble
  df <- tibble::tibble(dtime,
                       rainfall_in,
                       # Why are we changing the name?
                       depth_ft = waterlevel_ft, 
                       # Why not calculate this first rather than have a zero?
                       vol_ft3 = 0,
                       # Why is this initialized at 0?
                       slow_release_ft3 = 0)
  
  # Calculate volume
  #### It is not a good practice to use functions with . as they are usually designated for methods
  #### Why are storage depths and volume selecting the first column or object?
  #### This depends on whether it's a vector, matrix, etc
  df$vol_ft3 <- depth.to.vol(maxdepth_ft = storage_depth_ft[1],
                                      maxvol_cf = storage_vol_ft3[1],
                                      depth_ft = df$depth_ft)
  # Calculate orifice flow
  # If orifice dimensions are not provided, slow_release_ft = 0 (1.1)
  if(!is.na(orifice_diam_in[1])){
    df$slow_release_ft3 <- marsUnderdrainOutflow_cf(dtime_est,
                                                    waterlevel_ft,
                                                    orifice_height_ft,
                                                    orifice_diam_in)
  }

  # Get final depth
  last_depth <- df$depth_ft[length(df$depth_ft)]
  
  # Identify times associated with 5" and 7" depths
  # Note: For this approach, the time at which the depth drops below the 5" or 7" threshold is identified
  # by working backwards through the dataset. The data is first filtered to remove subsequent peaks and
  # is then again filtered by the threshold. The last row (slice(n())) represents the timestep immediately
  # before the level drops below the threshold. This value may not represent the value closest to the
  # threshold, however, this approach ensures that the values are taken from receding limbs.
  #### Why are we doing it this way? We can easily pull the prior value with accuracy.

  last_depth5 <- df %>%
    dplyr::filter(depth_ft > (depth_in - 1)/12 + last_depth) %>% #value immediately prior to water level crossing 5"
    dplyr::slice(dplyr::n()) #latest timestep

  last_depth7 <- df %>%
  dplyr::filter(depth_ft > (depth_in + 1)/12 + last_depth) %>% #value immediately prior to water level crossing 7"
  dplyr::slice(dplyr::n()) #latest timestep

  # Check that data is appropriate for calculating infiltration rate
  if(nrow(last_depth5)== 0 | nrow(last_depth7) == 0){
    message(paste("Event",event[1], "does not include observation data that approximately equals", depth_in - 1, "or", depth_in + 1, "in. of water depth."))
    return(-900)
  }

  # Does the 5" measurement occur after the 7"?
  #### Does this do what we think it does?
  if(last_depth5$dtime == last_depth7$dtime){
    message(paste0("Code does not capture descending limb in Event ", event[1], "."))
    return(-910)
  }

  # Establish temp series. Is last depth above 5" part of the same descending limb? Does rainfall occur during recession period?
  tempseries <- df %>%
    # Filter results where the timestamp is >= 7" and <= 5?
    dplyr::filter(dtime >= last_depth7$dtime & dtime <= last_depth5$dtime)
  # Assure that the 5" depth used is within the same descending limb as the 7"
  # The following the last 7" measurement, level can dip and rise back above 5". This cuts off the dip and rise
  ##### Is this the same as <last_depth5$dtime?
  ##### Not going to check this for now.
  if(min(tempseries$depth_ft) < (depth_in - 1)/12 + last_depth){
    
    # Select the first point where depth drops below 5"
    cutoff <- tempseries %>%
      dplyr::filter(depth_ft < (depth_in - 1)/12 + last_depth) %>%
      dplyr::slice(1)
    # cut tempseries
    tempseries %<>% dplyr::filter(dtime < cutoff$dtime)
    # reassign last depth above 5"
    last_depth5 <- tempseries %>% dplyr::slice(dplyr::n())
  }
  #check this again!
  #### Why?
  if(last_depth5$dtime == last_depth7$dtime){
    message(paste0("Code does not capture descending limb in Event ", event[1], "."))
    return(-910)
  }
  
  # Does significant ( > 0.05") amount of rainfall occur during the recession period between 7" and 5" (or whatever the specified range is)?
  if(sum(tempseries$rainfall_in, na.rm = TRUE) >= 0.05){
    message(paste0('Rainfall greater than 0.05 inches occurs during the recession period in Event ', event[1], '.'))
    return(-920)
  }

  # Calculate infiltration rate

  # Calculate total orifice flow
  # Make sure that orifice outflow does not exceed delta V at any timestep
  #sensor noise sometimes causes delta V to go the wrong direction, so that is evened out to zero
  tempseries %<>% 
    dplyr::mutate(slow_release_check = dplyr::case_when(is.na(dplyr::lead(vol_ft3)) ~ slow_release_ft3,
                                                        vol_ft3 - dplyr::lead(vol_ft3) < 0 ~ 0,
                                                        TRUE ~ pmin(slow_release_ft3,(vol_ft3 - dplyr::lead(vol_ft3)))))


  total_orifice_ft3 <- sum(tempseries$slow_release_check, na.rm = TRUE)

  # Calculate total change storage
  change_storage_ft3 <- tempseries$vol_ft3[1] - tempseries$vol_ft3[nrow(tempseries)] - total_orifice_ft3
  
  change_depth_in <- vol.to.depth(maxdepth_ft = storage_depth_ft,
                                  maxvol_cf = storage_vol_ft3,
                                  vol_cf = change_storage_ft3)*12

  # Calculate infiltration
  infiltration_rate_inhr <- round(change_depth_in/ #inches
                                    as.numeric(difftime(last_depth5$dtime, last_depth7$dtime, units = "hours")), 3)

  if(infiltration_rate_inhr < 0.1){
    message(paste0("Infiltration rate is negligible in Event ", event[1], "."))
    return(-930)
  }
  
  infiltration_rate_inhr
  
}

# Observed Orifice Outflow Volume -------------------------------------------
#Description of the arguments:

#IN:  dtime           A vector of POSIXct date times, in ascending order
#IN:  waterlevel_ft        Observed water level data, in feet
#IN:  orifice_height_ft    Orifice height, in feet
#IN:  orifice_diam_in      Orifice diameter, in inches
#IN:  discharge_coeff      Orifice discharge coefficient

#OUT: Total observed orifice outflow volume, in cubic feet

#' Observed Orifice Outflow Volume
#' 
#' Total observed orifice outflow volume (cf)
#' 
#' @param  dtime            A vector of POSIXct date times, in ascending order
#' @param  waterlevel_ft        Observed water level data (ft)
#' @param  orifice_height_ft    Orifice height (ft)
#' @param  orifice_diam_in      Orifice diameter (in)
#' @param  discharge_coeff      Orifice discharge coefficient
#' 
#' @return Output is total observed orifice outflow volume (cf) 
#' 
#' @seealso \code{\link{marsInfiltrationRate_inhr}}
#' 
#' @export
#' 
#' @examples
#' obs_250_fill <- obs_250_all %>%  
#' dplyr::arrange(dtime_est) %>%
#'   tidyr::fill(event) %>% #Fill NA's
#'   dplyr::mutate( # Pull in system specs from smp_stats table    
#'     storage_depth_ft = smp_stats$storage_depth_ft[7], 
#'     storage_vol_ft3 = smp_stats$storage_vol_ft3[7],
#'     infil_footprint_ft2 = smp_stats$infil_footprint_ft2[7],
#'     dcia_ft2 =  smp_stats$dcia_ft2[7],
#'     orifice_height_ft = smp_stats$orifice_height_ft[7],
#'     orifice_diam_in = smp_stats$orifice_diam_in[7],
#'     
#'     # Calculate orifice flow, if applicable
#'    orifice_vol_cf = marsUnderdrainOutflow_cf(dtime_est,
#'                                         waterlevel_ft = level_ft,
#'                                         orifice_height_ft,
#'                                         orifice_diam_in)
#'   )
#' 
#' 

marsUnderdrainOutflow_cf <- function(dtime, 
                                     waterlevel_ft, 
                                     orifice_height_ft,
                                     orifice_diam_in,
                                     discharge_coeff = 0.62){
  
  # Prepare data
  # Initialize data frame
  df <- tibble::tibble(dtime = dtime,
                       # Why do we change the name?
                       depth_ft = waterlevel_ft)
  
  # Calculate Orifice Outflow
  # Q_orifice = C * Area * sqrt(2 * gravity * depth) * time
  
  # Calculate area of orifice (ft2)
  orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4 #area of orifice (ft2)
  
  df <- df %>%
    dplyr:: mutate(#2.2 calculate elapsed time (hrs) 
      elapsed_time_hr = difftime(dtime, dplyr::lag(dtime), units = "hours"),
      # Calculate height of water above orifice (ft)
      WL_above_orifice_ft = depth_ft - orifice_height_ft[1],
      
      # Set height of water to 0 if below elevation of orifice
      WL_correction = ifelse(WL_above_orifice_ft < 0,0, WL_above_orifice_ft),
      
      # Calculate total discharge through orifice
      slow_release_ft3 = discharge_coeff*
        orifice_area_ft2*
        sqrt(2 * 32.2 * WL_correction) *
        #convert cfs to cfhr  
        60*60 *    
        as.numeric(elapsed_time_hr))
  
  
  return(df$slow_release_ft3)
}

# Depth - Volume Conversions ----------------------
# Functions taken directly from Rmarkdown file "GSI_Data_Analysis_EWRILIDFork_6AprVersion_180612_RDM.Rmd"
# NOTE: The following comment was in script: "QA RECOMMENDED TO CONFIRM WE ARE HANDLING STAGE-STORAGE RELATIONSHIP AS INTENDED"
# Laurie and Katie discussed this with Andrew Baldridge and Dwayne Myers and confirmed that the functions convert correctly - 3/22/2019

#' Return Volume or Depth
#' Input maximum depth and maximum volume of SMP, along with depth or volume, to return the volume or depth. 
#'
#' @name depth.volume
NULL

#' @rdname depth.volume
#' 
#' @param maxdepth_ft depth of SMP when full, (ft)
#' @param maxvol_cf volume of SMP when full (cf)
#' @param vol_cf input or output volume (cf)
#' @param depth_ft input or output depth (cf)
#' 
#' @return Output is either volume (cf) or depth (ft)
#' 
#' @export

vol.to.depth <- function(maxdepth_ft, maxvol_cf, vol_cf){
  return(maxdepth_ft[1] * vol_cf/maxvol_cf[1])
}

#' @rdname depth.volume
#' 
#' @export

depth.to.vol <- function(maxdepth_ft, maxvol_cf, depth_ft){
  return(maxvol_cf[1] * depth_ft/maxdepth_ft[1])
}

# Overtopping Check ----------------------------------

#Description of the arguments:

#IN:  waterlevel_ft      Water level, in feet
#IN:  storage_depth_ft   Maximum storage depth, in feet

#OUT: T/F designation based on overtopping evaluation

#' Simulation Stats
#' 
#' Summarize and Evaluate System Performance
#' 
#' @name simulation.stats
NULL

#' @rdname simulation.stats
#' 
#' @param waterlevel_ft Water Level (ft)
#' @param storage_depth_ft Total storage Depth (ft)
#' 
#' @return \describe{
#'      \item{\code{marsOvertoppingCheck_bool}}{Output is TRUE or FALSE based on overtopping evaluation}
#'      \item{\code{-710}}{No Water Level were provided for evaluation}
#'      \item{\code{-720}}{Non-numeric argument was provided for either storage_depth_ft}
#' }
#' 
#' @examples
#' 
#' simulation_summary <- simulated_data %>%
#'   dplyr::group_by(event) %>%
#'   dplyr::summarize(startdate = min(dtime_est), #add start date of event to summary table,
#'                   
#'    #1. Overtopping check
#'    overtopping = marsOvertoppingCheck_bool(Simulated_depth_ft,smp_stats$storage_depth_ft[7]),
#' 
#'    #2. Simulated storage utilization
#'    peakUtilization = marsPeakStorage_percent(Simulated_depth_ft,smp_stats$storage_depth_ft[7]),
#' 
#'    #3. Peak release rate
#'    peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, Simulated_orifice_vol_ft3),
#' 
#'    #4. Total orifice outflow volume (rounded for table format)
#'    orifice_volume_ft3 = round(sum(Simulated_orifice_vol_ft3),0),
#' 
#'    #5. Draindown time
#'    draindown_time_hr = marsDraindown_hr(dtime_est, rainfall_in, Simulated_depth_ft))
#'    
#' @export



marsOvertoppingCheck_bool <- function(waterlevel_ft, storage_depth_ft){
  
  #1. Make sure data exist and are valid for use
  
  # remove NA's
  waterlevel_ft <- waterlevel_ft[!is.na(waterlevel_ft)]

  # return error code if no data are available
  #### These should throw an error seeing how we expect it to return T/F
  if(length(waterlevel_ft) == 0){
    return(-710)
  }
  
  if(!is.numeric(storage_depth_ft)){
    return(-720)
  }
  
  #2. Pull max water level
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  #3. Compare to max structure storage
  #### Why are these in quotes?
  #### Should there be a margin of error?
  check <- ifelse(max_water_level < storage_depth_ft, "FALSE", "TRUE")
  
  return(check)
}

# Peak Storage ------------------------------------
#Description of the arguments:

#IN:  waterlevel_ft      Water level, in feet
#IN:  storage_depth_ft   Maximum storage depth, in feet

#OUT: Peak Storage Utilization Percentage

# Peak Storage Utilization Percentage
# 
# Percentage of peak storage filled

#' @rdname simulation.stats
#' 
#' @return \describe{
#'      \item{\code{marsPeakStorage_percent}}{Output is a percentage of peak storage filled, by depth}
#' }
#' 
#' @export

marsPeakStorage_percent <- function(waterlevel_ft, storage_depth_ft){
  
  #1. Pull starting water level
  starting_level <- ifelse(waterlevel_ft[1] < 0, 0, waterlevel_ft[1])
  
  #2. Pull max water level
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  #3. Apply correction for starting water levels
  event_max_water_level <- max_water_level - starting_level
  
  max_storage <- storage_depth_ft - starting_level
  
  #3. Calculate Peak Storage Utilization
  peak_util <- (event_max_water_level/max_storage)*100
  
  #4. Apply correction for overtopping
  peak_util <- ifelse(peak_util > 100, 100, peak_util)
  
  #5. Set to zero if negative
  peak_util <- ifelse(peak_util < 0, 0, peak_util)
  
  return(round(peak_util, 4))
}


# Peak Release Rate -----------------------------------------------
#' @rdname simulation.stats
#' 
#' @param dtime A vector of POSIXct date times, in ascending order
#' @param orifice_outflow_ft3 Orifice outflow volume (cf)
#' 
#' @return \describe{
#'      \item{\code{marsPeakReleaseRate_cfs}}{Output is peak orifice release rate (cfs)}
#' }
#' 
#' @export


marsPeakReleaseRate_cfs <- function(dtime,
                                    orifice_outflow_ft3){
  #### No checks to make sure the vectors are same size
  
  # Prepare data
  # Initialize data frame
  df <- tibble::tibble(dtime,
                       #### Why do we change the name here?
                       orifice_ft3 = orifice_outflow_ft3) 
  
  # Calculate timestep and pull maximum value
  df_max <- df %>%
    dplyr::mutate(elapsed_time_hr = difftime(dplyr::lead(dtime), dtime, units = "hours")) %>%
    dplyr::filter(is.na(orifice_ft3) == FALSE) %>%
    dplyr::arrange(orifice_ft3) %>%
    dplyr::slice(dplyr::n()) #pull row containing max orifice volume

  # Calculate peak rate or return NA if there is no max outflow
  #### Why are we waiting until this point to determine this?
  if(nrow(df_max) == 0){
    rate <- NA
  } else {
    #### This breaks if the largest outflow is at the end of the series
    #### The last value is going to be NA because it's elapsed and you can't divide by NA
    rate <- df_max$orifice_ft3/
      as.numeric(df_max$elapsed_time_hr*60*60) #hr converted to seconds
  }
  # Round to 4 digits
  rate <- round(rate, 4)
}


# Draindown ---------------------------------------------------------------
#' @rdname simulation.stats
#' 
#' @param dtime A vector of POSIXct date times, in ascending order
#' @param rainfall_in Rainfall depths during periods corresponding to times in  dtime (in)
#' @param waterlevel_ft Vector of water levels that corresponds to dtime vector
#'
#' @return \describe{
#'      \item{\code{marsDraindown_hr}}{Output is Calculated Draindown time (hr). Returns \code{NA} if water level does not return to the starting level after event.}
#' \describe{
#'        \item{\code{marsDraindown_hr}}{Output is Calculated Draindown time (hr).Returns the following outputs as coders for these error messages:}
#'        \item{\code{-810}}{Water level does not respond to rain event}
#'        \item{\code{-820}}{Water level does not return to a stable baseline}
#'        \item{\code{-830}}{There is an increase during the descending limb that pushes the draindown time more than one hour}
#'        \item{\code{-840}}{Water level did not drop below baseline}
#'  } 
#'  }
#' 
#' @export
#' 

marsDraindown_hr <- function(dtime, rainfall_in, waterlevel_ft){
  #### We should probably check to make sure dtime is in ascending order.
  # Process data
  starting_level_ft <- dplyr::first(waterlevel_ft)
  # Put data into a tibble
  #### We may just want to have a gettter function return these based on a dtime range
  combined_data <- tibble::tibble(
    dtime = dtime,
    rainfall_in = rainfall_in,
    waterlevel_ft  = waterlevel_ft)
  
  # Confirm that there was a response in the structure during the event (water level >= 0.1)
  check <- any(waterlevel_ft > starting_level_ft + 0.1)  

  if(check == FALSE){
    return(-810)
  }

  # Confirm that there is a 'baseline' where water level returns after peak
  #### The baseline returned from the baseline function is determined by the last value of the series.
  #### I'm not sure how this would help.
  baseline_ft <- marsWaterLevelBaseline_ft(dtime = dtime, level_ft = waterlevel_ft)

  if(is.na(baseline_ft)){
    return(-820)
  }

  # Find time at which peak occurs
  peak_time <- combined_data$dtime[which.max(combined_data$waterlevel_ft)]

  # Filter by storage depth to get the last time above baseline
  # in this case we are finding the first time after peak but above baseline + 0.01, which should prevent us from capturing a flat-ish tail
  stor_end_time <- combined_data %>%
    # Filter obs where dtime is after the peak time
    dplyr::filter(dtime > peak_time) %>%
    # Filter where water_level is lower than the baseline + 0.1
    dplyr::filter(waterlevel_ft < baseline_ft + 0.1) %>%
    dplyr::arrange(dtime) %>%
    dplyr::slice(1L)

  # See if there is an increase in water level during the descending limb
  increase <- combined_data %>%
    # Find the rolling mean of waterlevel
    dplyr::mutate(waterlevel_ft = zoo::rollmean(waterlevel_ft, 3, fill = NA)) %>%
    # Filter obs after peak time
    dplyr::filter(dtime > peak_time) %>%
    # Filter obs before the second to last level
   dplyr::filter(dtime < stor_end_time$dtime) %>%
   dplyr::mutate(check_increase = dplyr::case_when(difftime(dplyr::lead(dtime, 12), dtime, units = "hours") == 1 &
                                                     dplyr::lead(waterlevel_ft, 12) > waterlevel_ft + 0.1 ~ TRUE,
                                                   dplyr::lead(waterlevel_ft, 4) > waterlevel_ft + 0.1 ~ TRUE,
                                                   TRUE ~ FALSE))

  if(sum(increase$check_increase == TRUE) > 2){
    return(-830)
  }


  # Assure that water level dropped below baseline + 0.01 (i think it has to right?)
  #### If the baseline is the last value in a timeseries, then this should always be the baseline
  #### Shouldn't this be if it drops below baseline + 0.1 based on the the code above?
  if(nrow(stor_end_time) > 0){
    #4.1 Calculate draindown time
    draindown_hrs <- difftime(stor_end_time$dtime, peak_time, units = "hours")

    #4.2 Round to 4 digits
    draindown_hrs <- round(draindown_hrs,4)

    return(as.double(draindown_hrs))

  }else{
    return(-840)

  }
  
}

# marsDraindownAssement ------------------------------------------
#' Draindown Assessment
#' 
#' Assess Draindown Rate based on storm size
#' 
#' @param  level_ft             Observed water level data (ft)
#' @param  eventdepth_in        Rainfall event depth (in)
#' @param  designdepth_in       Depth that the SMP is designed to managed (in)
#' @param  storage_depth_ft     Maximum storage depth of system (ft)
#' @param  draindown_hr         Draindown calculated by marsDraindown_hr (hr)
#' @param  subsurface           If the SMP is subsurface, TRUE. If surface, FALSE.
#' 
#' @return Output is a number based on draindown time and draindown time is rain event was scaled to the design depth. Outputs are codes for the following messages: 
#'  \describe{
#'        \item{\code{5}}{Draindown is below the target duration, and scaled draindown is below the target duration.}
#'        \item{\code{4}}{Draindown is above the target duration, but scaled draindown is below the target duration.}
#'        \item{\code{3}}{Draindown is below the target duration, but scaled draindown is above the target duration.}
#'        \item{\code{2}}{Draindown is above the target duration, and scaled draindown is above the target duration.}
#'        \item{\code{1}}{Draindown was not calculated.}
#'  } 
#'  
#' @export
#'  

marsDraindownAssessment <- function(level_ft, eventdepth_in, designdepth_in, storage_depth_ft, draindown_hr, subsurface = c(TRUE, FALSE)){
  
  #this is a function to assess draindown based on relationship between storm size and design storm size
  #if there is a draindown error code, return 0.
  if(draindown_hr < 0){
    assessment <- 1
  }else{
    
    #define a target draindown time
    #72 hours for subsurface, 24 hours for surface
    if(subsurface == TRUE){
      target_hr <- 72
    }else if(subsurface == FALSE){
      target_hr <- 24
    }
    
    #initial level
    initial_level_ft <- level_ft %>% 
      dplyr::first()
    
    #peak level
    peak_level_ft <- level_ft %>% 
      max() 
    
    #height of ascending limb
    ascend_ft <- peak_level_ft - initial_level_ft
    
    #ratio of design storm size to real storm size
    storm_size_ratio <- designdepth_in/eventdepth_in
    
    #height of scaled ascending limb based on a linear relationship to the storm size
    #max it out at the storage depth
    scaled_ascend_ft <- min(storm_size_ratio*ascend_ft, storage_depth_ft)
    
    #ratio of scaled peak to real peak
    peak_ratio <- scaled_ascend_ft/ascend_ft
    
    #scaled draindown time based on linear relationship to peak
    scaled_draindown_hr <- peak_ratio*draindown_hr
    
    #compare real draindown to target
    real_draindown_complies <- draindown_hr <= target_hr
    
    #compare scaled draindown to target
    scaled_draindown_complies <- scaled_draindown_hr <= target_hr
    
    #set values 
    if(real_draindown_complies & scaled_draindown_complies){
      assessment <- 5
    }else if(scaled_draindown_complies & real_draindown_complies == FALSE){
      assessment <- 4
    }else if(scaled_draindown_complies == FALSE & real_draindown_complies == TRUE){
      assessment <- 3
    }else if(scaled_draindown_complies == FALSE & real_draindown_complies == FALSE){
      assessment <- 2
    }
    
    return(assessment)
    
  }
}



