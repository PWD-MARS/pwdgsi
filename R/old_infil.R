old_infil <- function(event, #for warning messages
                                      dtime_est,
                                      rainfall_in, #for removing overlapping events
                                      dcia_ft2, #directly connected impervious area
                                      orifice_height_ft = NA, #default to NA if no orifice outlet
                                      orifice_diam_in = NA, #default to NA if no orifice outlet
                                      storage_depth_ft,
                                      storage_vol_ft3,
                                      waterlevel_ft, #observed data
                                      depth_in = 6, #depth at which to take infiltration rate
                                      discharge_coeff = 0.62 #Orifice discharge coefficient
){
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       rainfall_in,
                       depth_ft = waterlevel_ft, #observed data
                       vol_ft3 = 0,
                       #runoff_ft3 = 0,
                       slow_release_ft3 = 0)
  
  #1.2 Calculate volume
  df$vol_ft3 <- depth.to.vol(maxdepth_ft = storage_depth_ft[1],
                             maxvol_cf = storage_vol_ft3[1],
                             depth_ft = df$depth_ft)
  
  #1.3 Calculate orifice flow
  # If orifice dimensions are not provided, slow_release_ft = 0 (1.1)
  if(!is.na(orifice_diam_in[1])){
    df$slow_release_ft3 <- marsUnderdrainOutflow_cf(dtime_est,
                                                    waterlevel_ft,
                                                    orifice_height_ft,
                                                    orifice_diam_in)
    
  }
  
  #1.4 get Final depth
  last_depth <- df$depth_ft[length(df$depth_ft)]
  
  #2. Identify times associated with 5" and 7" depths
  # Note: For this approach, the time at which the depth drops below the 5" or 7" threshold is identified
  # by working backwards through the dataset. The data is first filtered to remove subsequent peaks and
  # is then again filtered by the threshold. The last row (slice(n())) represents the timestep immediately
  # before the level drops below the threshold. This value may not represent the value closest to the
  # threshold, however, this approach ensures that the values are taken from receding limbs.
  
  last_depth5 <- df %>%
    dplyr::filter(depth_ft > (depth_in - 1)/12 + last_depth) %>% #value immediately prior to water level crossing 5"
    dplyr::slice(dplyr::n()) #latest timestep
  
  last_depth7 <- df %>%
    dplyr::filter(depth_ft > (depth_in + 1)/12 + last_depth) %>% #value immediately prior to water level crossing 7"
    dplyr::slice(dplyr::n()) #latest timestep
  
  #2.4 Check that data is appropriate for calculating infiltration rate
  #2.4.1 Do observation values exist in the dataset approximately equal to 5 and 7"?
  if(nrow(last_depth5)== 0 | nrow(last_depth7) == 0){
    message(paste("Event",event[1], "does not include observation data that approximately equals", depth_in - 1, "or", depth_in + 1, "in. of water depth."))
    return(-900)
  }
  
  #2.4.2 Does the 5" measurement occur after the 7"?
  if(last_depth5$dtime_est == last_depth7$dtime_est){
    message(paste0("Code does not capture descending limb in Event ", event[1], "."))
    return(-910)
  }
  
  #2.4.3 Establish temp series. Is last depth above 5" part of the same descending limb? Does rainfall occur during recession period?
  tempseries <- df %>%
    dplyr::filter(dtime_est >= last_depth7$dtime_est & dtime_est <= last_depth5$dtime_est)
  
  #assure that the 5" depth used is within the same descending limb as the 7"
  #following the last 7" measurement, level can dip and rise back above 5". This cuts off the dip and rise
  if(min(tempseries$depth_ft) < (depth_in - 1)/12 + last_depth){
    
    #select the first point where depth drops below 5"
    cutoff <- tempseries %>%
      dplyr::filter(depth_ft < (depth_in - 1)/12 + last_depth) %>%
      dplyr::slice(1)
    
    #cut tempseries
    tempseries %<>% dplyr::filter(dtime_est < cutoff$dtime_est)
    
    #reassign last depth above 5"
    last_depth5 <- tempseries %>% dplyr::slice(dplyr::n())
  }
  
  #check this again!
  if(last_depth5$dtime_est == last_depth7$dtime_est){
    message(paste0("Code does not capture descending limb in Event ", event[1], "."))
    return(-910)
  }
  
  #2.4.4 Does significant ( > 0.05") amount of rainfall occur during the recession period between 7" and 5" (or whatever the specified range is)?
  if(sum(tempseries$rainfall_in, na.rm = TRUE) >= 0.05){
    message(paste0('Rainfall greater than 0.05 inches occurs during the recession period in Event ', event[1], '.'))
    return(-920)
  }
  
  #3. Calculate infiltration rate
  
  #3.1 Calculate total orifice flow
  #3.1.1 make sure that orifice outflow does not exceed delta V at any timestep
  #sensor noise sometimes causes delta V to go the wrong direction, so that is evened out to zero
  tempseries %<>% dplyr::mutate(slow_release_check = dplyr::case_when(is.na(dplyr::lead(vol_ft3)) ~ slow_release_ft3,
                                                                      vol_ft3 - dplyr::lead(vol_ft3) < 0 ~ 0,
                                                                      TRUE ~ pmin(slow_release_ft3,(vol_ft3 - dplyr::lead(vol_ft3)))))
  
  
  total_orifice_ft3 <- sum(tempseries$slow_release_check, na.rm = TRUE)
  
  #3.2 Calculate total change storage
  change_storage_ft3 <- tempseries$vol_ft3[1] - tempseries$vol_ft3[nrow(tempseries)] - total_orifice_ft3
  
  change_depth_in <- vol.to.depth(maxdepth_ft = storage_depth_ft,
                                  maxvol_cf = storage_vol_ft3,
                                  vol_cf = change_storage_ft3)*12
  
  #3.3 Calculate infiltration
  infiltration_rate_inhr <- round(change_depth_in/ #inches
                                    as.numeric(difftime(last_depth5$dtime_est, last_depth7$dtime_est, units = "hours")),3)
  
  if(infiltration_rate_inhr < 0.1){
    message(paste0("Infiltration rate is negligible in Event ", event[1], ".")) 
    return(-930)
  }
  
  return(round(infiltration_rate_inhr, 4))
  
}