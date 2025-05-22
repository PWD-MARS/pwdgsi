old_simlevelSeries <- function(dtime_est,
                                        rainfall_in,
                                        event,
                                        infil_footprint_ft2, #footprint of the SMP that is open to infiltration
                                        dcia_ft2, #directly connected impervious area
                                        orifice_height_ft = NA, #default to NA if no orifice outlet
                                        orifice_diam_in = NA, #default to NA if no orifice outlet
                                        storage_depth_ft,
                                        storage_vol_ft3,
                                        infil_rate_inhr = 0.06, #New default based on Compliance guidance
                                        #default values
                                        initial_water_level_ft = 0, 
                                        runoff_coeff = 1, #rational method coefficient
                                        discharge_coeff = 0.62 #Orifice discharge coefficient
){ 
  
  #Data Validation 
  if(is.na(storage_depth_ft)| is.na(dcia_ft2) | is.na(storage_vol_ft3)){
    stop("storage_depth_ft, dcia_ft2, or storage_volume_ft3 is NA")
  }
  
  if(storage_depth_ft == 0 | dcia_ft2 == 0 | storage_vol_ft3 == 0){
    stop("storage_depth_ft, dcia_ft2, or storage_volume_ft3 equals 0")
  }
  
  if(is.na(infil_rate_inhr) & is.na(orifice_diam_in)){
    stop("both infil_rate_inhr and orifice_diam_in equal NA")
  }
  
  if(infil_rate_inhr == 0 & is.na(orifice_diam_in)){
    stop("infil_rate_inhr is 0 and orifice_diam_in is NA")
  }
  
  if((infil_footprint_ft2 == 0 | is.na(infil_footprint_ft2)) & is.na(orifice_diam_in)){
    stop("infil_footprint_ft2 is 0 or NA, and orifice_diam_in is NA")
  }
  
  if(infil_rate_inhr == 0){
    infil_rate_inhr <- 0.06 #Overload existing snapshots
  }
  
  # browser()
  
  
  
  #Prepare data
  #Initialize data frames
  collected_data <- data.frame(dtime_est, rainfall_in, event)
  
  if(length(initial_water_level_ft) > 1){
    events <- unique(event)
    events <- events[!is.na(events)]
    events_initial <- data.frame(events, initial_water_level_ft)
    collected_data <- collected_data %>% dplyr::left_join(events_initial, by = c("event" = "events"))
  }
  
  simseries_total <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                                    rainfall_in = 0, 
                                    event = 0,
                                    depth_ft = 0, 
                                    vol_ft3 = 0,
                                    runoff_ft3 = 0,
                                    slow_release_ft3 = 0,
                                    infiltration_ft3 = 0, #POTENTIAL infiltration
                                    end_vol_ft3 = 0) 
  simseries_total <- simseries_total[0,]
  unique_events <- unique(collected_data$event) #vector of unique event IDs
  unique_events <- unique_events[!is.na(unique_events)]
  infil_rate_inhr[is.na(infil_rate_inhr)] <- 0
  
  #if statements
  orifice_if <- is.na(orifice_diam_in[1]) == FALSE
  if(orifice_if){
    orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4
  }
  
  #split data into list of dataframes, one for each event
  collected_data <- split(collected_data, collected_data$event)
  
  #apply the function "sim_loop" to each dataframe in the list "collected data". This is in lieu of a for loop
  simseries_total <- lapply(collected_data, sim_loop, debug, simseries_total, infil_rate_inhr, orifice_if, orifice_area_ft2, infil_footprint_ft2, dcia_ft2, orifice_height_ft, orifice_diam_in, storage_depth_ft, storage_vol_ft3, initial_water_level_ft, runoff_coeff, discharge_coeff)
  
  #take the output of the lapply, a list of dataframes, and bind rows into one dataframe
  simseries_total <- dplyr::bind_rows(simseries_total)
  
  #Series returns a data frame including water depth #may be updated
  simseries_total <- simseries_total %>% dplyr::select("dtime_est", "rainfall_in", "event", "depth_ft", "vol_ft3", "slow_release_ft3")
  simseries_total$dtime_est %<>% lubridate::force_tz("EST")
  colnames(simseries_total) <- c("dtime_est", 
                                 "rainfall_in", 
                                 "radar_event_uid", 
                                 "simulated_depth_ft", 
                                 "simulated_vol_ft3", 
                                 "simulated_orifice_vol_ft3")
  
  return(simseries_total)
  
}
