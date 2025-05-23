old_drainDown <- function(dtime_est, rainfall_in, waterlevel_ft){
  
  #1. Process data
  #1.1 Initialize dataframe
  dtime_est <- lubridate::force_tz(dtime_est, tz = "EST")
  
  starting_level_ft <- dplyr::first(waterlevel_ft)
  
  combined_data <- tibble::tibble(
    dtime_est = dtime_est,
    rainfall_in = rainfall_in,
    waterlevel_ft  = waterlevel_ft)

  # Confirm that there was a response in the structure during the event (water level >= 0.1)
  check <- any(waterlevel_ft > starting_level_ft + 0.1)

  if(check == FALSE){
    return(-810)
  }

  #2.1 Confirm that there is a 'baseline' where water level returns after peak
  baseline_ft <- old_baseline(dtime_est = dtime_est, level_ft = waterlevel_ft)

  if(is.na(baseline_ft)){
    return(-820)
  }

  #2.2 find time at which peak occurs
  peak_time <- combined_data$dtime_est[which.max(combined_data$waterlevel_ft)]

  # #3. Filter by storage depth to get the last time above baseline 
  # # in this case we are finding the first time after peak but above baseline + 0.01, which should prevent us from capturing a flat-ish tail
  stor_end_time <- combined_data %>%
    dplyr::filter(dtime_est > peak_time) %>%
    dplyr::filter(waterlevel_ft < baseline_ft + 0.1) %>%
    dplyr::arrange(dtime_est) %>%
    dplyr::slice(1L)

  #3.1 see if there is an increase in water level during the descending limb
  increase <- combined_data %>%
    dplyr::mutate(waterlevel_ft = zoo::rollmean(waterlevel_ft, 3, fill = NA)) %>%
    dplyr::filter(dtime_est > peak_time) %>%
    dplyr::filter(dtime_est < stor_end_time$dtime_est) %>%
    dplyr::mutate(check_increase = dplyr::case_when(difftime(dplyr::lead(dtime_est, 12), dtime_est, units = "hours") == 1 &
                                                      dplyr::lead(waterlevel_ft, 12) > waterlevel_ft + 0.1 ~ TRUE,
                                                    dplyr::lead(waterlevel_ft, 4) > waterlevel_ft + 0.1 ~ TRUE,
                                                    TRUE ~ FALSE))

  if(sum(increase$check_increase == TRUE) > 2){
    return(-830)
  }


  #4. Assure that water level dropped below baseline + 0.01 (i think it has to right?)
  if(nrow(stor_end_time) > 0){
    #4.1 Calculate draindown time
    draindown_hrs <- difftime(stor_end_time$dtime_est, peak_time, units = "hours")

    #4.2 Round to 4 digits
    draindown_hrs <- round(draindown_hrs,4)

    return(as.double(draindown_hrs))

  }else{
    return(-840)

  }
  
}