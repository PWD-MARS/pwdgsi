old_peakRelease <- function(dtime_est,
                                    orifice_outflow_ft3){
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       orifice_ft3 = orifice_outflow_ft3) 
  
  #2. Calculate timestep and pull maximum value
  df_max <- df %>%
    dplyr::mutate(elapsed_time_hr = difftime(dplyr::lead(dtime_est), dtime_est, units = "hours")) %>%
    dplyr::filter(is.na(orifice_ft3) == FALSE) %>%
    dplyr::arrange(orifice_ft3) %>%
    dplyr::slice(dplyr::n()) #pull row containing max orifice volume
  
  #3. Calculate peak rate
  #3.1 Check that outflow data is not NA
  if(nrow(df_max) == 0){
    rate <- NA
  }else{

    #3.2 Calculate rate
    rate <- "test"
    rate <- df_max$orifice_ft3#/
      as.numeric(df_max$elapsed_time_hr*60*60) #hr converted to seconds
  }
  rate

  #4. Round to 4 digits
  rate <- round(rate, 4)

  return(rate)
}