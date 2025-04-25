##### Any reason why target_id is not smp_id?
edit_fetchRain <- function(con, target_id, source = c("gage", "radar"), start_date, end_date, DLS = TRUE){
  if (isTRUE(DLS)) {
    tz <- "America/New_York"
  } else{
    tz <- "Etc/GMT-5"
  }
  # Check if there is a connection
  if(!DBI::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  # Make sure start and end dates are able to be saved as yyyy-mm-dd (returns with TZ)
  dates <- c(start_date = check_date(start_date, tz), end_date = check_date(end_date, tz))
  if (length(dates) != 2) {
    rlang::abort("Please use yyyy-mm-dd for date formats")
  }
  
  # Create query table based on gage/radar
  if (stringr::str_detect(source, "^g(a|au)ge")) {
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", 
                             raintable = "data.viw_gage_rainfall", 
                             uidvar = "gage_uid", 
                             loctable = "admin.tbl_gage", 
                             eventuidvar = "gage_event_uid", 
                             stringsAsFactors=FALSE)
  } else if (source == "radar") {
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar",
                             raintable = "data.viw_radar_rainfall",
                             uidvar = "radar_uid", 
                             loctable = "admin.tbl_radar", 
                             eventuidvar = "radar_event_uid", 
                             stringsAsFactors=FALSE)
  } else {
    rlang::abort("Please supply a source value of 'gage' or 'radar'")
  }
  # Get closest rainfall source
  smp_query <- paste0("SELECT * FROM ", rainparams$smptable)
  rainsource <- DBI::dbGetQuery(con, smp_query) %>% dplyr::filter(smp_id == target_id) %>% dplyr::pull(rainparams$uidvar)
  # Get rain data
  rain_query <- sprintf("SELECT *, dtime_edt::varchar as dtime FROM %s WHERE %s = %s AND dtime_edt BETWEEN %s AND %s",
                        rainparams$raintable,
                        rainparams$uidvar,
                        paste("'", rainsource, "'", sep = ""),
                        paste("'", dates["start_date"], "'", sep = ""),
                        paste("'", dates["end_date"], "'", sep = "")
  )
  # gage_rain_uid, dtime_edt (posixct), gage_uid, rainfall_in, gage_event_uid, dtime
  rain_temp <- DBI::dbGetQuery(con, rain_query)
  # # Stop if there is no rain for this timeframe
  # if (nrow(rain_temp) == 0) {
  #   rlang::abort("There is no data in the database for this date range.")
  # }
  
  # Make dtime_edt edt again (delete once datetime data in DB is stored as ETD)
  rain_temp <- rain_temp |> 
    dplyr::mutate(dtime_est = lubridate::force_tz(dtime_edt, tz)) |> 
    dplyr::select(gage_rain_uid, gage_uid, rainfall_in, gage_event_uid, dtime_est)
  rain_temp
  
  # #Punctuate data with zeroes to prevent linear interpolation when plotting
  # #If the time between data points A and B is greater than 15 minutes (the normal timestep), we must insert a zero 15 minutes after A
  # #If it's greather than 30 minutes, we must insert a zero 15 minutes before B also
  # 
  # #First, create data frame to contain zero fills with same column names as our rain data
  # zeroFills <- rain_temp[0,]
  # #print("Begin zero-filling process")
  # for(i in 1:(nrow(rain_temp) - 1)){
  #   # k <- difftime(rain_temp$dtime_edt[i+1], rain_temp$dtime_edt[i], units = "min")
  #   k <- difftime(rain_temp$dtime_est[i+1], rain_temp$dtime_est[i], units = "min")    
  #   
  #   #If gap is > 15 mins, put a zero 15 minutes after the gap starts
  #   if(k > 15){
  #     
  #     
  #     zeroFillIndex <- nrow(zeroFills)+1
  #     
  #     #Boundaries of the interval to be zeroed
  #     boundary.low <- rain_temp$dtime_est[i]
  #     boundary.high <- rain_temp$dtime_est[i+1]
  #     # boundary.low <- rain_temp$dtime_edt[i]
  #     # boundary.high <- rain_temp$dtime_edt[i+1]
  #     
  #     #The zero goes 15 minutes (900 seconds) after the first boundary
  #     #Filled by index because R is weird about partially filled data frame rows
  #     fill <- boundary.low + lubridate::seconds(900)
  #     #these were causing several functions to crash. Need a way to index so this doesn't happen again.
  #     zeroFills[zeroFillIndex, 5] <- fill                   #dtime_edt
  #     zeroFills[zeroFillIndex, 3] <- 0                      #rainfall_in
  #     zeroFills[zeroFillIndex, 2] <- rainsource   #gage_uid or radarcell_uid
  #     # browser()
  #     # print(paste("Gap-filling event ID. Before:", rain_temp$event[i], "After:", rain_temp$event[i+1]))
  #     zeroFills[zeroFillIndex, 5] <- marsGapFillEventID(event_low = rain_temp[i, 5], event_high = rain_temp[i+1, 5]) #event
  #     
  #     #If the boundary is longer than 30 minutes, we need a second zero
  #     if(k > 30){
  #       
  #       #This zero goes 15 minutes before the upper boundary
  #       fill <- boundary.high - lubridate::seconds(900)
  #       zeroFills[zeroFillIndex + 1, 2] <- fill                   #dtime_edt
  #       zeroFills[zeroFillIndex + 1, 4] <- 0                      #rainfall_in
  #       zeroFills[zeroFillIndex + 1, 3] <- rainsource   #gage_uid or radarcell_uid
  #       
  #       #print(paste("Gap-filling event ID. Before:", rain_temp[i, 5], "After:", rain_temp[i+1, 5]))
  #       zeroFills[zeroFillIndex + 1, 5] <- marsGapFillEventID(event_low = rain_temp[i, 5], event_high = rain_temp[i+1, 5]) #event
  #       
  #     }
  #     
  #   }
  # }
  # 
  # #Replace UIDs with SMP IDs
  # rainlocs <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$loctable))
  # # finalseries <- dplyr::bind_rows(rain_temp, zeroFills) %>%
  # #   dplyr::left_join(rainlocs, by = rainparams$uidvar) %>%
  # #   dplyr::select(dtime_edt, rainfall_in, rainparams$uidvar, rainparams$eventuidvar) %>%
  # #   dplyr::arrange(dtime_edt)
  # finalseries <- dplyr::bind_rows(rain_temp, zeroFills) %>%
  #   dplyr::left_join(rainlocs, by = rainparams$uidvar) %>%
  #   dplyr::select(dtime_est, rainfall_in, rainparams$uidvar, rainparams$eventuidvar) %>%
  #   dplyr::arrange(dtime_est)
  # 
  # #round date to nearest minute
  # finalseries$dtime_est %<>% lubridate::round_date("minute")
  # 
  # #Rename dtime column if we are undoing daylight savings time
  # # if(daylightsavings == FALSE){
  # #   finalseries <- finalseries %>%
  # #     dplyr::mutate(dtime_est = dtime_edt) %>%
  # #     dplyr::select(-dtime_edt)
  # #   finalseries <- dplyr::select(finalseries, dtime_est, rainfall_in, rainparams$uidvar, rainparams$eventuidvar)
  # # }
  # 
  # return(finalseries)
}
