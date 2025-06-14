old_rain <- function(con, target_id, source = c("gage", "radar"), start_date, end_date, daylightsavings){
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  # browser()
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage","radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  #Are we working with gages or radarcells?
  if(source == "gage" | source == "gauge"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", raintable = "data.viw_gage_rainfall", uidvar = "gage_uid", loctable = "admin.tbl_gage", eventuidvar = "gage_event_uid", stringsAsFactors=FALSE)
  } else if(source == "radar"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar", raintable = "data.viw_radar_rainfall", uidvar = "radar_uid", loctable = "admin.tbl_radar", eventuidvar = "radar_event_uid", stringsAsFactors=FALSE)
  } else { #Parameter is somehow invalid
    stop("Argument 'source' is not one of 'gage' or 'radar'")
  }
  
  #Get closest rainfall source
  smp_query <- paste0("SELECT * FROM ", rainparams$smptable)
  #print(smp_query)
  rainsource <- odbc::dbGetQuery(con, smp_query) %>% dplyr::filter(smp_id == target_id) %>% dplyr::pull(rainparams$uidvar)
  
  #Collect gage data
  #First, get all the relevant data from the closest gage
  rain_query <- paste(paste0("SELECT *, dtime_edt::varchar as dtime FROM ", rainparams$raintable, " "),
                      paste0("WHERE ", rainparams$uidvar, " = CAST('", rainsource, "' as int)"),
                      "AND dtime_edt >= Date('", start_date, "')",
                      "AND dtime_edt <= Date('", end_date + lubridate::days(1), "');")

  # print(rain_query)
  rain_temp <- odbc::dbGetQuery(con, rain_query)
  

  # print(rain_query)
  rain_temp <- odbc::dbGetQuery(con, rain_query)

  if(nrow(rain_temp) == 0){

    if(lubridate::month(start_date) == lubridate::month(lubridate::today())){
      stop(paste("Rainfall data appears in the MARS database on about a 5 week delay. \nData for", lubridate::month(start_date, label = TRUE, abbr = FALSE), "should be available in the second week of", lubridate::month(lubridate::today() + lubridate::dmonths(1), label = TRUE, abbr = FALSE)))
    }
    stop("There is no data in the database for this date range.")
  }
  #
  rain_temp$rainfall_in %<>% as.numeric
  rain_temp
 # rain_temp$dtime_edt %<>% lubridate::ymd_hms(tz = "America/New_York")
  # we need to reformat dates at midnight because ymd_hms does not know how to handle them
  # this is actually a base R issue, but it is still dumb
  # https://github.com/tidyverse/lubridate/issues/1124

rain_temp %<>% dplyr::mutate(dtime_est = lubridate::ymd_hms(dtime, tz = "EST")) %>% dplyr::select(-dtime_edt, -dtime)

  #Apparently, attempting to set the time zone on a datetime that falls squarely on the spring forward datetime
  #Such as 2005-04-03 02:00:00
  #Returns NA, because the time is impossible.
  #I hate this so, so much
  #To mitigate this, we will strip NA values from the new object
  # rain_temp %<>% dplyr::filter(!is.na(dtime_edt)) %>% dplyr::arrange(dtime_edt)
  rain_temp %<>% dplyr::filter(!is.na(dtime_est)) %>% dplyr::arrange(dtime_est)

  #Our water level data is not corrected for daylight savings time. ie it doesn't spring forwards
  #So we must shift back any datetimes within the DST window
  #Thankfully, the dst() function returns TRUE if a dtime is within that zone
  # if(daylightsavings == FALSE){
  #   dst_index <- lubridate::dst(rain_temp$dtime_edt)
  #   rain_temp$dtime_edt %<>% lubridate::force_tz("EST") #Assign new TZ without changing dates
  #   rain_temp$dtime_edt[dst_index] <- rain_temp$dtime_edt[dst_index] - lubridate::hours(1)
  # }
  # 
  # #Punctuate data with zeroes to prevent linear interpolation when plotting
  # #If the time between data points A and B is greater than 15 minutes (the normal timestep), we must insert a zero 15 minutes after A
  # #If it's greather than 30 minutes, we must insert a zero 15 minutes before B also
  # 
  # #First, create data frame to contain zero fills with same column names as our rain data
  zeroFills <- rain_temp[0,]
  #print("Begin zero-filling process")
  for(i in 1:(nrow(rain_temp) - 1)){
    # k <- difftime(rain_temp$dtime_edt[i+1], rain_temp$dtime_edt[i], units = "min")
    k <- difftime(rain_temp$dtime_est[i+1], rain_temp$dtime_est[i], units = "min")

    #If gap is > 15 mins, put a zero 15 minutes after the gap starts
    if(k > 15){


      zeroFillIndex <- nrow(zeroFills)+1

      #Boundaries of the interval to be zeroed
      boundary.low <- rain_temp$dtime_est[i]
      boundary.high <- rain_temp$dtime_est[i+1]
      # boundary.low <- rain_temp$dtime_edt[i]
      # boundary.high <- rain_temp$dtime_edt[i+1]

      #The zero goes 15 minutes (900 seconds) after the first boundary
      #Filled by index because R is weird about partially filled data frame rows
      fill <- boundary.low + lubridate::seconds(900)
      #these were causing several functions to crash. Need a way to index so this doesn't happen again.
      zeroFills[zeroFillIndex, 5] <- fill                   #dtime_edt
      zeroFills[zeroFillIndex, 3] <- 0                      #rainfall_in
      zeroFills[zeroFillIndex, 2] <- rainsource   #gage_uid or radarcell_uid
      # browser()
      # print(paste("Gap-filling event ID. Before:", rain_temp$event[i], "After:", rain_temp$event[i+1]))
      zeroFills[zeroFillIndex, 5] <- marsGapFillEventID(event_low = rain_temp[i, 5], event_high = rain_temp[i+1, 5]) #event

      #If the boundary is longer than 30 minutes, we need a second zero
      if(k > 30){

        #This zero goes 15 minutes before the upper boundary
        fill <- boundary.high - lubridate::seconds(900)
        zeroFills[zeroFillIndex + 1, 2] <- fill                   #dtime_edt
        zeroFills[zeroFillIndex + 1, 4] <- 0                      #rainfall_in
        zeroFills[zeroFillIndex + 1, 3] <- rainsource   #gage_uid or radarcell_uid

        #print(paste("Gap-filling event ID. Before:", rain_temp[i, 5], "After:", rain_temp[i+1, 5]))
        zeroFills[zeroFillIndex + 1, 5] <- marsGapFillEventID(event_low = rain_temp[i, 5], event_high = rain_temp[i+1, 5]) #event

      }

    }
  }
  # 
  # #Replace UIDs with SMP IDs
  # rainlocs <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$loctable))
  # finalseries <- dplyr::bind_rows(rain_temp, zeroFills) %>%
  #   dplyr::left_join(rainlocs, by = rainparams$uidvar) %>%
  #   dplyr::select(dtime_edt, rainfall_in, rainparams$uidvar, rainparams$eventuidvar) %>%
  #   dplyr::arrange(dtime_edt)
  # finalseries <- dplyr::bind_rows(rain_temp, zeroFills) %>%
  #   dplyr::left_join(rainlocs, by = rainparams$uidvar) %>%
  #   dplyr::select(dtime_est, rainfall_in, rainparams$uidvar, rainparams$eventuidvar) %>%
  #   dplyr::arrange(dtime_est)

  # #round date to nearest minute
  rain_temp$dtime_est %<>% lubridate::round_date("minute")
  rain_temp
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
