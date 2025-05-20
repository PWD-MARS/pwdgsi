old_monitoring <- function(con, target_id, ow_suffix, source = c("gage", "radar"), start_date, end_date,
                                    sump_correct = TRUE, rain_events = TRUE, rainfall = TRUE, level = TRUE, daylight_savings = FALSE,
                                    debug = FALSE){
  
  #1 Argument validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage","radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage/gauge' or 'radar'")
  }
  
  #Are we working with gages or radarcells?
  if(source == "gage" | source == "gauge"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", eventtable = "data.tbl_gage_event", uidvar = "gage_uid", loctable = "admin.tbl_gage", eventuidvar = "gage_event_uid", stringsAsFactors=FALSE)
  } else if(source == "radar"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar", eventtable = "data.tbl_radar_event", uidvar = "radar_uid", loctable = "admin.tbl_radar", eventuidvar = "radar_event_uid", stringsAsFactors=FALSE)
  } else { #Parameter is somehow invalid
    stop("Argument 'source' is not one of 'gage' or 'radar'")
  }
  
  #2 Initialize list
  results <- list()
  
  #Get closest gage
  
  if(debug){
    ptm <- proc.time()
  }
  
  smp_rain <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$smptable)) %>% dplyr::filter(smp_id %in% target_id)
  ow_validity <- odbc::dbGetQuery(con, "SELECT * FROM fieldwork.tbl_ow")
  ow_uid_gage <- ow_validity %>% dplyr::right_join(smp_rain, by = "smp_id")
  
  if(debug){
    print(paste0(source, "_lookup_time: ", (proc.time()-ptm)[3]))
  }
  
  #Set datetime date types
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  if(debug){
    ptm <- print(paste("date_time conversion time:", (proc.time()-ptm)[3]))
  }
  
  #3 Add rain events
  if(rain_events == TRUE){
    for(i in 1:length(target_id)){
      results[["Rain Event Data step"]] <- old_fetchEvents(con, target_id[i], source, start_date[i], end_date[i])
      start_date[i] <- ifelse(nrow(results$`Rain Event Data step`) > 1,
                              min(results[["Rain Event Data step"]]$eventdatastart_est),
                              start_date[i])
      end_date[i] <-  ifelse(nrow(results$`Rain Event Data step`) > 1,
                             max(results[["Rain Event Data step"]]$eventdatastart_est),
                             end_date[i])
      results[["Rain Event Data"]] <- dplyr::bind_rows(results[["Rain Event Data"]], results[["Rain Event Data step"]])
      results[["Rain Event Data step"]] <- NULL
    }
  }


  if(debug){
    ptm <- proc.time()
  }


  #4 Add Rainfall
  if(rainfall == TRUE){
    for(i in 1:length(target_id)){
      results[["Rainfall Data step"]] <- old_rain(con, target_id[i], source, start_date[i], end_date[i], daylight_savings)
      start_date[i] <- min(results[["Rainfall Data step"]]$dtime_est - lubridate::days(1), na.rm = TRUE)
      end_date[i] <- max(results[["Rainfall Data step"]]$dtime_est + lubridate::days(1), na.rm = TRUE)
      results[["Rainfall Data"]] <- dplyr::bind_rows(results[["Rainfall Data"]], results[["Rainfall Data step"]])
      results[["Rainfall Data step"]] <- NULL
    }
  }
  #####

  if(debug){
    print(paste("rainfall_lookup_time:", (proc.time()-ptm)[3]))
  }

  if(debug){
    ptm <- proc.time()
  }

  #5 Add level data
  if(level == TRUE){

    for(i in 1:length(target_id)){
      results[["Level Data step"]] <- old_level(con, target_id[i], ow_suffix[i], start_date[i], end_date[i], sump_correct) %>%
        dplyr::left_join(ow_uid_gage, by = "ow_uid") %>%  #join rain gage uid
        dplyr::select(dtime_est, level_ft, ow_uid, rainparams$uidvar) #remove extra columns
      if(rain_events == TRUE){
        level_data_step <- results[["Level Data step"]] #coerce to data frame

        results_event_data <- results[["Rain Event Data"]]

        level_data_step <- level_data_step[(!is.na(level_data_step$dtime_est)),]

        #select relevant columns from the results
        results_event_data %<>% dplyr::select(rainparams$eventuidvar, rainparams$uidvar, eventdatastart_est)

        #join by gage uid and by start time, to give a rainfall gage event uid at the start of each event
        level_data_step %<>% dplyr::left_join(results_event_data,
                                              by = c(rainparams$uidvar, "dtime_est" = "eventdatastart_est"))

        #carry event uids forward from event start to start of next event
        level_data_step[[rainparams$eventuidvar]] %<>% zoo::na.locf(na.rm = FALSE)

        #isolate event data needed for assuring that the rainfall gage event uid isn't assigned too far past the event end
        event_data <- results[["Rain Event Data"]]  %>%
          dplyr::select(rainparams$eventuidvar, eventdataend_est)

        #browser()

        #join event end times to level data by event uid
        #check that any dtime that has that event uid does not exceed the end time by greater than three days
        #if it does, reassign NA to event uid
        level_data_step %<>% dplyr::left_join(event_data, by = rainparams$eventuidvar) %>%
          dplyr::mutate(new_event_uid = dplyr::case_when(dtime_est >= (eventdataend_est + lubridate::days(4)) ~ NA_integer_,
                                                         TRUE ~ level_data_step[[rainparams$eventuidvar]])) %>%
          dplyr::select(-!!rainparams$eventuidvar, -eventdataend_est) %>%
          dplyr::rename(!!rainparams$eventuidvar := new_event_uid)


        results[["Level Data step"]] <- NULL
        results[["Level Data step"]] <- level_data_step
      }

      results[["Level Data"]] <- dplyr::bind_rows(results[["Level Data"]], results[["Level Data step"]])
      results[["Level Data step"]] <- NULL
    }
  }

  if(debug){
    print(paste("level_lookup_time:", (proc.time()-ptm)[3]))
  }

  if(debug){
    ptm <- proc.time()
  }



# remove incomplete events from level/rainfall/rain event data
  if(rain_events == TRUE & rainfall == TRUE & level == TRUE){
    test_df_id <- dplyr::full_join(results[["Rainfall Data"]], results[["Level Data"]], by = c("dtime_est", rainparams$eventuidvar, rainparams$uidvar)) %>% #join
      dplyr::arrange(dtime_est) %>%
      dplyr::filter(!is.na(rainparams$eventuidvar) & is.na(level_ft)) %>%  #filter events that are not NA and and water level that is not NA
      dplyr::pull(rainparams$eventuidvar)
  }
  
    results[["Level Data"]] %<>% dplyr::filter(!(rainparams$eventuidvar %in% test_df_id))
  
    # !!sym syntax comes from here: https://stackoverflow.com/questions/49786597/r-dplyr-filter-with-a-dynamic-variable-name
    # Because our variable name is a string, we need to make it evaluate as an R symbol instead of the string
    # choked during recent deployment, going to attempt to use rlang::sym() instead
  
    #filter out rain data for events that do have corresponding water level data
    results[["Rainfall Data"]] %<>% dplyr::filter((!!rlang::sym(rainparams$eventuidvar) %in% results[["Level Data"]][, rainparams$eventuidvar]))
  
    #fiter out rain events that no longer have corresponding rainfall data
    results[["Rain Event Data"]] %<>% dplyr::filter((!!rlang::sym(rainparams$eventuidvar) %in% results[["Rainfall Data"]][, rainparams$eventuidvar]))
  
    if(debug){
      print(paste("filtering_time:", (proc.time()-ptm)[3]))
    }
  

return(results)
}
