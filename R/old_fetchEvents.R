old_fetchEvents <-  function(con, target_id, source = c("gage", "radar"), start_date, end_date){
  #1 Argument validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  #Sanitize start and end date
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage","radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  #Are we working with gages or radarcells?
  if(source == "gage" | source == "gauge"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", eventtable = "data.tbl_gage_event", uidvar = "gage_uid", loctable = "admin.tbl_gage", eventuidvar = "gage_event_uid", stringsAsFactors=FALSE)
  } else if(source == "radar"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar", eventtable = "data.tbl_radar_event", uidvar = "radar_uid", loctable = "admin.tbl_radar", eventuidvar = "radar_event_uid", stringsAsFactors=FALSE)
  } else { #Parameter is somehow invalid
    stop("Argument 'source' is not one of 'gage' or 'radar'")
  }
  
  
  #2 Get closest rain source
  rainsource <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$smptable)) %>% 
    dplyr::filter(smp_id == target_id) %>%
    dplyr::pull(rainparams$uidvar)
  
  #2.1 Query event data
  event_query <- paste(paste0("SELECT * FROM ", rainparams$eventtable),
                       "WHERE", rainparams$uidvar, "= CAST('", rainsource, "' as int)",
                       "AND eventdatastart_edt >= Date('", start_date, "')",
                       "AND eventdataend_edt <= Date('", end_date + lubridate::days(1), "');")
  
  events <- odbc::dbGetQuery(con, event_query) 
  # making this "EST"
  events %<>% dplyr::mutate(eventdatastart_est = lubridate::force_tz(eventdatastart_edt,"EST"))
  events %<>% dplyr::mutate(eventdataend_est = lubridate::force_tz(eventdataend_edt,"EST"))
  events %<>% dplyr::select(-eventdatastart_edt,
                            -eventdataend_edt)
  
  #3 return event data
  return(events)
}