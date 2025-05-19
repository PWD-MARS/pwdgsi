<<<<<<< HEAD
old_events <- function(con, target_id, source = c("gage", "radar"), start_date, end_date){
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
  # # making this "EST"
  # events %<>% dplyr::mutate(eventdatastart_est = lubridate::force_tz(eventdatastart_edt,"EST"))
  # events %<>% dplyr::mutate(eventdataend_est = lubridate::force_tz(eventdataend_edt,"EST"))
  # events %<>% dplyr::select(-eventdatastart_edt,
  #                           -eventdataend_edt)
  # 
  # #3 return event data
  # return(events)
=======
old_events <- function(dtime_est, rainfall_in, 
                             #DEFAULT VALUES
                             iet_hr = 6, mindepth_in = 0.10) {
  
  # 1. QC checks
  # 1.1 Check for non-zero and negative rainfall values
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0.")
  }
  
  # 1.2 Check that datetime is in ascending order
  if(!identical(order(dtime_est), 1:length(dtime_est))) {
    stop("Datetime data is not sorted in ascending order.")
  }
  
  # 1.3 Check for duplicated data
  if(!all(!duplicated(dtime_est))) {
    stop("Datetime data cannot contain duplicates.")
  }
  
  # 1.4 Check that datetime is in correct format
  if(!(class(dtime_est)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }
  
  # 1.5 Check to make sure paired data matches
  if(!(length(dtime_est) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }
  
  # Assumed interval
  interval_sec <- 15 * 60
  
  # 2. Process rainfall data
  prepseries <- tibble::tibble(dtime = dtime_est,
                               rf_in = rainfall_in) %>%
    dplyr::mutate(lag_time = dplyr::lag(dtime, 1, default = dplyr::first(dtime) - interval_sec)) %>%
    dplyr::mutate(gap_hr = difftime(dtime, lag_time, units = "hours"))
  
  min_interval <- min(prepseries$gap_hr, na.rm = TRUE)
  
  # 3. Identify events
  
  # 3.1 Initialize column
  prepseries$start <- 0
  
  # 3.2 Check whether first measurement in row 1 is included in following event
  prepseries$start[1] <- ifelse(prepseries$gap_hr[2] < iet_hr, 1, 0)
  
  # 3.3 Identify beginning of new events
  prepseries$start[prepseries$gap_hr >= iet_hr + min_interval] <- 1
  
  # 3.4 Generate series of new events
  prepseries <- prepseries %>%
    dplyr::mutate(event = cumsum(start))
  
  # 3.5 Identify events that are less than the min_depth
  prepsums <- prepseries %>%
    dplyr::group_by(event) %>%
    dplyr::summarize(total_in = sum(rf_in)) %>%
    dplyr::filter(total_in >= mindepth_in-0.0000001) %>%
    # note: subtracted from minimum threshold because spot check indicated that
    # some events at the threshold were not being included in the event
    # detection (but not all). Probably a floating point issue.
    dplyr::mutate(event_id = 1:dplyr::n()) # all events that are greater than the min depth
  
  # 3.6 Join event summary to rainfall data
  output <- prepseries %>%
    dplyr::left_join(prepsums, by = "event") %>%
    dplyr::select(dtime_est = dtime,
                  rainfall_in = rf_in,
                  event_id)
  
  return(output$event_id)
>>>>>>> 82503c4e81fae4af26c9b0e06c24bcdd13ed1770
}