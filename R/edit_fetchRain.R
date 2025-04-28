edit_fetchRain <- function(con, target_id, source = c("gage", "radar"), start_date, end_date){
  # Check if there is a connection
  if(!DBI::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  # Make sure start and end dates are able to be saved as yyyy-mm-dd (returns with TZ)
  dates <- c(start_date = check_date(start_date), end_date = check_date(end_date))
  # length(dates) = number of valid dates
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
  # Get rain data
  #### This is two steps that should be 1. We don't need to pull the whole SMP table to match it.
  smp_query <- paste0("SELECT * FROM ", rainparams$smptable)
  rainsource <- DBI::dbGetQuery(con, smp_query) %>% dplyr::filter(smp_id == target_id) %>% dplyr::pull(rainparams$uidvar)
  # Build rain query
  rain_query <- sprintf("SELECT *, dtime_edt as dtime FROM %s WHERE %s = %s AND dtime_edt BETWEEN %s AND %s",
                        rainparams$raintable,
                        rainparams$uidvar,
                        paste("'", rainsource, "'", sep = ""),
                        paste("'", dates["start_date"], "'", sep = ""),
                        paste("'", dates["end_date"], "'", sep = "")
  )
  # Returns rain data based on the SMP and start/end date
  # gage_rain_uid, dtime_edt (posixct), gage_uid, rainfall_in, gage_event_uid, dtime
  rain_data <- DBI::dbGetQuery(con, rain_query)
  # Stop if there is no rain for this timeframe
  if (nrow(rain_data) == 0) {
    rlang::abort("There is no data in the database for this date range.")
  }
  
  # Make dtime_edt either EST or EDT based on DST argument. (delete once datetime data in DB is stored as EDT)
  rain_data <- rain_data |> 
    dplyr::mutate(dtime = lubridate::force_tz(dtime, "America/New_York")) |>
    dplyr::select(dtime, rainfall_in, gage_uid, gage_event_uid) |>
    dplyr::arrange(dtime)
}
