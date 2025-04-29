#' Fetch rainfall data
#'
#' @param conn Connection to database
#' @param smp_id SMP ID for the desired rainfall data
#' @param source Source of the rainfall data: Gage or Radar
#' @param start_date Start of the rainfall period (yyyy-mm-dd format)
#' @param end_date End of the rainfall period (yyyy-mm-dd format)
#'
#' @return Data frame of rainfall data between the start and end date
#' @export
#' 
#' @examples
#' \dontrun{rain_data <- fetchRain(conn, "1267-2-1, gage, "2024-01-31", "2024-03-31")}
#' 
fetchRain <- function(conn, smp_id, source = c("gage", "radar"), start_date, end_date){
  # Check if there is a connection
  if(!DBI::dbIsValid(conn)){
    stop("Argument 'conn' is not an open ODBC channel")
  }
  
  # Make sure start and end dates are able to be saved as yyyy-mm-dd (returns with tz)
  dates <- c(start_date = check_date(start_date), end_date = check_date(end_date))
  
  # length(dates) = number of valid dates
  if (length(dates) != 2) {
    rlang::abort("Please use yyyy-mm-dd for date formats")
  }
  
  # Create query table based on gage/radar
  if (stringr::str_detect(source, "^g(a|au)ge")) {
    rainparams <- data.frame(smp_table = "admin.tbl_smp_gage", 
                             rain_table = "data.viw_gage_rainfall", 
                             uid = "gage_uid", 
                             event_uid = "gage_event_uid", 
                             stringsAsFactors=FALSE)
  } else if (source == "radar") {
    rainparams <- data.frame(smp_table = "admin.tbl_smp_radar",
                             rain_table = "data.viw_radar_rainfall",
                             uid = "radar_uid", 
                             event_uid = "radar_event_uid", 
                             stringsAsFactors=FALSE)
  } else {
    rlang::abort("Please supply a source value of 'gage' or 'radar'")
  }
  
  # Get rain data
  rainsource_q <- sprintf("SELECT %s FROM %s WHERE smp_id = '%s'",
                          rainparams$uid,
                          rainparams$smp_table,
                          smp_id)
  rainsource <- DBI::dbGetQuery(conn, rainsource_q)
  # # Build rain query
  rain_query <- sprintf("SELECT *, dtime_edt as dtime FROM %s WHERE %s = %s AND dtime_edt BETWEEN %s AND %s",
                        rainparams$rain_table,
                        rainparams$uid,
                        paste("'", rainsource, "'", sep = ""),
                        paste("'", dates["start_date"], "'", sep = ""),
                        paste("'", dates["end_date"], "'", sep = "")
  )
  # Returns rain data based on the SMP and start/end date
  # gage_rain_uid, dtime_edt (posixct), gage_uid, rainfall_in, gage_event_uid, dtime
  rain_data <- DBI::dbGetQuery(conn, rain_query)
  # Stop if there is no rain for this timeframe
  if (nrow(rain_data) == 0) {
    rlang::abort("There is no data in the database for this date range.")
  }

  # Make dtime_edt either EST or EDT based on DST argument. (delete once datetime data in DB is stored as EDT)
  rain_data <- rain_data |>
    dplyr::mutate(dtime = lubridate::force_tz(dtime, "America/New_York")) |>
    dplyr::select(dtime, rainfall_in, rainparams$uid, rainparams$event_uid) |>
    dplyr::arrange(dtime)
}
