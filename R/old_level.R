old_fetch <- function(con, target_id, ow_suffix, start_date, end_date, sump_correct){
  
  #1 Argument Validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  
  #1.2 Check if smp_id and ow_suffix are in the MARS table "ow_validity"
  # Return match
  validity_query <- paste0("select * from fieldwork.fun_get_ow_uid('",target_id,"','",ow_suffix,"', NULL)")
  ow_uid <- odbc::dbGetQuery(con, validity_query)
  
  # #1.3 check if data exists in viw will have a sump. Change sump_correct to FALSE if necessary
  # 
  # 
  # #1.4 Pick which table to query
  # if(stringr::str_replace(ow_suffix, ".$", "") %in% c("CW", "GW", "PZ")){
  #   level_table <- "data.tbl_gw_depthdata_raw"
  # }else if(!(stringr::str_replace(ow_suffix, ".$", "") %in% c("CW", "GW", "PZ")) & sump_correct == TRUE){
  #   level_table <- "data.viw_ow_leveldata_sumpcorrected"
  # }else if(!(stringr::str_replace(ow_suffix, ".$", "") %in% c("CW", "GW", "PZ")) & sump_correct == FALSE){
  #   level_table <- "data.tbl_ow_leveldata_raw"
  # }
  # start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  # end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  # 
  # #1.5 Add buffer to requested dates
  # start_date <- lubridate::round_date(start_date) - lubridate::days(1)
  # end_date <- lubridate::round_date(end_date) + lubridate::days(1)
  # 
  # #2 Query database for level data
  # leveldata_query <- paste0("select * from ", level_table, "
  #                               WHERE ow_uid = '", ow_uid, "'
  #                               AND dtime_est BETWEEN '",start_date,"' AND '", end_date, "'")
  # 
  # leveldata <- odbc::dbGetQuery(con, leveldata_query) %>% dplyr::arrange(dtime_est)
  # 
  # leveldata$dtime_est %<>% lubridate::force_tz(tz = "EST") %>% lubridate::round_date("minute")
  # 
  # #3 Return level data
  # return(leveldata)
}