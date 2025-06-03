old_smpSnap <- function(con, smp_id, ow_suffix, request_date){
  
  #1 Argument Validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  #1.2 check argument lengths
  if(length(smp_id) != length(ow_suffix)){
    stop("smp_id and ow_suffix must be of equal length")
  }
  
  if(length(smp_id) != length(request_date) & length(request_date) != 1){
    stop("request_date must be a single date, or equal length to smp_id and ow_suffix")
  }
  
  #1.3 Assign today() to 'today'
  request_date <- stringr::str_replace(request_date, "today", as.character(lubridate::today()))
  
  #1.4 Check if smp_id and ow_suffix combination are valid
  #1.4.1 Create dataframe
  request_df <- data.frame(smp_id, ow_suffix, request_date, stringsAsFactors = FALSE)
  
  #1.4.2 Query fieldwork.tbl_ow and check if each smp id and observation well are there
  # Initialize dataframe
  ow_validity <- data.frame(ow_uid = numeric(), 
                            smp_id =  character(),  
                            ow_suffix = character(), 
                            stringsAsFactors = FALSE)
  
  # Check if smp_id and ow_suffix are in the MARS table "fieldwork.tbl_ow"
  # Return matches
  for(i in 1:length(request_df$smp_id)){
    ow_valid_check <- odbc::dbGetQuery(con, "SELECT * FROM fieldwork.tbl_ow") %>% dplyr::select(-facility_id) %>%  dplyr::filter(smp_id == request_df$smp_id[i] & ow_suffix == request_df$ow_suffix[i])
    ow_validity <- dplyr::bind_rows(ow_validity, ow_valid_check)
  }

  # Join dates back to observation wells and ow_uids back to request criteria
  ow_validity %<>% dplyr::left_join(request_df, by = c("smp_id", "ow_suffix"))
  request_df_validity <- dplyr::left_join(request_df  %>% dplyr::select(-request_date), ow_validity, by = c("smp_id", "ow_suffix"))

  #2 Query
  #2.1 initialize dataframe
  result <- data.frame("snapshot_uid" = numeric(),
                       "ow_uid" = numeric(),
                       "well_measurements_uid" = integer(),
                       "smp_id" = character(),
                       "ow_suffix" = character(),
                       "dcia_ft2" = numeric(),
                       "storage_footprint_ft2" = numeric(),
                       "orifice_diam_in" = numeric(),
                       "infil_footprint_ft2" = numeric(),
                       "storage_depth_ft" = numeric(),
                       "lined" = logical(),
                       "surface" = logical(),
                       "storage_volume_ft3" = numeric(),
                       "infil_dsg_rate_inhr" = numeric(),
                       "orifice_lookup_uid" = integer(),
                       "orificedepth_ft" = numeric(),
                       "sumpdepth_lookup_uid" = integer(),
                       "sumpdepth_ft" = numeric(),
                       stringsAsFactors = FALSE)


  #2.2 Run get_arbitrary_snapshot in a loop and bind results
  for(i in 1:length(ow_validity$smp_id)){
    snapshot_query <- paste0("SELECT * FROM metrics.viw_snapshot_well_measurements where ow_uid in (",
                             ow_validity$ow_uid[i],") ORDER BY snapshot_uid DESC LIMIT 1")
    new_result <- odbc::dbGetQuery(con, snapshot_query)
    new_result$lined <- new_result$lined %>% as.logical()
    new_result$surface <- new_result$surface %>% as.logical()
    result <- dplyr::bind_rows(result, new_result)
  }
  # #3 Join and return
  # #3.1 Join results to request criteria
  # # snapshot_results <- request_df_validity %>% dplyr::left_join(result, by = "ow_uid") %>%
  # #                     dplyr::filter(!is.na(ow_uid))
  # # 
  # #3.2 Return results
  # return(result)
}