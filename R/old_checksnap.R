old_checksnap <- function(con, smp_id, ow_suffix, request_date){
  
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
                       "dcia_ft2" = numeric(),
                       "storage_footprint_ft2" = numeric(), 
                       "orifice_diam_in" = numeric(),
                       "infil_footprint_ft2" = numeric(),
                       "storage_depth_ft" = numeric(),
                       "lined" = character(), # make logical later 
                       "surface" = character(),  # make logical later 
                       "storage_volume_ft3" = numeric(),
                       "infil_dsg_rate_inhr" = numeric(),
                       stringsAsFactors = FALSE)
  
  
  #2.2 Run get_arbitrary_snapshot in a loop and bind results
  for(i in 1:length(ow_validity$smp_id)){
    snapshot_query <- paste0("SELECT * FROM metrics.fun_get_arbitrary_snapshot('", ow_validity$smp_id[i], "','", ow_validity$ow_suffix[i], "','", ow_validity$request_date[i], "') ORDER BY snapshot_uid DESC LIMIT 1")
    new_result <- odbc::dbGetQuery(con, snapshot_query)
    result <- dplyr::bind_rows(result, new_result)
  }
  
  #3 Are the snapshots we have equal to the current GreenIT/PlanReview values!?
  
  #3.0.5 Where are we looking?
  table_query <- paste0('select smp_id, \'viw_planreview_crosstab_snapshot\' as location from external.tbl_planreview_crosstab
                         where smp_id IN (\'',paste(ow_validity$smp_id, collapse = "', '"),'\')  
                         UNION
                         select smp_id, \'viw_greenit_unified\' as location from external.viw_greenit_unified
                         where  smp_id IN (\'',paste(ow_validity$smp_id, collapse = "', '"),'\')')
  
  check_table <- odbc::dbGetQuery(con, table_query)
  
  
  #3.1 Let's look at the view greenit unified/planreview crosstab
  current_values <- data.frame("ow_uid" = numeric(),
                               "dcia_ft2" = numeric(),
                               "storage_footprint_ft2" = numeric(), 
                               "orifice_diam_in" = numeric(),
                               "infil_footprint_ft2" = numeric(),
                               "storage_depth_ft" = numeric(),
                               "lined" = character(), # make logical later 
                               "surface" = character(),  # make logical later 
                               "storage_volume_ft3" = numeric(),
                               "infil_dsg_rate_inhr" = numeric(),
                               stringsAsFactors = FALSE)
  
  # change the query based on the smp chosen
  for(i in 1:nrow(check_table)){
    current_values_query <- paste0("SELECT * FROM external.", check_table$location[i]," where smp_id IN ('",check_table$smp_id[i],"')")
    current_value <- odbc::dbGetQuery(con, current_values_query)
    current_values <- dplyr::bind_rows(current_values, current_value)
  }
  
  
  #3.2 Now let's compare between the two for differences
  comp_fields <- c("ow_uid", "dcia_ft2", "storage_footprint_ft2",
                   "orifice_diam_in", "infil_footprint_ft2", "storage_depth_ft",
                   "lined", "surface", "storage_volume_ft3", "infil_dsg_rate_inhr")
  
  comp_values <- current_values %>% dplyr::select(all_of(comp_fields)) %>% arrange(ow_uid)
  comp_result <- result %>% dplyr::select(all_of(comp_fields)) %>% arrange(ow_uid)
  
  # let's compare
  for(i in length(comp_result$ow_uid)){
    result_hash <- digest::digest(comp_result[i,], algo = "md5")
    value_hash <- digest::digest(comp_values[i,], algo = "md5")  
    
    if(result_hash != value_hash){
      
      smp_id_x <- ow_validity$smp_id[ow_validity$ow_uid == comp_result$ow_uid[i]]
      suffix_x <- ow_validity$ow_suffix[ow_validity$ow_uid == comp_result$ow_uid[i]]
      
      #new snapshot entry dbSendQuery, complete tonight/tomorrow
      update_query <- paste0("select * from metrics.fun_insert_snapshot('",smp_id_x,"', '",suffix_x,"')")
      dbGetQuery(con, update_query)
    }
    
  }
  
  
  #4 Join results to original request df
  #4.1 not.na function
  not.na <- function(x){
    !is.na(x)
  }
  
  #4.2 turn it into a bool
  result_bool <- request_df_validity %>% dplyr::left_join(result, by = "ow_uid") %>%
    dplyr::select(snapshot_uid) %>% not.na() %>% as.vector
  
  return(result_bool)
  
}