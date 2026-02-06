baro <- function(con, target_id, start_date, end_date, data_interval = c("5 mins", "15 mins")){
  if (!odbc::dbIsValid(con)) {
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  
  # Convert start/end to of type date
  start_date <- lubridate::ymd(start_date, tz = "America/New_York")
  end_date <- lubridate::ymd(end_date, tz = "America/New_York")

  # Get SMP and baro sensor locations
  smp_loc <- DBI::dbGetQuery(con, "SELECT * FROM admin.tbl_smp_loc")
  locus_loc <- smp_loc |> dplyr::filter(smp_id == target_id)
  baro_smp <- DBI::dbGetQuery(con, "SELECT DISTINCT smp_id FROM admin.tbl_baro_rawfile;") %>% dplyr::pull(smp_id)

  # Get baro data
  baro <- DBI::dbGetQuery(con, paste0("SELECT * FROM data.viw_barodata_smp WHERE dtime >= '", start_date, "'", " AND dtime <= '", end_date + lubridate::days(1), "' order by dtime"))
  #### This doesn't appear to be used ever
  # baro_latest_valid <- DBI::dbGetQuery(con, paste0("SELECT max(dtime) FROM data.viw_barodata_neighbors WHERE neighbors >= 4 and dtime < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()

  # If no results, return baro_latest_dtime
  if (length(baro$dtime) == 0) {
    baro_latest_dtime <- DBI::dbGetQuery(con, paste0("SELECT max(dtime) FROM data.tbl_baro WHERE dtime < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()
    stop (paste0("No data available in the reqested interval. The latest available baro data is from ", baro_latest_dtime, "."))
  }

  # Check if dtime has seconds
  needs_thickening <- baro$dtime |>
    lubridate::second() |> {. > 0} |> any() == TRUE
  
  # 
  if (needs_thickening == TRUE) {
    baro <- baro |>
      padr::thicken(interval = "5 mins", rounding = "down") %>%
      dplyr::group_by(dtime_5_min, smp_id) %>%
      dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>%
      dplyr::select(dtime = dtime_5_min, smp_id, baro_psi) %>%
      dplyr::ungroup()
  } else {
    baro %<>% dplyr::group_by(dtime, smp_id) %>%
      dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE))#%>%
      # dplyr::select(dtime, smp_id, baro_psi) %>%
      # dplyr::ungroup()
  }
  # 
  # baro$dtime %<>% lubridate::with_tz(tz = "America/New_York")
  # 
  # #initialize countNAs_t in case the loop doesn't run. It is passed as a param to markdown so it needs to exist.
  # countNAs_t <- 0
  # 
  # #When the user requests data at a 5-minute resolution, we need to stretch our 15-minute data into 5-minute data
  # #We can use tidyr::spread and padr::pad to generate the full 5 minute time series,
  # #And then use zoo::na.locf (last observation carried forward) to fill the NAs with the most recent value
  # if(data_interval == "5 mins"){
  #   
  #   #Spread data to have all baro measurements use the same dtime_est column
  #   #So we can pad every 15-minute time series at once
  #   baro <- tidyr::spread(baro, "smp_id", "baro_psi")
  #   
  #   #Pad installs 5 minute intervals in our 15 minute dtime_est column. All other columns become NA
  #   #End value is 10 minutes after the final period because that 15 minute data point is good for 10 more minutes
  #   baro_pad <- padr::pad(baro, start_val = min(baro$dtime), end_val = max(baro$dtime) + lubridate::minutes(10), interval = "5 mins")
  #   
  #   #To count the LOCF operations, we count the NAs in the data frame before and after the LOCF
  #   countNAs <- baro_pad[1,]
  #   for(i in 2:ncol(baro_pad)){
  #     countNAs[,i] <- sum(is.na(baro_pad[,i])) #count NAs before they are filled
  #     baro_pad[,i] <- zoo::na.locf(baro_pad[,i], maxgap = 2, na.rm = FALSE) #maxgap = 2 means only fill NAs created by the pad
  #     countNAs[,i] <- countNAs[,i]- sum(is.na(baro_pad[,i])) #subtract remaining NAs to get number of NAs filled
  #   }
  #   countNAs %<>% dplyr::select(-dtime)
  #   countNAs_t <- countNAs %>% t() %>% data.frame() %>% tibble::rownames_to_column() %>%  magrittr::set_colnames(c("Location", "No. of LOCFs"))
  #   
  #   #Return baro data to long data format
  #   baro <- tidyr::gather(baro_pad, "smp_id", "baro_psi", -dtime) %>%
  #     dplyr::filter(!is.na(baro_psi))
  # }
  # #Calculate the distance between every baro location and the target SMP, then add weight
  # baro_weights <- dplyr::filter(smp_loc, smp_id %in% baro_smp) %>%
  #   dplyr::mutate(lon_dist = lon_wgs84 - locus_loc$lon_wgs84,
  #                 lat_dist = lat_wgs84 - locus_loc$lat_wgs84,
  #                 dist_total = sqrt(lon_dist**2 + lat_dist**2)) %>%
  #   dplyr::mutate(weight = 1/dist_total) %>% #inverse distance weight with power = 1
  #   dplyr::select(smp_id, weight) %>%
  #   dplyr::arrange(smp_id)
  # 
  # #Cap weight at 1000
  # baro_weights$weight <- replace(baro_weights$weight, baro_weights$weight > 1000, 1000)
  # # Interpolate and return the baro data
  # interpolated_baro <- dplyr::left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
  #   dplyr::group_by(dtime) %>% #group datetimes, then calculate weighting effect for each datetime
  #   dplyr::summarize(baro_psi = marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
  #                    smp_id =  "Interpolated",
  #                    neighbors = dplyr::n()) %>%
  #   zoo::na.trim(sides = "right") #trim trailing NAs
  
}