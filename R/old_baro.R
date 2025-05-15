old_baro <- function(con, target_id, start_date, end_date, data_interval = c("5 mins", "15 mins")){
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  
  #Handle date Conversion
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  #Get SMP locations, and the locations of the baro sensors
  smp_loc <- odbc::dbGetQuery(con, "SELECT * FROM admin.tbl_smp_loc")
  locus_loc <- dplyr::filter(smp_loc, smp_id == target_id)
  baro_smp <- odbc::dbGetQuery(con, "SELECT DISTINCT smp_id FROM admin.tbl_baro_rawfile;") %>% dplyr::pull(smp_id)

  #Collect baro data
  #Get all baro data for the specified time period
  baro <- odbc::dbGetQuery(con, paste0("SELECT * FROM data.viw_barodata_smp b WHERE b.dtime_est >= '", start_date, "'", " AND b.dtime_est <= '", end_date + lubridate::days(1), "' order by dtime_est;"))

  baro_latest_dtime <- odbc::dbGetQuery(con, paste0("SELECT max(dtime_est) FROM data.tbl_baro WHERE dtime_est < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()
  baro_latest_valid <- odbc::dbGetQuery(con, paste0("SELECT max(dtime_est) FROM data.viw_barodata_neighbors WHERE neighbors >= 4 and dtime_est < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()

  if(length(baro$dtime_est) == 0){
    stop (paste0("No data available in the reqested interval. The latest available baro data is from ", baro_latest_dtime, "."))
  }

  #this is a seperate pipe so that it could be stopped before the error
  needs_thickening <- baro$dtime_est %>% lubridate::second() %>% {. > 0} %>% any() == TRUE
  if(needs_thickening == TRUE){
    baro %<>% padr::thicken(interval = "5 mins", rounding = "down") %>%
      dplyr::group_by(dtime_est_5_min, smp_id) %>%
      dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>%
      dplyr::select(dtime_est = dtime_est_5_min, smp_id, baro_psi) %>%
      dplyr::ungroup()
  }else{
    baro %<>% dplyr::group_by(dtime_est, smp_id) %>%
      dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>%
      dplyr::select(dtime_est, smp_id, baro_psi) %>%
      dplyr::ungroup()
  }


  baro$dtime_est %<>% lubridate::force_tz(tz = "EST")

  #initialize countNAs_t in case the loop doesn't run. It is passed as a param to markdown so it needs to exist.
  countNAs_t <- 0

  #When the user requests data at a 5-minute resolution, we need to stretch our 15-minute data into 5-minute data
  #We can use tidyr::spread and padr::pad to generate the full 5 minute time series,
  #And then use zoo::na.locf (last observation carried forward) to fill the NAs with the most recent value
  if(data_interval == "5 mins"){

    #Spread data to have all baro measurements use the same dtime_est column
    #So we can pad every 15-minute time series at once
    baro <- tidyr::spread(baro, "smp_id", "baro_psi")

    #Pad installs 5 minute intervals in our 15 minute dtime_est column. All other columns become NA
    #End value is 10 minutes after the final period because that 15 minute data point is good for 10 more minutes
    baro_pad <- padr::pad(baro, start_val = min(baro$dtime_est), end_val = max(baro$dtime_est) + lubridate::minutes(10), interval = "5 mins")

    #To count the LOCF operations, we count the NAs in the data frame before and after the LOCF
    countNAs <- baro_pad[1,]
    for(i in 2:ncol(baro_pad)){
      countNAs[,i] <- sum(is.na(baro_pad[,i])) #count NAs before they are filled
      baro_pad[,i] <- zoo::na.locf(baro_pad[,i], maxgap = 2, na.rm = FALSE) #maxgap = 2 means only fill NAs created by the pad
      countNAs[,i] <- countNAs[,i]- sum(is.na(baro_pad[,i])) #subtract remaining NAs to get number of NAs filled
    }

    countNAs %<>% dplyr::select(-dtime_est)
    countNAs_t <- countNAs %>% t() %>% data.frame() %>% tibble::rownames_to_column() %>%  magrittr::set_colnames(c("Location", "No. of LOCFs"))

    #Return baro data to long data format
    baro <- tidyr::gather(baro_pad, "smp_id", "baro_psi", -dtime_est) %>%
      dplyr::filter(!is.na(baro_psi))
  }
  baro

  #Calculate the distance between every baro location and the target SMP, then add weight
  baro_weights <- dplyr::filter(smp_loc, smp_id %in% baro_smp) %>%
    dplyr::mutate(lon_dist = lon_wgs84 - locus_loc$lon_wgs84,
                  lat_dist = lat_wgs84 - locus_loc$lat_wgs84,
                  dist_total = sqrt(lon_dist**2 + lat_dist**2)) %>%
    dplyr::mutate(weight = 1/dist_total) %>% #inverse distance weight with power = 1
    dplyr::select(smp_id, weight) %>%
    dplyr::arrange(smp_id)

  #Cap weight at 1000
  baro_weights$weight <- replace(baro_weights$weight, baro_weights$weight > 1000, 1000)

  interpolated_baro <- dplyr::left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
    dplyr::group_by(dtime_est) %>% #group datetimes, then calculate weighting effect for each datetime
    dplyr::summarize(baro_psi = marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
                     smp_id =  "Interpolated",
                     neighbors = dplyr::n()) %>%
    zoo::na.trim(sides = "right") #trim trailing NAs
  # 
  # #Initialize Final Series
  # finalseries <- interpolated_baro
  # 
  # 
  # #Give 5 or 15 minute data as appropriate
  # if(data_interval == "15 mins"){
  #   clippedseries <- data.frame(dtime_est = seq.POSIXt(from = start_date, to = end_date + lubridate::days(1), by = data_interval) )
  #   
  #   finalseries <- dplyr::filter(finalseries, dtime_est %in% clippedseries$dtime_est)
  #   baro <- dplyr::filter(baro, dtime_est %in% clippedseries$dtime_est)
  # }
  # 
  # 
  # #Adding "neighbor" counts and instances to report
  # neighbors <- dplyr::group_by(finalseries, neighbors) %>%
  #   dplyr::summarize(count = dplyr::n()) %>%
  #   magrittr::set_colnames(c("Neighbors", "Count"))
  # 
  # ##finalseries is now ready, but return() must happen after plot and markdown are created
  # 
  # #Baro Raster Plot
  # #Get elevations #This has been removed in favor of using distances to sort SMPs on plot. Code is left in case 
  # #baro_elev <- odbc::dbGetQuery(con, "SELECT * FROM smp_elev") %>% filter(smp_id %in% baro$smp_id)
  # 
  # #currently disabled (2/2/21)
  # 
  # # #Bind all raw baro data with interpolated data, and add weights
  # # baro_p <- dplyr::bind_rows(baro, finalseries)
  # # baro_p <- dplyr::left_join(baro_p, baro_weights, by = "smp_id") 
  # # 
  # # #Set NA weights to max weight +1 so interpolated data plots at the top of the chart
  # # baro_p$weight[is.na(baro_p$weight)] <- max(baro_p$weight)+1
  # # 
  # # #Sort SMP IDs by elevation
  # # baro_p$smp_id <- factor(baro_p$smp_id, levels = unique(baro_p$smp_id[order(baro_p$weight)]))
  # # 
  # # #Add year and day for chart
  # # baro_p %<>% dplyr::mutate("day" = yday_decimal(baro_p$dtime_est),
  # #                    "year" = lubridate::year(baro_p$dtime_est))
  # # 
  # # 
  # # baro_p$smp_id %<>% as.factor
  # # 
  # # #Create baro Raster Chart
  # # p <- marsBaroRasterPlot(baro_p)
  # 
  # 
  # #Create baro Map
  # # baro_loc <- smp_loc %>% dplyr::filter(smp_id %in% baro$smp_id)
  # # rownames(baro_loc) <- NULL
  # # coords <- baro_loc[c("lon_wgs84", "lat_wgs84")]
  # # baro_sp <- sp::SpatialPointsDataFrame(coords, data = baro_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # # coords <- locus_loc[c("lon_wgs84", "lat_wgs84")]
  # # smp_sp <- sp::SpatialPointsDataFrame(coords, data = locus_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # # baro_map <- mapview::mapview(baro_sp, layer.name = "Baro") + mapview::mapview(smp_sp, color = "red", col.regions = NA, layer.name = "Target SMP")
  # # 
  # downloader_folder <- "O:/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader"
  # # downloader_folder_csv <- "\\\\\\\\pwdoows\\\\oows\\\\Watershed Sciences\\\\GSI Monitoring\\07 Databases and Tracking Spreadsheets\\13 MARS Analysis Database\\\\Scripts\\\\Downloader\\\\Baro Data Downloader\\\\"
  # # 
  # #render markdown document
  # #output file and output dir arguments do not work, so file is placed where markdown document is, and moved later
  # 
  # #markdown is currently disabled
  # 
  # # rmarkdown::render(system.file("rmd", "baro.rmd", package = "pwdgsi"), #find .rmd location on local cpu
  # #                   params = list(smp_id = target_id,  #input parameters to be passed into markdown body
  # #                                 start_date = start_date,
  # #                                 end_date = end_date,
  # #                                 data_interval = data_interval,
  # #                                 neighbors = neighbors,
  # #                                 countNAs = countNAs_t,
  # #                                 p = p,
  # #                                 map = baro_map,
  # #                                 baro_latest_dtime = baro_latest_dtime,
  # #                                 baro_latest_valid = baro_latest_valid))
  # 
  # # #give a new filename and path
  # # new_filename <- paste0(downloader_folder, "/Reports/", paste(target_id, start_date, "to", end_date, sep = "_"), "_baro_report.html")
  # # 
  # # #move file to Baro Data Downloader/Reports folder
  # # file.rename(from = paste0(system.file("rmd", "baro.html", package = "pwdgsi")),
  # #             to = new_filename)
  # # 
  # # #open html
  # # browseURL(new_filename)
  # 
  # #return Final Series. 
  # return(finalseries)
  
}