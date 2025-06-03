old_eventPlot <- function(con,
                          event_date,
                          source = 'radar',
                          event_uid,
                          smp_id,
                          ow_suffix,
                          sump_correct = TRUE,
                          orifice_show = FALSE,
                          orifice_height_ft = NULL,
                          storage_depth_ft,
                          metrics_show = FALSE,
                          obs_RSPU,
                          # sim_RSPU,
                          obs_infil_inhr,
                          # sim_infil_inhr,
                          obs_draindown_hr,
                          # sim_draindown_hr,
                          obs_overtopping
                          # sim_overtopping

){
  
  ##debug
  # if(debug == TRUE){
  #   browser()
  # }
  
  ## Check DB connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage", "radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  
  ## Grabbing data from the given inputs
  
  #get ow_uid
  ow_query <- paste0("SELECT ow_uid FROM fieldwork.tbl_ow
                     WHERE smp_id LIKE '",smp_id,
                     "' AND ow_suffix LIKE '",ow_suffix, "'")
  
  ow_uid <- DBI::dbGetQuery(con, ow_query) |> dplyr::pull()
  
  # check if ow exists
  if(length(ow_uid) == 0){
    stop("OW does not exist within the fiedlwork database.")
  }


  if(missing(event_uid)){


    #check for one day on either side of the event date
    event_date %<>% as.POSIXct(format = '%Y-%m-%d')
    start_date <- event_date - 86400
    end_date <- event_date   + 86400


    # browser()
    # Grab the data
    mon_data <- old_monitoring(con = con,
                                target_id = smp_id,
                                ow_suffix = ow_suffix,
                                start_date = start_date,
                                source = source,
                                end_date = end_date,
                                sump_correct = sump_correct)
    
  event_data <- mon_data$`Rain Event Data`

    if(source %in% c('gage','gauge')){
      event_uid <- event_data$gage_event_uid
    }

    if(source == 'radar'){
      event_uid <- event_data$radar_event_uid
    }

    # Stop if no events have been found
    if(nrow(event_data) == 0){
      stop(paste0("There are no events on ",event_date))}



    # pick closest event if multiple events
    if(nrow(event_data) > 1){
      min_dif <- min(abs(event_data$eventdatastart_est - event_date))
      event_data <- event_data[abs(event_data$eventdatastart_est - event_date) == min_dif,]
    }
  } else {

    # browser()
    # write event query based on source
    if(source %in% c('gage','gauge')){
      event_query <- paste0('SELECT * FROM data.tbl_gage_event where gage_event_uid = ',event_uid)
    }



  if(source == 'radar'){
    event_query <- paste0('SELECT * FROM data.tbl_radar_event where radar_event_uid = ',event_uid)
  }

  # browser()
  event_data <- dbGetQuery(con, event_query)
  event_date <- event_data$eventdatastart_edt %>% as.Date()
  start_date <- event_data$eventdatastart_edt %>% as.Date()
  end_date <- event_data$eventdataend_edt %>% as.Date()

  # browser()
  mon_data <- old_monitoring(con = con,
                              target_id = smp_id,
                              ow_suffix = ow_suffix,
                              start_date = start_date,
                              source = source,
                              end_date = end_date,
                              sump_correct = sump_correct)
  }
  mon_data


  # get event column name (dependent on radar/gage)
  event_col <- colnames(mon_data$`Level Data`)[grep('event',colnames(mon_data$`Level Data`))]

  # define individual datasets
  # filter out rainfall data not associated with an event
  rainfall_data <- mon_data$`Rainfall Data`[!is.na(mon_data$`Rainfall Data`[,event_col]),]
  #filter to specific event
  rainfall_data <- rainfall_data[rainfall_data[,event_col] == event_data[,event_col],]

  # 
  # make sure we capture at least three points before the water level response
  level_data <- mon_data$`Level Data`
  level_data$lvl_lag <- c(diff(level_data$level_ft, 3),0,0,0)
  # 
  level_data <- level_data %>% dplyr::mutate(lageql = abs(level_ft - lvl_lag)) %>%
    dplyr::mutate(isevent = !is.na(level_data[,event_col])) %>%
    dplyr::filter(lageql != 0 | isevent) %>%
  #   #remove columns no longer used
    dplyr::select(-lvl_lag, -lageql,-isevent)
  level_data <- mon_data$`Level Data`[!is.na(mon_data$`Level Data`[,event_col]),]
  #filter to specific event
  level_data <- level_data[level_data[,event_col] == event_data[,event_col],]


  #match inputs to marsCombinedPlot
  rainfall_in <- rainfall_data$rainfall_in
  rainfall_datetime <- rainfall_data$dtime_est
  obs_level_ft <- level_data$level_ft
  obs_datetime <- level_data$dtime_est


#Define values from snapshot
snapshot <- marsFetchSMPSnapshot(con = con,
                                 smp_id = smp_id,
                                 ow_suffix = ow_suffix,
                                 request_date = event_date)

#set NA's to 0's
if( is.na(snapshot$orificedepth_ft) ){snapshot$orificedepth_ft <- 0}

# set max storage and orifice defaults when not provided
# browser()
if( missing(orifice_show) ){
  orifice_show <- if(snapshot$orificedepth_ft == 0){0}else{1}
}

if( missing(orifice_height_ft) ){
  orifice_height_ft <- snapshot$orificedepth_ft
}

if( missing(storage_depth_ft) ){
  storage_depth_ft <- snapshot$storage_depth_ft
}

# Combine strings for structure name
structure_name <- paste0(smp_id," | Monitoring Location: ",ow_suffix)

#Add a last date so the hyetograph looks better
rainfall_in <- append(rainfall_in, 0)
rainfall_datetime <- append(rainfall_datetime, max(obs_datetime)) %>% lubridate::with_tz("EST")

#1 Run functions for individual plots
level_plot <- old_levelPlot(event = event_data$event_uid[1],
                             structure_name = structure_name,
                             obs_datetime = obs_datetime,
                             obs_level_ft = obs_level_ft,
                             storage_depth_ft = storage_depth_ft,
                             orifice_show = orifice_show,
                             orifice_height_ft = orifice_height_ft)

  rainfall_plot <- old_rainPlot(event = event_data$event_uid[1],
                                dtime_est = rainfall_datetime,
                                rainfall_in = rainfall_in,
                                reverse_y = TRUE)

  #2 Combine Plots

  #Save out legends
  level_legend <- cowplot::get_legend(level_plot)
  rainfall_legend <- cowplot::get_legend(rainfall_plot)

  #Calculate date plotting limits(x-axis)
  #Calculate minimum and maximum data values
  min_date <- min(obs_datetime, na.rm = TRUE)
  max_date <- max(obs_datetime, na.rm = TRUE) #+ hours(6)

  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date

  #set date marker offset by duration
  if(units(event_duration) == "days"){
    marker_scale <- 0.02
  }else{
    marker_scale <- 0.015
  }
marker_scale
  #Calculations for dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")

  #Calculate axis breaks based on plotting limits
  #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST")

  #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12), max_date + lubridate::hours(6), by = "hour"), tz = "EST")

  #Title
  title_text <- paste0("Water Level and Rainfall\nSMP ID: ", structure_name,
                       " | Event: ", event_uid,
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(min_date),
                       sep = "")

  #Remove legends and titles and update axes
  level_plot <- level_plot +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1)),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(.95)))
  rainfall_plot <- rainfall_plot +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.35)),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.25)),
                   axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.25))) +
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks)  +
    ggplot2::labs(title = title_text)

  # ggplot2::geom_label(ggplot2::aes(x = max_date - (max_date - min_date)*0.1,
  #                                  y = Inf,
  #                                  label = metrics_caption),
  #                    # size = ggplot2::rel(5),
  #                     size = 4.7,
  #                     fill = "white",
  #                     label.size = 0)
  # ggplot2::annotate("richtext", y = Inf, x = max_date - (max_date - min_date)*0.01, vjust=0, hjust = 1, size = 4.7, label = metrics_caption, fill = "white")
  # ggplot2::annotate("text", x = max_date - lubridate::minutes(60), y = max(rainfall_in), vjust=0, hjust = 1, label = metrics_caption)


  if( missing(metrics_show) ){metrics_show <- FALSE}
  if(metrics_show == TRUE){

    #set missing values to ""
    if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
    if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
    if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
    if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
    if( missing(obs_RSPU) ){obs_RSPU <- ""}
    if( missing(sim_RSPU) ){sim_RSPU <- ""}
    if( missing(obs_overtopping) ){obs_overtopping <- ""}
    if( missing(sim_overtopping) ){sim_overtopping <- ""}

    level_plot %<>% marsMetricsTable(obs_RSPU = obs_RSPU,
                                     obs_infil_inhr = obs_infil_inhr,
                                     obs_draindown_hr = obs_draindown_hr,
                                     obs_overtopping = obs_overtopping,
                                     sim_RSPU = sim_RSPU,
                                     sim_infil_inhr = sim_infil_inhr,
                                     sim_draindown_hr = sim_draindown_hr,
                                     sim_overtopping = sim_overtopping)
  }



  #Calculate max width and set both to that value
  #Grob
  level_grob <- ggplot2::ggplotGrob(level_plot)
  rainfall_grob <- ggplot2::ggplotGrob(rainfall_plot)

  #Set max width
  maxWidth = grid::unit.pmax(level_grob$widths[2:9], rainfall_grob$widths[2:9])
  level_grob$widths[2:9] <- maxWidth
  rainfall_grob$widths[2:9] <- maxWidth

  #Arrange the plots and export
  combined_plot <- gridExtra::grid.arrange(rainfall_grob, level_grob, #plots
                                           rainfall_legend, level_legend, #legends
                                           ncol = 1,
                                           heights = c(1.1, 2, 0.15, 0.15))

  return(combined_plot)
}
