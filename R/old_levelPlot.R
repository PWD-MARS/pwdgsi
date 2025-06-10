old_levelPlot <- function(event, 
                                                structure_name, 
                                                storage_depth_ft, 
                                                obs_datetime, 
                                                obs_level_ft,
                                                level_names = NA,
                                                sim_datetime = NA,
                                                sim_level_ft = NA,
                                                orifice_show = FALSE,
                                                orifice_height_ft = NULL,
                                                datetime_2 = NA,
                                                level_ft_2 = NA,
                                                datetime_3 = NA,
                                                level_ft_3 = NA,
                                                datetime_4 = NA,
                                                level_ft_4 = NA,
                                                metrics_show = FALSE,
                                                obs_RSPU,
                                                sim_RSPU,
                                                obs_infil_inhr,
                                                sim_infil_inhr,
                                                obs_draindown_hr,
                                                sim_draindown_hr,
                                                obs_overtopping ,
                                                sim_overtopping){
  
  #1 Process Data
  
  #1.1 
  #Confirm that storage depth is explicitly defined
  if(!is.numeric(storage_depth_ft) | is.na(storage_depth_ft)){
    stop("storage_depth is not numeric.")
  }
  
  #1.2
  #Set negative water levels to zero
  obs_level_ft[which(obs_level_ft < 0)] <- 0
  
  # if(!is.na(sim_level_ft[1])){
  #   sim_level_ft[which(sim_level_ft < 0)] <- 0
  # }
  
  
  #1.3
  #Check that data is associated with event
  if(length(obs_level_ft) == 0){
    stop(paste0("No data loaded in observed Event", event, "."))
  }
  
  #1.4 QC check for observed data record
  #Using code from marsDetectEvents
  prepseries <- obs_datetime %>%
    data.frame() %>% 
    dplyr::mutate(lag_time = dplyr::lag(obs_datetime, 1)) %>%
    dplyr::mutate(gap_hr = difftime(obs_datetime, lag_time, units = "hours")) %>%
    dplyr::filter(gap_hr > 6)
  
  if(nrow(prepseries) > 0){
    message(paste0("Warning: Missing values in observed time series."))
    warning_label <- "Warning: Missing values in observed time series."
  }else{
    warning_label <- ""
  }


  #1.5
  #Check is orifice should be shown
  if(orifice_show == TRUE){
    orifice_plot <- orifice_height_ft
    orifice_lab <- paste0("orifice elevation: ",round(orifice_height_ft, 2))
  }else{
    orifice_plot <- 0 #line will be covered by bottom of structure if option is not selected
  }

  #1.6 set default names for levels if none are provided
  #### This is broken if more than 1 level is given (it's expecting a single value)
  if(is.na(level_names)){
    level_names <- c("Obs. Level 1",
                     "Obs. Level 2",
                     "Obs. Level 3",
                     "Obs. Level 4")
  }

  #2. Calculate plotting parameters

  #2.1 Calculate date plotting limits(x-axis)
  #Calculate minimum and maximum data values


  if(!is.na(sim_level_ft[1])){
    min_date <- min(obs_datetime, sim_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, sim_datetime, na.rm = TRUE)
  }else{
    min_date <- min(obs_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, na.rm = TRUE) #+ hours(6)
  }

  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date

  #set date marker offset by duration
  if(units(event_duration) == "days"){
    marker_scale <- 0.02
    day_lengths <- event_duration + 2
  }else{
    marker_scale <- 0.015
    day_lengths <- 14
  }
  marker_scale

  #2.2 Calculations for dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = day_lengths), tz = "EST")

  #2.4 Calculate axis breaks based on plotting limits
  #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST")

  # #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12), max_date + lubridate::hours(6), by = "hour"), tz = "EST")

  #2.5 Generate title block
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(min_date),
                       sep = "")


  # Build dataframes
  obs_df <- data.frame(obs_datetime, obs_level_ft)

  # # We no longer use include sim functions in pwdgsi
  # if(!is.na(sim_level_ft[1])){
  #   sim_df <- data.frame(sim_datetime, sim_level_ft)
  # }

  if(!is.na(level_ft_2[1])){
    obs2_df <- data.frame(datetime_2, level_ft_2)
  }

  if(!is.na(level_ft_3[1])){
    obs3_df <- data.frame(datetime_3, level_ft_3)
  }

  if(!is.na(level_ft_4[1])){
    obs4_df <- data.frame(datetime_4, level_ft_4)
  }

  #3. Generate plot
  #3.1 Water Level (observed)
  level_plot <-
    ggplot2::ggplot(data = obs_df) +

    #Day boundaries
    ggplot2::geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", linewidth = 1.2) + #date boundaries

  ggplot2::annotate("text", x = day_marker-marker_scale*event_duration,
                    y = 0.8*storage_depth_ft,
                    label = day_marker,
                    angle = 90,
                    size = ggplot2::rel(5))+ #5

  #   #Warning message for data gaps in observed record
    ggplot2::annotate("text", x = day_marker[1]+1,
                      y = 0.5*storage_depth_ft,
                      label = warning_label, #empty if no warning
                      hjust = 0,
                      color = "red",
                      size = ggplot2::rel(5))+

  #   #Structure top and bottom
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2)+ #bottom

    ggplot2::geom_hline(yintercept = storage_depth_ft, color = "orange", linewidth = 1.2) + #top

    ggplot2::geom_label(x = min_date + event_duration/4,
                        y = storage_depth_ft*1.04,
                        label = "Maximum Storage Depth",
                        size = ggplot2::rel(5),
                        fill = "white",
                        label.size = 0) +
    # ggplot2::annotate(ggplot2::aes(x = min_date + event_duration/4,
    #                                  y = storage_depth_ft*1.04,
    #                                  label = "Maximum Storage Depth"),
    #                     size = ggplot2::rel(5),
    #                     fill = "white",
    #                     label.size = 0) +

    #Observed water level
    ggplot2::geom_line(data = obs_df,
                       ggplot2::aes(x = obs_datetime,
                                    y = obs_level_ft,
                                    color = paste(level_names[1])),
                       linewidth = 2
    ) +

    #Formatting
    ggplot2::theme_bw() + # a basic black and white theme

    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)), # set x axis limits
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks
    ) +

  ggplot2::scale_y_continuous(
    breaks = seq(0, storage_depth_ft+1, by = if(storage_depth_ft > 2) round(storage_depth_ft/4, 0) else ceiling(storage_depth_ft/4)),
    minor_breaks = seq(-0.5,2*storage_depth_ft, by = 0.1)
  ) +

    ggplot2::scale_color_manual(values = c("#7822E0","#E0DE43","#E03838","#E12CE0","#16E050")) +

    ggplot2::labs(
      y = "Water Level (ft)",
      title = title_text
    ) +

    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
      legend.title=ggplot2::element_blank())

  if(!is.na(sim_level_ft[1])){
    level_plot <- level_plot +
      #Simulated water level
      ggplot2::geom_line(data = sim_df,
                         ggplot2::aes(x = sim_datetime,
                                      y = sim_level_ft,
                                      color = "Simulated Water Level"),
                         linewidth = 2
      )
  }

  if(!is.na(level_ft_2[1])){
    level_plot <- level_plot +
      #Simulated water level
      ggplot2::geom_line(data = obs2_df,
                         ggplot2::aes(x = datetime_2,
                                      y = level_ft_2,
                                      color = paste(level_names[2])),
                         linewidth = 2
      )
  }
  if(!is.na(level_ft_3[1])){
    level_plot <- level_plot +
      #Simulated water level
      ggplot2::geom_line(data = obs3_df,
                         ggplot2::aes(x = datetime_3,
                                      y = level_ft_3,
                                      color = paste(level_names[3])),
                         linewidth = 2
      )
  }
  if(!is.na(level_ft_4[1])){
    level_plot <- level_plot +
      #Simulated water level
      ggplot2::geom_line(data = obs4_df,
                         ggplot2::aes(x = datetime_4,
                                      y = level_ft_4,
                                      color = paste(level_names[4])),
                         linewidth = 2
      )
  }

  if(orifice_show == TRUE){
    level_plot <- level_plot +

      ggplot2::geom_hline(yintercept = orifice_plot, color = "grey", linetype = 2, linewidth = 1.2) +
      ggplot2::geom_label(label = orifice_lab,
                          y = orifice_height_ft*1.1,
                          x = obs_datetime[round(0.75*length(obs_datetime))])

  }

  # Add metrics
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

    level_plot %<>% marsMetricsTable( obs_RSPU = obs_RSPU,
                                      obs_infil_inhr = obs_infil_inhr,
                                      obs_draindown_hr = obs_draindown_hr,
                                      obs_overtopping = obs_overtopping,
                                      sim_RSPU = sim_RSPU,
                                      sim_infil_inhr = sim_infil_inhr,
                                      sim_draindown_hr = sim_draindown_hr,
                                      sim_overtopping = sim_overtopping)
  }

  return(level_plot)
  
}