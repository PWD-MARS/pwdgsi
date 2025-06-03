old_combinedPlot <- function(event, 
                             structure_name, 
                             obs_datetime, 
                             obs_level_ft,
                             sim_datetime = NA,
                             sim_level_ft = NA,
                             storage_depth_ft, 
                             orifice_show = FALSE,
                             orifice_height_ft = NULL,
                             rainfall_datetime,
                             rainfall_in,
                             metrics_show = FALSE,
                             obs_RSPU,
                             sim_RSPU,
                             obs_infil_inhr,
                             sim_infil_inhr,
                             obs_draindown_hr,
                             sim_draindown_hr,
                             obs_overtopping,
                             sim_overtopping
){
  
  # potential to add back in; updated variable names; kept obs_peak_level_ft
  # if(!is.na(obs_peak_level_ft) | !is.na(obs_infil_inhr) | !is.na(obs_percent_storage_relative) | !is.na(obs_draindown_hr)){
  #   metrics_caption <- paste0("Performance Metrics  Obs. Sim. <br />
  #                              Peak Level     (ft)  ", obs_peak_level_ft[1], "  ",  sim_peak_level_ft[1], "<br />
  #                              Sat. Infil  (in/hr)  ", obs_infil_inhr[1], "  ", sim_infil_inhr[1], "<br />
  #                              Rel Storage Use   %  ", obs_RSPU[1], "  ", sim_RSPU[1], "<br />
  #                              Draindown Time (hr)  ", obs_draindown_hr[1], "  ", sim_draindown_hr[1])
  # }else{
  #   metrics_caption <- ""
  # }
  
  #Add a last date so the hyetograph looks better
  rainfall_in <- append(rainfall_in, 0)
  if(!is.na(sim_level_ft[1])){
    rainfall_datetime <- append(rainfall_datetime, max(obs_datetime, sim_datetime)) %>% lubridate::with_tz("EST")
  }else{
    rainfall_datetime <- append(rainfall_datetime, max(obs_datetime)) %>% lubridate::with_tz("EST")
  }

  #1 Run functions for individual plots
  level_plot <- old_levelPlot(event = event,
                              structure_name = structure_name,
                              obs_datetime = obs_datetime,
                              obs_level_ft = obs_level_ft,
                              sim_datetime = sim_datetime,
                              sim_level_ft = sim_level_ft,
                              storage_depth_ft = storage_depth_ft,
                              orifice_show = orifice_show,
                              orifice_height_ft = orifice_height_ft)

  rainfall_plot <- old_rainPlot(event = event,
                                dtime_est = rainfall_datetime,
                                rainfall_in = rainfall_in,
                                reverse_y = TRUE)

  #2 Combine Plots

  #Save out legends
  level_legend <- get_legend(level_plot)
  rainfall_legend <- get_legend(rainfall_plot)
  # 
  # #Calculate date plotting limits(x-axis) 
  # #Calculate minimum and maximum data values
  # if(!is.na(sim_level_ft[1])){
  #   min_date <- min(obs_datetime, sim_datetime, na.rm = TRUE)
  #   max_date <- max(obs_datetime, sim_datetime, na.rm = TRUE)
  # }else{
    min_date <- min(obs_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, na.rm = TRUE) #+ hours(6)
  # }

  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date

  #set date marker offset by duration
  if(units(event_duration) == "days"){
    marker_scale <- 0.02
  }else{
    marker_scale <- 0.015
  }

    # #Calculations for dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")

  #Calculate axis breaks based on plotting limits
  #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST")

  #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12), max_date + lubridate::hours(6), by = "hour"), tz = "EST")

  #Title
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
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
                   axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.25)))  +
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks)  +
    ggplot2::labs(title = title_text)
  # 
  # # ggplot2::geom_label(ggplot2::aes(x = max_date - (max_date - min_date)*0.1, 
  # #                                  y = Inf, 
  # #                                  label = metrics_caption),
  # #                    # size = ggplot2::rel(5),
  # #                     size = 4.7,
  # #                     fill = "white", 
  # #                     label.size = 0)
  # # ggplot2::annotate("richtext", y = Inf, x = max_date - (max_date - min_date)*0.01, vjust=0, hjust = 1, size = 4.7, label = metrics_caption, fill = "white")
  # # ggplot2::annotate("text", x = max_date - lubridate::minutes(60), y = max(rainfall_in), vjust=0, hjust = 1, label = metrics_caption)
  # 
  # if(metrics_show == TRUE){
  #   
  #   #set missing values to ""
  #   if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
  #   if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
  #   if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
  #   if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
  #   if( missing(obs_RSPU) ){obs_RSPU <- ""}
  #   if( missing(sim_RSPU) ){sim_RSPU <- ""}
  #   if( missing(obs_overtopping) ){obs_overtopping <- ""}
  #   if( missing(sim_overtopping) ){sim_overtopping <- ""}
  #   
  #   level_plot %<>% marsMetricsTable(obs_RSPU = obs_RSPU,
  #                                    obs_infil_inhr = obs_infil_inhr,
  #                                    obs_draindown_hr = obs_draindown_hr,
  #                                    obs_overtopping = obs_overtopping,
  #                                    sim_RSPU = sim_RSPU,
  #                                    sim_infil_inhr = sim_infil_inhr,
  #                                    sim_draindown_hr = sim_draindown_hr,
  #                                    sim_overtopping = sim_overtopping) 
  # }
  # 
  # 
  # 
  #Calculate max width and set both to that value
  #Grob
  level_grob <- ggplot2::ggplotGrob(level_plot)
  rainfall_grob <- ggplot2::ggplotGrob(rainfall_plot)
  # 
  # #Set max width
  maxWidth = grid::unit.pmax(level_grob$widths[2:9], rainfall_grob$widths[2:9])
  level_grob$widths[2:9] <- maxWidth
  rainfall_grob$widths[2:9] <- maxWidth

  #Arrange the plots and export
  combined_plot <- gridExtra::grid.arrange(rainfall_grob, level_grob, #plots
                                           rainfall_legend, level_legend, #legends
                                           ncol = 1,
                                           heights = c(1.1, 2, 0.15, 0.15),
                                           newpage = TRUE)

  return(combined_plot)
}