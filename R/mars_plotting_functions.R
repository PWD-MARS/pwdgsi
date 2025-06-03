# marsRainfallPlot ------------------------------------------
#' Plot hyetograph
#'
#' Return hyetograph of a single rain event
#'
#' @param dtime vector, POSIXct datetimes representing a single rain event
#' @param rainfall_in vector, num, rainfall in inches for that rain event
#' @param event chr, label for the hyetograph for what rain gage the data came from
#' @param reverse_y logical, whether the Y axes should be reversed
#'
#' @return Output is a ggplot2 object of the hyetograph. 
#'
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}
#'
#' @export
#' 
#' @examples
#' gage_temp <- dplyr::mutate(marsSampleRain, 
#'   event_id = marsDetectEvents(dtime = marsSampleRain$dtime, 
#'   rainfall_in = marsSampleRain$rainfall_in, 
#'   iet_hr = 6, mindepth_in = 0.10)) %>% dplyr::filter(event_id == 2)
#'   
#' marsRainfallPlot(dtime = gage_temp$dtime, 
#'   rainfall_in = gage_temp$rainfall_in, event = 2)   



marsRainfallPlot <- function(dtime, rainfall_in, event, reverse_y = FALSE) {
  #### Should this be based on detect events? Doesn't fetchRainfall give this info?
  #### There needs to be a pre-process step to select a single event.
  
  # Data validation
  if(length(dtime) != length(rainfall_in)) {
    stop("Datetime and rainfall lengths must be equal")
  }
  
  if(length(event) > 1) {
    stop("Argument 'event' must be of length 1")
  }
  
  # Combine into dataframe
  rain_data <- data.frame(dtime,
                          rainIN = rainfall_in) %>% 
    dplyr::arrange(dtime)
  ### Shouldn't this happen before we combine into a df?
  if(nrow(rain_data) == 0) {
    stop("No data loaded")
  }

  # Minimum interval is 15 min
  min_interval <- lubridate::minutes(15)

  # Add cumulative rainfall to df
  #### Should we have culmuative amount of rain in inches or percent of rain from a storm?
  rain_data <- rain_data %>% dplyr::mutate(cumulative = cumsum(rainIN))
  # Generate title block
  startdate <- min(rain_data$dtime) - min_interval
  title_text <- paste0("Hyetograph\n| Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::label_time("%Y-%m-%d %H:%M", tz = "America/New_York")(startdate),
                       sep = "")

  # Shift dtime so column shows rainfall between interval
  rain_data$dtime <- rain_data$dtime - min_interval
  
  # Calculate plotting parameters
  min_date <- min(rain_data$dtime, na.rm = TRUE)
  max_date <- max(rain_data$dtime, na.rm = TRUE)
  min_rain <- 0
  max_rain <- max(rain_data$rainIN, na.rm = TRUE)
  # calculate scaling factor for secondary y-axis for cumulative rainfall
  #### Both axis are for inches of rainfall but they have 2 diff scales. Discrete vs Cumulative
  #### This makes it really hard to understand and I would recommend changing the cumulative
  #### to % of storm total and then specifically reference the size of the storm.
  #### Why are we calculating the scaling factor like this? Why 110%? This done outside of scale
  max_cumulative_scaling <- max(1.1*rain_data$cumulative, na.rm = TRUE)/max_rain

  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  range_rainfall <- max_rain - min_rain
  
  # If rainfall range is < 0.1, set as max and recalculate cumulative scale
  if(range_rainfall < 0.1) {
    max_rain <- 0.1 
    #### This should always be 0.1 right?
    range_rainfall <- max_rain - min_rain
    max_cumulative_scaling<- max(1.1*rain_data$cumulative, na.rm = TRUE)/max_rain
  }

  # Scale fix for events with only one measurement interval
  #### This function is for a single event. Why are we plotting a single observation?
  if(nrow(rain_data) == 1) {
    max_cumulative_scaling <- max(rain_data$cumulative, na.rm = TRUE)/max_rain
  }

  # Calculate break intervals for y-axis
  #### Should these be hard-coded? Clean up with switch?
  if (range_rainfall > 0.5) {
    rain_major_interval <- 0.2
    rain_minor_interval <- 0.1
  } else {
    if(range_rainfall > 0.2) {
      rain_major_interval <- 0.1
      rain_minor_interval <- 0.05
    } else {
      rain_major_interval <- 0.05
      rain_minor_interval <- 0.01
    }}
  # Calculate major break intervals for x-axis
  #### This needs to be simplified. 
  # Event duration is < 4 days
  if(units(event_duration) == "days" & event_duration < 4) {
    # Set to 12-hour intervals
    x <- "12 hours"
  } else {
    # If duration is >= 4 days
    if(units(event_duration) == "days" & event_duration >= 4) {
      # Set x-axis major breaks to 1/4 days of the event duration
      x <- paste0(floor(event_duration/4)," days")
      } else {
        # If event duration is between 12 and 24hrs
        if(event_duration > 12) {
        # Set to 6hr interval
        x <- "6 hours"
      } else {
        # If the event duration is between 8 and 12hrs
        if(event_duration > 8) {
          # Set to 2hr interval
          x <- "2 hours"
          # Any duration < 8hrs
          } else {
          # Set to 1hr interval
          x <- "hour"
        }
      }
    }
  }
  # Calculations for dashed vertical line at day boundaries
  #### This seems a bit strange. I'm guessing we want to go from min day to min day with 00:00:00
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"),
                                               by = "day", length.out = 14),
                                    tz = "America/New_York")
  
  # Calculate axis breaks based on plotting limits
  # Select major x-axis breaks based on event duration
  major_date_breaks <- seq.POSIXt(day_marker[1], max_date, by = x)

  # Set minor x-axis breaks as 1hr
  minor_date_breaks <- seq.POSIXt(day_marker[1], max_date + lubridate::hours(6), by = "hour")

  # Add row for cumulative rainfall
  #### Why are we doing this? Replacing the last two values with new data?
  end <- data.frame(dtime = c(max_date - min_interval, max_date),
                    rainIN = c(0,0),
                    cumulative = c(max(rain_data$cumulative), max(rain_data$cumulative)))
  rain_data <- rbind(rain_data, end)

  # Determine scale function
  if (reverse_y == TRUE ) {
    y_scale_function <- ggplot2::scale_y_reverse
  } else {
    y_scale_function <- ggplot2::scale_y_continuous
  }
  # Filter by unique datetime.
  #### Why are we doing this? We essentially replaced the last two values, and then
  #### this gets rid of them? This will cause potentially unexpected results.
  rain_data <- rain_data %>% dplyr::distinct(dtime, .keep_all = TRUE)

  # Plot Hyteograph
  hyetograph <-
    ggplot2::ggplot(data = rain_data,
                    ggplot2::aes(x = dtime,
                                 y = cumulative/max_cumulative_scaling)) +
    # Culmulative Rainfall
    ggplot2::geom_area(ggplot2::aes(fill = "  Cumulative Rainfall    "),
                       color = "grey32",
                       alpha = 0.2) +
    # Rainfall
    ggplot2::geom_bar(data = rain_data,
                      ggplot2::aes(x = dtime,
                                   y = rainIN,
                                   fill = "  Rainfall"),
                      stat = "identity") +
    # Scale
    ggplot2::scale_fill_manual(values = c("slateblue1",
                                          "cornflowerblue"),
                               guide = ggplot2::guide_legend(title = NULL,
                                                             override.aes = list(
                                                               alpha = c(0.2,1)))) +

    # Day boundaries
    ggplot2::geom_vline(xintercept = day_marker,
                        color = "black",
                        linetype = "dashed",
                        linewidth = 1.2) +
    # Use B/W theme
    ggplot2::theme_bw() +
    # Scales
    ggplot2::scale_x_datetime(
      name = " ",
      labels = scales::date_format("%H:%M", "America/New_York"),
      limits = c(min_date - min_interval, max_date),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks
    ) +
    y_scale_function(
      breaks = seq(min_rain, max_rain, by = rain_major_interval),
      minor_breaks = seq(min_rain, max_rain, by = rain_minor_interval),
      sec.axis = ggplot2::sec_axis(~.*max_cumulative_scaling, name = "Cumulative Rainfall (in)")) +
    ggplot2::labs(
      y = "Rainfall (in)",
      title = title_text) +
    # Plot Theme
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 14, color = "black"),
      axis.text.y = ggplot2::element_text(size = 14, color = "black"),
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA),
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"),
      panel.grid.major =  ggplot2::element_line(colour = "grey70", linewidth = 0.2),
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", linewidth = 0.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10),
      legend.title=ggplot2::element_blank()
    )
}
# Pull legend from separate ggplots for combined plot ----------------------
# Function used for created combined legend in gridExtra
# Copied directly from this wiki (accessed May 8, 2019):
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930

#Description of the arguments:

#IN:  myggplot             ggplot object

#OUT: Combined legend

#' Get Legend
#' 
#' Pull legend from separate ggplots for combined plot
#' 
#' @param myggplot ggplot object
#' 
#' @return Returns a combined legend


get_legend<-function(myggplot) {
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# marsWaterLevelPlot ----------------------------------

#' Observed Water Level Plot
#'
#' Create a plot of observed water level for a single storm
#' 
#' @param  event                              Rainfall gage event ID 
#' @param  structure_name                     SMP ID and OW Suffix
#' @param  obs_datetime                       Vector of POSIXct datetimes for observed dataset
#' @param  obs_level_ft                       Vector of water level data (ft), corresponding to \code{obs_datetime}
#' @param  level_names                        Vector of names that corresponds to the number of comparison levels/dtimes
#' @param  datetime_2                         Vector of POSIXct datetimes for observed dataset used for comparison
#' @param  level_ft_2                         Vector of water level data (ft) used for comparison
#' @param  datetime_3                         Vector of POSIXct datetimes for observed dataset used for comparison
#' @param  level_ft_3                         Vector of water level data (ft) used for comparison
#' @param  datetime_4                         Vector of POSIXct datetimes for observed dataset used for comparison
#' @param  level_ft_4                         Vector of water level data (ft) used for comparison
#' @param  storage_depth_ft                   Maximum storage depth of system (ft)
#' @param  orifice_show                       TRUE if user wants to include the orifice height as dashed line on plot (optional)
#' @param  orifice_height_ft                  Orifice height, in feet (optional)
#' @param  metrics_show                       Bool, Default FALSE. TRUE if user wants to include a table of metrics on the plot (optional)
#' @param ...                                 additional arguments for showing metrics if metrics_show is set to TRUE. See \code{\link{marsMetricsTable}}
#' 
#' 
#' @return Output is a ggplot2 object of the water level plot.
#' 
#' @export


marsWaterLevelPlot <- function(event, 
                               structure_name, 
                               storage_depth_ft, 
                               obs_datetime, 
                               obs_level_ft,
                               level_names = NULL,
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
                               obs_infil_inhr,
                               obs_draindown_hr,
                               obs_overtopping) {
  
  #### Why are there more arguments than noted? Also, why so many?
  # Confirm that storage depth is explicitly defined
  if(!is.numeric(storage_depth_ft) | is.na(storage_depth_ft)) {
    stop("storage_depth is not numeric.")
  }
  # Set negative water levels to zero
  obs_level_ft[which(obs_level_ft < 0)] <- 0
  
  # Check that there is at least 1 level observation
  #### Do we really want to do this analysis with 1 level observation?
  if(length(obs_level_ft) == 0){
    stop(paste0("No data loaded in observed Event", event, "."))
  }
  
  #### There are no checks for any other data values. You can have level without any datetime
  # Check to make sure no gaps beyond 6 hours in time series
  prepseries <- obs_datetime %>%
    data.frame() %>%
    dplyr::mutate(lag_time = dplyr::lag(obs_datetime, 1)) %>%
    dplyr::mutate(gap_hr = difftime(obs_datetime, lag_time, units = "hours")) %>%
    dplyr::filter(gap_hr > 6)

  #### This doesn't seem very helpful.
  if(nrow(prepseries) > 0) {
    message(paste0("Warning: Missing values in observed time series."))
    warning_label <- "Warning: Missing values in observed time series."
  } else {
    warning_label <- ""
  }

  # Add orifice to plot
  if(orifice_show == TRUE) {
    #### Why are we changing this?
    orifice_plot <- orifice_height_ft
    orifice_lab <- paste0("orifice elevation: ",round(orifice_height_ft, 2))
  }else{
    #### This is why the x-axis has a thick black line which looks out of place.
    #### This should be removed.
    orifice_plot <- 0 #line will be covered by bottom of structure if option is not selected
  }

  # Set default names for levels if none are provided
  if(is.null(level_names)) {
    level_names <- c("Obs. Level 1",
                     "Obs. Level 2",
                     "Obs. Level 3",
                     "Obs. Level 4")
  }

  # Calculate plotting parameters

  # Minimum and maximum data values
  min_date <- min(obs_datetime, na.rm = TRUE)
  max_date <- max(obs_datetime, na.rm = TRUE)

  # Axis breaks by category
  event_duration <- max_date - min_date

  # Set date marker offset by duration
  if(units(event_duration) == "days") {
    marker_scale <- 0.02
    ## The duration of the event + 2 days?
    day_lengths <- event_duration + 2
  } else {
    marker_scale <- 0.015
    day_lengths <- 14
  }

  # Dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"),
                                               by = "day", length.out = day_lengths), tz = "America/New_York")

  # Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"),
                                           tz = "America/New_York")

  # #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12),
                                                      max_date + lubridate::hours(6), by = "hour"),
                                           tz = "America/New_York")

  #Generate title block
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::label_time("%Y-%m-%d %H:%M", tz = "America/New_York")(min_date),
                       sep = "")


  # Build dataframes
  # Full dataset
  obs_df <- data.frame(obs_datetime, obs_level_ft)
  # Each water level section
  if(!is.na(level_ft_2[1])) {
    obs2_df <- data.frame(datetime_2, level_ft_2)
  }
  if(!is.na(level_ft_3[1])) {
    obs3_df <- data.frame(datetime_3, level_ft_3)
  }
  if(!is.na(level_ft_4[1])) {
    obs4_df <- data.frame(datetime_4, level_ft_4)
  }

  # Generate plot
  level_plot <-
    ggplot2::ggplot(data = obs_df) +
    # Day boundaries
    ggplot2::geom_vline(xintercept = day_marker,
                        color = "black",
                        linetype = "dashed",
                        linewidth = 1.2) +
    ggplot2::annotate("text", x = day_marker-marker_scale*event_duration,
                      y = 0.8*storage_depth_ft,
                      label = day_marker,
                      angle = 90,
                      size = ggplot2::rel(5)) +
  #   #Warning message for data gaps in observed record
    ggplot2::annotate("text", x = day_marker[1]+1,
                      # Put warning halfway between up the storage depth
                      y = 0.5*storage_depth_ft,
                      label = warning_label, #empty if no warning
                      hjust = 0,
                      color = "red",
                      size = ggplot2::rel(5)) +
    #### Places a darker line across the x-axis? Why?
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
    # Top of storage
    ggplot2::geom_hline(yintercept = storage_depth_ft, color = "orange", linewidth = 1.2) +

    ggplot2::geom_label(x = min_date + event_duration/4,
                        y = storage_depth_ft*1.04,
                        #### Should this be called maximum storage depth or storage height?
                        label = "Maximum Storage Depth",
                        size = ggplot2::rel(5),
                        fill = "white",
                        label.size = 0) +

    #Observed water level
    ggplot2::geom_line(data = obs_df,
                       ggplot2::aes(x = obs_datetime,
                                    y = obs_level_ft,
                                    color = paste(level_names[1])),
                       linewidth = 2
    ) +

    # Formatting
    ggplot2::theme_bw() + # a basic black and white theme
    # Scales
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "America/New_York"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)), # set x axis limits
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, storage_depth_ft+1, by = if(storage_depth_ft > 2) round(storage_depth_ft/4, 0) else ceiling(storage_depth_ft/4)),
      minor_breaks = seq(-0.5,2*storage_depth_ft, by = 0.1)) +
    ggplot2::scale_color_manual(values = c("#7822E0","#E0DE43","#E03838","#E12CE0","#16E050")) +
    ggplot2::labs(
      y = "Water Level (ft)",
      title = title_text
    ) +
    ggplot2::theme(
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
  # Add level 2
  #### We already created the dfs for these, we don't need to check the arg anymore
  if(!is.na(level_ft_2[1])) {
    level_plot <- level_plot +
      ggplot2::geom_line(data = obs2_df,
                         ggplot2::aes(x = datetime_2,
                                      y = level_ft_2,
                                      color = paste(level_names[2])),
                         linewidth = 2
      )
  }
  if(!is.na(level_ft_3[1])) {
    level_plot <- level_plot +
      ggplot2::geom_line(data = obs3_df,
                         ggplot2::aes(x = datetime_3,
                                      y = level_ft_3,
                                      color = paste(level_names[3])),
                         linewidth = 2
      )
  }
  if(!is.na(level_ft_4[1])) {
    level_plot <- level_plot +
      ggplot2::geom_line(data = obs4_df,
                         ggplot2::aes(x = datetime_4,
                                      y = level_ft_4,
                                      color = paste(level_names[4])),
                         linewidth = 2
      )
  }

  if(orifice_show == TRUE ) {
    level_plot <- level_plot +
      ggplot2::geom_hline(yintercept = orifice_plot, color = "grey", linetype = 2, linewidth = 1.2) +
      ggplot2::geom_label(label = orifice_lab,
                          y = orifice_height_ft*1.1,
                          x = obs_datetime[round(0.75*length(obs_datetime))])

  }

  # Add metrics
  #### This is broken, needs to be determined if we want to fix it.
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
  #   level_plot %<>% marsMetricsTable( obs_RSPU = obs_RSPU,
  #                                     obs_infil_inhr = obs_infil_inhr,
  #                                     obs_draindown_hr = obs_draindown_hr,
  #                                     obs_overtopping = obs_overtopping,
  #                                     sim_RSPU = sim_RSPU,
  #                                     sim_infil_inhr = sim_infil_inhr,
  #                                     sim_draindown_hr = sim_draindown_hr,
  #                                     sim_overtopping = sim_overtopping)
  # }

level_plot
}


# marsCombinedPlot --------------------------------------------------------
#' Plot hyetograph and water level plot on the same chart
#'
#' Return hyetograph and observed and simulated (optional) water level plot for the same rain event on the same chart
#'
#' @param event                               chr, rain gage event UID
#' @param structure_name                      chr, SMP ID and OW Suffix for plot title
#' @param obs_datetime                        vector, POSIXct datetimes corresponding to \code{obs_level_ft}
#' @param obs_level_ft                        vector, water level data (ft), corresponding to \code{obs_datetime}
#' @param storage_depth_ft                    num, maximum storage depth of system (ft)
#' @param orifice_show                        TRUE if user wants to include the orifice height as dashed line on plot (optional)
#' @param orifice_height_ft                   Orifice height, in feet (optional)
#' @param rainfall_datetime                   vector, POSIXct datetimes corresponding to \code{rainfall_in}
#' @param rainfall_in                         vector, num, rainfall in inches corresponding to \code{rainfall_datetime}
#' @param metrics_show                        bool, Default FALSE. TRUE if user wants to include a table of metrics on the plot (optional)
#' @param ...                                 additional arguments for showing metrics (metrics_show = TRUE) and providing multiple water level time series. See \code{\link{marsMetricsTable}} and \code{\link{marsWaterLevelPlot}}
#'
#' @return Output will be a gridExtra object of the two plots
#'
#' @seealso \code{\link{marsRainfallPlot}}, \code{\link{marsWaterLevelPlot}}
#'
#' @export

marsCombinedPlot <- function(event, 
                             structure_name, # This is a really confusing name/description
                             obs_datetime, 
                             obs_level_ft,
                             storage_depth_ft,
                             orifice_show = FALSE,
                             orifice_height_ft = NULL,
                             rainfall_datetime,
                             rainfall_in,
                             metrics_show = FALSE,
                             obs_RSPU,
                             obs_infil_inhr,
                             obs_draindown_hr,
                             obs_overtopping) {
  #### This doesn't take the multiple levels that the marsWaterLevelPlot does?
  
  #Add a last date so the hyetograph looks better
  rainfall_in <- append(rainfall_in, 0)
  #### Why are we adding the last level datetime to rainfall?
  rainfall_datetime <- append(rainfall_datetime, max(obs_datetime))

  # Create individual plots
  level_plot <- marsWaterLevelPlot(event = event,
                                           structure_name = structure_name,
                                           obs_datetime = obs_datetime,
                                           obs_level_ft = obs_level_ft,
                                           storage_depth_ft = storage_depth_ft,
                                           orifice_show = orifice_show,
                                           orifice_height_ft = orifice_height_ft)
  # Default to reversing y-axis for rainfall
  rainfall_plot <- marsRainfallPlot(event = event,
                                    dtime = rainfall_datetime,
                                    rainfall_in = rainfall_in,
                                    reverse_y = TRUE)

  # Combine Plots
  # Save legends
  level_legend <- get_legend(level_plot)
  rainfall_legend <- get_legend(rainfall_plot)

  # Calculate minimum and maximum data values
    min_date <- min(obs_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, na.rm = TRUE)

  # Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  #set date marker offset by duration
  if(units(event_duration) == "days") {
    marker_scale <- 0.02
  } else {
    marker_scale <- 0.015
  }
  
  # Dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"), by = "day", length.out = 14), tz = "America/New_York")

  # X-axis breaks
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "America/New_York")
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12),
                                                      max_date + lubridate::hours(6),
                                                      by = "hour"), tz = "America/New_York")
  # Title
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "America/New_York")(min_date),
                       sep = "")

  # Remove legends and titles and update axes
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
      labels = scales::date_format("%H:%M", "America/New_York"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks)  +
    ggplot2::labs(title = title_text)
  #### Broken: See Issue #41
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
  # Calculate max width
  level_grob <- ggplot2::ggplotGrob(level_plot)
  rainfall_grob <- ggplot2::ggplotGrob(rainfall_plot)
  # 
  # Set max width
  maxWidth = grid::unit.pmax(level_grob$widths[2:9], rainfall_grob$widths[2:9])
  level_grob$widths[2:9] <- maxWidth
  rainfall_grob$widths[2:9] <- maxWidth

  # Arrange the plots and export
  combined_plot <- gridExtra::grid.arrange(rainfall_grob, level_grob,
                                           rainfall_legend, level_legend, 
                                           ncol = 1,
                                           heights = c(1.1, 2, 0.15, 0.15),
                                           newpage = TRUE)
  
  combined_plot
}

# marsEventCombinedPlot --------------------------------------------------------
#' Plot hyetograph and observed water level for a single plot the same chart with minimal inputs
#'
#' @param con                                 A connection to the MARS Analysis database 
#' @param event_date                          chr or POSIXCT, day during which event occurs
#' @param source                              chr, rainfall source, one of 'gage'/'gauge' or 'radar'. Defaults to 'radar'
#' @param event_uid                           int, rain event uid. Alternate to override event_date and rain_source (optional)
#' @param smp_id                              chr, SMP ID
#' @param ow_suffix                           chr, OW Suffix
#' @param sump_correct                        boolean, passed to fetch monitoring date to trim data
#' @param ...                                 additional arguments for showing metrics and overriding orifice/storage elevations. See \code{\link{marsCombinedPlot}}
#'
#' @return Output will be a gridExtra object of the two plots
#'
#' @seealso \code{\link{marsRainfallPlot}}, \code{\link{marsWaterLevelPlot}}, \code{\link{marsCombinedPlot}}
#'
#' @export

marsEventCombinedPlot <- function(con,
                                  event_date,
                                  #### Why are we defaulting to something we rarely use
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
                                  obs_infil_inhr,
                                  obs_draindown_hr,
                                  obs_overtopping) {
  
  #### This function is very different than it's components. Should it be?
  ## Check DB connection
  if(!DBI::dbIsValid(con)) {
    stop("Argument 'con' is not an open database connection")
  }
  
  # Was a string supplied to source?
  #### This should be not equal to make sure the right value is passed
  if(isTRUE(all.equal(source, c("gage", "radar")))) {
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  # Fetch Data
  
  # Observation well data
  #### This shouldn't be hard-coded
  #### Why are we using like in this context?
  ow_query <- paste0("SELECT ow_uid FROM fieldwork.tbl_ow
                     WHERE smp_id LIKE '", smp_id,
                     "' AND ow_suffix LIKE '", ow_suffix, "'")
  
  ow_uid <- DBI::dbGetQuery(con, ow_query) |> dplyr::pull()
  
  # Check if ow exists
  if(length(ow_uid) == 0) {
    stop("OW does not exist within the fiedlwork database.")
  }
  
  # Get event_uid if not supplied
  if(missing(event_uid)) {


    # check for one day on either side of the event date
    #### This doesn't really capture a full day before and after
    #### It captures start_date 1, start_date, start_date + 1 hr (00:00:00)
    event_date %<>% as.POSIXct(format = '%Y-%m-%d')
    start_date <- event_date - 86400
    end_date <- event_date   + 86400
    
    # Grab the data
    mon_data <- marsFetchMonitoringData(con = con,
                                        target_id = smp_id,
                                        ow_suffix = ow_suffix,
                                        start_date = start_date,
                                        source = source,
                                        end_date = end_date,
                                        sump_correct = sump_correct)

    event_data <- mon_data$`Rain Event Data`
    
    # Get event_id based on source 
    if(source %in% c('gage','gauge')) {
      event_uid <- event_data$gage_event_uid
    }
  
    if(source == 'radar') {
      event_uid <- event_data$radar_event_uid
    }

    # Stop if no events have been found
    if(nrow(event_data) == 0) {
      stop(paste0("There are no events on ",event_date))
    }
  
  # Select closest event if multiple events
  #### This is a really confusing way of doing it. min_dif should just be dif
  #### and then it would be dif - min(dif)
  if(nrow(event_data) > 1) {
    min_dif <- min(abs(event_data$eventdatastart_est - event_date))
    event_data <- event_data[abs(event_data$eventdatastart_est - event_date) == min_dif,]
    }
    # If event_uid argument is provided
    } else {
      # Event query based on source and event_uid
      if(source %in% c('gage','gauge')) {
        event_query <- paste0('SELECT * FROM data.tbl_gage_event where gage_event_uid = ', 
                              event_uid)
      }
      
      if(source == 'radar') {
        event_query <- paste0('SELECT * FROM data.tbl_radar_event where radar_event_uid = ',
                              event_uid)
      }
      # Set event_data start and end dates
      #### This will return different results than if it was only given a day.
      event_data <- dbGetQuery(con, event_query)
      event_date <- event_data$eventdatastart_edt %>% as.Date()
      start_date <- event_data$eventdatastart_edt %>% as.Date()
      end_date <- event_data$eventdataend_edt %>% as.Date()
  
      # Get monitoring data
      mon_data <- marsFetchMonitoringData(con = con,
                                          target_id = smp_id,
                                          ow_suffix = ow_suffix,
                                          start_date = start_date,
                                          source = source,
                                          end_date = end_date,
                                          sump_correct = sump_correct)

    }

  # Get event column name
  #### Why does the event_id need to be associated with it being radar or gage?
  event_col <- colnames(mon_data$`Level Data`)[grep('event',colnames(mon_data$`Level Data`))]
 
  # Filter for rainfall has a event_uid
  rainfall_data <- mon_data$`Rainfall Data`[!is.na(mon_data$`Rainfall Data`[,event_col]), ]
  # Filter again for event_id in event_data
  #### This seems like should be done in one step
  rainfall_data <- rainfall_data[rainfall_data[,event_col] == event_data[,event_col], ]

  #### Not sure what this means: # make sure we capture at least three points before the water level response
  #### This doesn't seem to work in practice. See issue
  # Isolate level data
  level_data <- mon_data$`Level Data`
  # Compare the difference between the value a lag-3 value
  # Include 3 zeros at the end so that it isn't NA
  level_data$lvl_lag <- c(diff(level_data$level_ft, 3),0,0,0)
  # Absolute value of the lagged amount from the level
  level_data <- level_data %>% dplyr::mutate(lageql = abs(level_ft - lvl_lag)) %>%
    # TRUE/FALSE if there is an event
    dplyr::mutate(isevent = !is.na(level_data[,event_col])) %>%
    # Filter out any obs with lagegl == 0 AND isEvent is FALSE
    dplyr::filter(lageql != 0 | isevent) %>%
    # Remove lag columns
    dplyr::select(-lvl_lag, -lageql,-isevent)
  
  # Filter out obs with NA for event_uids
  level_data <- mon_data$`Level Data`[!is.na(mon_data$`Level Data`[, event_col]), ]
  # Filter only based on specific event_uid
  level_data <- level_data[level_data[, event_col] == event_data[, event_col], ]

  # Match inputs to marsCombinedPlot
  rainfall_in <- rainfall_data$rainfall_in
  rainfall_datetime <- rainfall_data$dtime
  obs_level_ft <- level_data$level_ft
  obs_datetime <- level_data$dtime

  # Replace missing arguments with values from snapshot
  #### What if those values aren't in the snapshot?
  snapshot <- marsFetchSMPSnapshot(con = con,
                                   smp_id = smp_id,
                                   ow_suffix = ow_suffix,
                                   request_date = event_date)

  # Replace NA with 0
  if(is.na(snapshot$orificedepth_ft)) {
    snapshot$orificedepth_ft <- 0
  }

  # Use default values if NA in snapshot
  #### Why are we changing orifice show from T/F to 0 or 1?
  if(missing(orifice_show)) {
    if(snapshot$orificedepth_ft == 0) {
      orifice_show <- 0
    } else {
      orifice_show <- 1}
  }

  if(missing(orifice_height_ft)) {
    #### Early if it's NA in the snapshot, we make it 0 in the snapshot
    #### Now if it's not provided as an argument, it is taken from the snapshot
    #### We probably should combine these in order to simplify
    orifice_height_ft <- snapshot$orificedepth_ft
  }

  if(missing(storage_depth_ft)) {
    storage_depth_ft <- snapshot$storage_depth_ft
  }

  # Combine strings for structure name
  structure_name <- paste0(smp_id," | Monitoring Location: ",ow_suffix)

  #Add a last date so the hyetograph looks better
  #### Why? This adds 0" of rain to the end of the rainfall_in vector
  rainfall_in <- append(rainfall_in, 0)
  #### This adds the last datetime of the level to the rainfall_datetime vector
  rainfall_datetime <- append(rainfall_datetime, max(obs_datetime))

  # Plot level and rainfall separately
  level_plot <- marsWaterLevelPlot(event = event_data$event_uid[1],
                                         structure_name = structure_name,
                                         obs_datetime = obs_datetime,
                                         obs_level_ft = obs_level_ft,
                                         storage_depth_ft = storage_depth_ft,
                                         orifice_show = orifice_show,
                                         orifice_height_ft = orifice_height_ft)

  rainfall_plot <- pwdgsi::marsRainfallPlot(event = event_data$event_uid[1],
                                            dtime = rainfall_datetime,
                                            rainfall_in = rainfall_in,
                                            reverse_y = TRUE)

  # Combine Plots
  # Legends
  level_legend <- cowplot::get_legend(level_plot)
  rainfall_legend <- cowplot::get_legend(rainfall_plot)
  
  # Calculate plotting limits
  min_date <- min(obs_datetime, na.rm = TRUE)
  max_date <- max(obs_datetime, na.rm = TRUE)

  # Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date

  # Set date marker scale using event_duration
  if(units(event_duration) == "days") {
    marker_scale <- 0.02
  } else {
    marker_scale <- 0.015
  }

  # Day boundaries for 14 days after start
  #### Why are we doing 14 days after start?
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "America/New_York"),
                                               by = "day", length.out = 14),
                                    tz = "America/New_York")

  # 12-hour x-axis major breaks
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"),
                                           tz = "America/New_York")

  # 1-hour x-axis minor breaks
  #### This starts 12 hours before the series, and ends 6 hours after
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12),
                                                      max_date + lubridate::hours(6),
                                                      by = "hour"),
                                           tz = "America/New_York")

  # Title
  title_text <- paste0("Water Level and Rainfall\nSMP ID: ", structure_name,
                       " | Event: ", event_uid,
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(min_date),
                       sep = "")

  # Remove legends and titles and update axes
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
      labels = scales::date_format("%H:%M", "America/New_York"),
      #### Why are we further extending the limits?
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks)  +
    ggplot2::labs(title = title_text)
  
  #### Broken see issue #41
  # if( missing(metrics_show) ){metrics_show <- FALSE}
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
  # Max widths
  level_grob <- ggplot2::ggplotGrob(level_plot)
  rainfall_grob <- ggplot2::ggplotGrob(rainfall_plot)
  maxWidth = grid::unit.pmax(level_grob$widths[2:9], rainfall_grob$widths[2:9])
  level_grob$widths[2:9] <- maxWidth
  rainfall_grob$widths[2:9] <- maxWidth

  #Arrange the plots and export
  combined_plot <- gridExtra::grid.arrange(rainfall_grob, level_grob, #plots
                                           rainfall_legend, level_legend, #legends
                                           ncol = 1,
                                           heights = c(1.1, 2, 0.15, 0.15))
  combined_plot
}



# marsBaroRasterPlot --------------------------------------------------------
#' Barometric Pressure Raster Plot
#' 
#' Create a raster plot of barometric pressures from each sensor for each day
#' 
#' @param baro a dataframe with columns: \code{smp_id, baro_psi, day, year}
#' 
#' @return p, a ggplot2 plot
#' 
#' @export
#' 
#' @examples 
#' marsSampleBaro_plot %<>% dplyr::mutate("day" = yday_decimal(marsSampleBaro_plot$dtime),
#'                                "year" = lubridate::year(marsSampleBaro_plot$dtime))
#' marsBaroRasterPlot(marsSampleBaro_plot)
#'

marsBaroRasterPlot <- function(baro){
  p <- ggplot2::ggplot(baro, ggplot2::aes(x = day, y = smp_id)) +
    ggplot2::facet_grid(. ~ year) +
    ggplot2::geom_tile(ggplot2::aes(fill = baro_psi)) +
    ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu")), name = "Pressure (psi)") +
    ggplot2::theme(axis.text=ggplot2::element_text(colour="black", size=15),
                   axis.title.x=ggplot2::element_text(colour="black", size=15),
                   axis.title.y=ggplot2::element_text(colour="black", size=15),
                   legend.text=ggplot2::element_text(size=15),
                   legend.title=ggplot2::element_text(size = 15),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +
    ggplot2::xlab("Day") + ggplot2::ylab("Baro Sites")
  
  return(p)
  
}


# marsOvertoppingPlot ------------------------------------------------------------

#' Add metrics to an existing water level or combined plot
#'
#' Return the gpglot object, with the metrics added to the object as a tableGrob annotation
#'
#' @param data                                dataframe, a data frame with a unique row each containing ow_uid, radar_event_uid, ow_suffix, eventdatastart, smp_id, eventavgintensity_inhr, eventpeakintensity_inhr, eventdepth_in, and overtop
#' @param design_storm                        num, a numeric value for the design storm in inches, see \code{marsFetchSMPSnapshot}
#' @param event_dates                         date, a vector of dates to show up as major events in the monitoring locations history (maintenance, retrofit, etc.)
#' @param event_descriptions                  char, a vector of strings corresponding to labels fo each major event on the plot (eg, "pipe jetting", "filter bag replaced") 
#' 
#' @return Output ggplot object plotting overtopping, event peak intensity, and date
#'  
#' @export
#' 
marsOvertoppingPlot <- function(data,
                                design_storm,
                                event_dates = NULL,
                                event_descriptions = NULL){
  
  
  min_date <- min(data$eventdatastart_edt)
  max_date <- max(data$eventdatastart_edt)
  
  #Typical date formats - allows for entry of either date-time or date to function
  date_formats <- c(lubridate::guess_formats(data$eventdatastart_edt, "mdy HMS"),
                    lubridate::guess_formats(data$eventdatastart_edt, "mdy"))
  
  if(!is.null(event_dates)){
    event_dates <- event_dates %>% lubridate::as_datetime(format = date_formats)    
  }
  
  
  #Set overtop to sizes
  data$overtop_sz[data$overtop == FALSE] <- as.numeric(2)
  data$overtop_sz[data$overtop == TRUE] <- as.numeric(4)
  data$overtop_col[data$overtop == FALSE] <- "#899DA4"
  data$overtop_col[data$overtop == TRUE] <- "#C93312"
  
  #subset of data exceeding design storm
  data_ovr_design <- data %>% dplyr::filter(eventdepth_in > design_storm)
  data <- data %>% dplyr::mutate("ExceedDesignStorm" = ifelse(eventdepth_in > design_storm,"True",NA))
  # y-max value
  ymax_obs <- max(data$eventpeakintensity_inhr, na.rm = TRUE)
  
  
  plot_x <- ggplot2::ggplot(data,
                            ggplot2::aes(x = eventdatastart_edt,
                                         y = eventpeakintensity_inhr)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(overtop_col),
                                     size = factor(overtop_sz))) +
    ggplot2::geom_hline(yintercept = 2.5, color = "#DC863B", size = 1.5) +
    ggplot2::scale_y_continuous(limits = c(0,max(3.6,ymax_obs)), minor_breaks =seq(0,max(3,ymax_obs),0.2)) +
    ggplot2::scale_x_datetime(date_minor_breaks = "2 months") +
    ggplot2::ylab("Event Peak Intensity (in/hr)") + ggplot2::xlab("Event Date/Time") +
    ggplot2::geom_text(label = "Philadelphia 1-year, 15-minute Peak Intensity: 2.5 in/hr",
                       y = 2.6, color = "black", size = 12 / .pt, hjust = "left",
                       x = data$eventdatastart_edt[round(0.05*length(data$eventdatastart_edt))]) +
    ggplot2::ggtitle(paste0("Event Peak Intensity and Overtopping vs Time for ",data$smp_id[1])) +
    
    #add design storm values
    ggplot2::geom_point(ggplot2::aes(x = eventdatastart_edt,
                                     y = eventpeakintensity_inhr, size = factor(overtop_sz),  color = factor(overtop_col), shape = factor(ExceedDesignStorm))) +
    ggplot2::scale_shape_manual(name = paste0("Exceeds Design Storm Depth: ",round(design_storm,2)," in"), values = c(2), labels = c("True"), na.translate = FALSE) +
    ggplot2::scale_color_manual(name = "Overtopping", values = c("#899DA4", "#C93312"), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    ggplot2::scale_size_manual(name = "Overtopping", values = c(2,4), labels = c("False","True"), guide = guide_legend(reverse = TRUE)) +
    #from pwdgsi plots
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "whitesmoke", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.5), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)))
  
  if(length(event_dates) > 0 & length(event_descriptions) > 0){
    for(i in 1:length(event_dates)){
      plot_x <- plot_x + ggplot2::geom_vline(xintercept = event_dates[i], color = "#DC863B", size = 1.1, linetype = "dashed") +
        ggplot2::geom_text(label = event_descriptions[i], angle = 90,
                           y = 2.7, color = "black", size = 12 / .pt, hjust = "left",
                           x = as.numeric(event_dates[i] + days(8)))
    }
    
  }
  
  
  return(plot_x)
  
}


# marsMetricsTable ------------------------------------------------------------

#' Add metrics to an existing water level or combined plot
#'
#' Return the gpglot object, with the metrics added to the object as a tableGrob annotation
#'
#' @param in_plot                             ggplot object without annotative metrics table 
#' @param obs_RSPU                            num, Metric: Observed relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param obs_infil_inhr                      num, Metric: Observed infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param obs_draindown_hr                    num, Metric: Observed draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param obs_overtopping                     bool, Metric: Observed overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' @param sim_RSPU                            num, Metric: Simulated relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param sim_infil_inhr                      num, Metric: Simulated infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param sim_draindown_hr                    num, Metric: Simulated draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param sim_overtopping                     bool, Metric: Simulated overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' 
#' @return Output ggplot object adding metrics when necessary
#'  
#' @export
#' 

marsMetricsTable <- function(in_plot,
                             obs_RSPU,
                             obs_infil_inhr,
                             obs_draindown_hr,
                             obs_overtopping,
                             sim_RSPU,
                             sim_infil_inhr,
                             sim_draindown_hr,
                             sim_overtopping){
  
  #set missing values to ""
  if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
  if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
  if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
  if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
  if( missing(obs_RSPU) ){obs_RSPU <- ""}
  if( missing(sim_RSPU) ){sim_RSPU <- ""}
  if( missing(obs_overtopping) ){obs_overtopping <- ""}
  if( missing(sim_overtopping) ){sim_overtopping <- ""}
  
  # browser()
  if( is.numeric(obs_infil_inhr) & obs_infil_inhr < 0 ){
    obs_infil_inhr <- paste0("ERR: ", obs_infil_inhr)
  }
  if( is.numeric(sim_infil_inhr) & sim_infil_inhr < 0 ){
    sim_infil_inhr <- paste0("ERR: ", sim_infil_inhr)
  }
  
  if(is.numeric(obs_draindown_hr)){ obs_draindown_hr <- round(obs_draindown_hr,2) %>% as.character}
  if(is.numeric(sim_draindown_hr)){ sim_draindown_hr <- round(sim_draindown_hr,2) %>% as.character}
  if(is.numeric(obs_infil_inhr)){ obs_infil_inhr <- round(obs_infil_inhr,2) %>% as.character}
  if(is.numeric(sim_infil_inhr)){ sim_infil_inhr <- round(sim_infil_inhr,2) %>% as.character}
  if(is.numeric(obs_RSPU)){ obs_RSPU <- round(obs_RSPU,2) %>% as.character}
  if(is.numeric(sim_RSPU)){ sim_RSPU <- round(sim_RSPU,2) %>% as.character}
  
  # browser()
  #------ table version
  metric_table <- as.data.frame(matrix(nrow=4))
  colnames(metric_table) <- "Metrics"
  metric_table$Metrics <- c("Drain down (hrs)",
                            "Infiltration rate (in/hr)",
                            "RSPU (%)",
                            "Overtopping (T/F)")
  
  obs_mets <- c(obs_draindown_hr,obs_infil_inhr, obs_RSPU, obs_overtopping)
  sim_mets <- c(sim_draindown_hr,sim_infil_inhr, sim_RSPU, sim_overtopping)
  
  #add columns if obs/sim exists
  if(sum(obs_mets == '') < 4){metric_table$Observed <- obs_mets}
  if(sum(sim_mets == '') < 4){metric_table$Simulated <- sim_mets}
  
  #remove rows if both are empty
  remove <- c()
  if(sum(metric_table[1,] == "") == ncol(metric_table)-1){ remove <- c(remove,1)}
  if(sum(metric_table[2,] == "") == ncol(metric_table)-1){ remove <- c(remove,2)}
  if(sum(metric_table[3,] == "") == ncol(metric_table)-1){ remove <- c(remove,3)}
  if(sum(metric_table[4,] == "") == ncol(metric_table)-1){ remove <- c(remove,4)}
  
  metric_table <- metric_table[c(1:4)[!(c(1:4) %in% remove)],]
  
  
  # determine if we're working with a ggplot object or a grob
  if('ggplot' %in% class(in_plot)) plot_type <- "ggplot" else plot_type <- "grob"
  
  
  if(plot_type == 'ggplot'){
    # get ymax value
    ymax_val <- max(ggplot_build(in_plot)$layout$panel_scales_y[[1]]$range$range)
    
    #add table to plot
    out_plot <- in_plot +
      ggplot2::annotation_custom (grob = gridExtra::tableGrob(metric_table,
                                                              rows = NULL,
                                                              theme = ggpp::ttheme_gtlight()),
                                  ymin = (ymax_val*0.70),
                                  ymax = (ymax_val*0.95),
                                  xmin = in_plot$data$obs_datetime[round(length(in_plot$data$obs_datetime)*0.5)],
                                  xmax = in_plot$data$obs_datetime[round(length(in_plot$data$obs_datetime))])
  }
  
  if(plot_type == 'grob'){
    
    # add the table in the water level grob
    wl_grob <- in_plot$grobs[[2]] %>% ggpubr::as_ggplot()
    
    in_plot$grobs[[2]] <- (wl_grob +
                             ggplot2::annotation_custom (grob = gridExtra::tableGrob(metric_table,
                                                                                     rows = NULL,
                                                                                     theme = ggpp::ttheme_gtlight()),
                                                         ymin = .7,
                                                         ymax = .9,
                                                         xmin = 0.5,
                                                         xmax = 0.95)) %>%  ggplot2::ggplotGrob()
    out_plot <- in_plot
    
  }
  
  
  return(out_plot)
  
  
  
}


# marsSavePlot ------------------------------------------------------------

#' Wrapper function for ggsave that ensures text appears correctly for saved images of pwdgsi plots 
#'
#' Return the gpglot object, with the metrics added to the object as a tableGrob annotation
#'
#' @param in_plot                             ggplot object without annotative metrics table 
#' @param filename                            text, file location to save the document refer to \code{ggsave}
#' @param plot_type                           text, one of 'combined', 'level', or 'rain' to determine the dimensions to use
#' @param resolution                          text, one of 'low', 'med', or 'high'. Each consists of default options that can be overwritten with ggsave arguments.
#' @param ...                                 overwrite assumed values of resolution. Accepts existing arguments to ggsave, see \code{ggsave} (optional)
#' @return Output ggplot object adding metrics when necessary
#'  
#' @export
#' 

marsSavePlot <- function(in_plot = last_plot(),
                         plot_type,
                         filename,
                         resolution = 'med',
                         ...){
  
  if(!(resolution %in% c('low','med','high'))){
    stop("Must provided one of 'low', 'med', or 'high' to argument 'resolution'. Values can be overwrittn by providing ggsave arguments.")
  }
  
  #resolution options
  res_df <- data.frame(name = c('low','med','high'),
                       dpi = c(75, 150, 250))
  
  res_x <- res_df$dpi[res_df$name == resolution]
  
  if(is.na(plot_type)){
    stop("Must provided one of 'level', 'rain', or 'combined' to argument 'plot_type'.")
  }
  
  if(!(plot_type %in% c('level','rain','combined'))){
    stop("Must provided one of 'level', 'rain', or 'combined' to argument 'plot_type'.")
  }
  
  #plot dimensions
  plot_df <- data.frame(type = c('level','rain','combined'),
                        width = c(8, 9.56, 10.67),
                        height = c(4, 4,8))
  
  width_x <- plot_df$width[plot_df$type == plot_type]
  height_x <- plot_df$height[plot_df$type == plot_type]
  
  ggplot2::ggsave(plot = in_plot,
                  width = width_x,
                  height = height_x,
                  dpi = res_x,
                  filename = filename)
}
