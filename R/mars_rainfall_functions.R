# marsDetectEvents -----------------------------------------
# NOTES: Based on a function written by Taylor Heffernan (see "marsDetectEvents.r" and related email from 4/5/18,
# modified by Katie Swanson 2/4/2019) returns a dataset of event IDs for a rainfall time
# series. Additional edits after review from Taylor Heffernan by Tim Adams and Katie Swanson (3/4/2019)
#
# IN: dtime A vector of POSIXct date times, in ascending order
# IN: rainfall_in Rainfall depths during periods corresponding to times in  dtime, in inches
# IN: iet_hr Interevent time, in hours
# IN: mindepth_in Minimum depth of a given event, in inches
# OUT: A vector of integers the same length as dtime, which represents the event ID for that time step.

# roxygen
#' Identify individual rainfall events
#'
#' Return a dataset of rainfall event IDs for a time period
#'
#' @param dtime vector, POSIXct date times
#' @param rainfall_in vector, num, of rainfall depths corresponding to \code{dtime}, in inches
#' @param iet_hr num, Interevent time, in hours. The default is 6 hours.
#' @param mindepth_in num, minimum depth of a given event, in inches. The default is 0.10 inches.
#'
#' @return Output will be a vector of integers corresponding to \code{dtime} and representing
#'   the event ID for each time step.
#'   
#' @details Function should be used inside \code{\link[dplyr]{mutate}} to add output to the corresponding table.    
#'
#' @export
#' 
#' @examples
#' gage_temp <- dplyr::mutate(marsSampleRain, 
#'   event_id = marsDetectEvents(dtime_est = marsSampleRain$dtime_est, 
#'   rainfall_in = marsSampleRain$rainfall_in, 
#'   iet_hr = 6, mindepth_in = 0.10))

marsDetectEvents <- function(dtime, rainfall_in, 
                         iet_hr = 6, mindepth_in = 0.10) {
  
  # Check for non-zero and negative rainfall values
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0.")
  }

  # Check that datetime is in ascending order
  if(!identical(order(dtime), 1:length(dtime))) {
    stop("Datetime data is not sorted in ascending order.")
  }

  # Check for duplicated data
  if(!all(!duplicated(dtime))) {
    stop("Datetime data cannot contain duplicates.")
  }
  
  # Check that datetime is in correct format
  if(!(class(dtime)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }
  #### So this should always match up beforehand?
  # Check to make sure paired data matches
  if(!(length(dtime) == length(rainfall_in))) {
    stop("dtime and rainfall_in must be the same length")
  }
  #### Should this be an argument?
  # Assumed interval
  interval_sec <- 15 * 60
  
  #### Why do we change the names? Should we ask for individual vectors or a df so we don't need this step?
  # Process rainfall data
  prepseries <- tibble::tibble(dtime = dtime,
                            rf_in = rainfall_in) %>%
    dplyr::mutate(lag_time = dplyr::lag(dtime, 1, default = dplyr::first(dtime) - interval_sec)) %>%
    dplyr::mutate(gap_hr = difftime(dtime, lag_time, units = "hours"))

  min_interval <- min(prepseries$gap_hr, na.rm = TRUE)

  # Identify events
  #### We're filling this column with 0s?
  # Initialize column
  prepseries$start <- 0

  # Check whether first measurement in row 1 is included in following event
  prepseries$start[1] <- ifelse(prepseries$gap_hr[2] < iet_hr, 1, 0)


  # Indicate the beginning of new events with a 1
  prepseries$start[prepseries$gap_hr >= iet_hr + min_interval] <- 1

  # Generate series of new events
  prepseries <- prepseries %>%
    dplyr::mutate(event = cumsum(start))

  # Identify events that are greater than the minimum event threshold
  prepsums <- prepseries %>%
    dplyr::group_by(event) %>%
    dplyr::summarize(total_in = sum(rf_in)) %>%
    dplyr::filter(total_in >= mindepth_in-0.0000001) %>%
    # note: subtracted from minimum threshold because spot check indicated that
    # some events at the threshold were not being included in the event
    # detection (but not all). Probably a floating point issue.
    dplyr::mutate(event_id = 1:dplyr::n()) # all events that are greater than the min depth

  # 3.6 Join event summary to rainfall data
  output <- prepseries %>%
    dplyr::left_join(prepsums, by = "event") %>%
    dplyr::select(dtime,
           rainfall_in = rf_in,
           event_id)

  return(output$event_id)
}

# marsStormDepth_in ------------------------------------
# NOTES: Function to export storm depth from events processed using marsDetectEvents function
#
# IN: rainfall_in A vector of rainfall depths for one storm
# OUT: The total rainfall depth, in inches

#'
#' Return storm attributes
#'
#' Return storm depth, duration, average intensity, and peak intensity of an event processed using \code{\link{marsDetectEvents}}.
#'
#'
#'
#' @name storm
#' @rdname storm
#'
#' @param rainfall_in vector, num, rainfall depth in inches representing a single rain event
#'
#' @return \describe{
#'        \item{\code{marsStormDepth_in}}{Output will be total rainfall depth for the event, in inches.}
#' }
#'
#' @seealso \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{summarise}},
#'  \code{\link[dplyr]{select}}
#'  
#' @export
#' 
#' @examples 
#' rain_newevents <- marsSampleRain %>%  #use dplyr pipe to update dataframe
#'  dplyr::group_by(gage_uid) %>% 
#'   dplyr::arrange(dtime_est) %>% 
#'   dplyr::mutate(event_id = marsDetectEvents(dtime_est, rainfall_in)) %>%
#'   dplyr::group_by(gage_uid, event_id) %>%
#'   dplyr::summarize(eventdatastart_edt = dplyr::first(dtime_est),
#'             eventdataend_edt = dplyr::last(dtime_est),
#'             eventduration_hr = marsStormDuration_hr(dtime_est),
#'             eventpeakintensity_inhr = marsStormPeakIntensity_inhr(dtime_est, rainfall_in),
#'             eventavgintensity_inhr = marsStormAverageIntensity_inhr(dtime_est, rainfall_in),
#'             eventdepth_in = marsStormDepth_in(rainfall_in))
 

marsStormDepth_in <- function(rainfall_in) {
  # Return NA if rainfall_in is zero
  if(length(rainfall_in) == 0){
    return(NA)
  }

  # Validatie all rainfall is > 0
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0.")
  }

  # Calculate stormDepth
  return(sum(rainfall_in))
}

# marsStormDuration ------------------
# NOTES: Function to export storm duration from events processed using marsDetectEvents function

#IN: A vector of times at which rainfall was collected in the storm for a single storm
#OUT: The total rainfall duration, in hours

#' @rdname storm
#'
#' @param dtime vector of POSIXct datetimes 
#' @return \describe{
#'        \item{\code{marsStormDuration_hr}}{Output will be a double with the duration of the event, in hours.}
#' }
#'
#' @export

marsStormDuration_hr <- function(dtime) {
  # Return NA if dtime vector has no values
  if(length(dtime) == 0){
    return(NA)
  }

  # Make sure 
  if(!identical(order(dtime), 1:length(dtime))) {
    stop("Datetime data is not sorted in ascending order.")
  }
  
  identical(order(dtime), 1:length(dtime))
  if(!all(!duplicated(dtime))) {
    stop("Datetime data can not contain duplicates.")
  }

  if(!(class(dtime)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }

  # 2. Calculate storm duration
  event_start <- dtime[1]
  #Notes: Assumes 15-minute time increments. As written, code should
  #only be applied to ONE EVENT at a time

  duration_calc <- difftime(dtime[length(dtime)], event_start, units = "hours")
  duration <- as.double(duration_calc) + 0.25
  #15-minutes added to account for time subtraction

  return(round(as.double(duration), 4))
}


# marsStormPeakIntensity -----------------------------
# NOTES: Function to export storm peak intensity from events processed using marsDetectEvents function
#
# IN:  rainfall_in The depth of water that fell at each time, in inches
# OUT:  The peak intensity, in inches per hour

#' @rdname storm

#' @param rainfall_in a vector of rainfall, in inches, for a given storm
#'
#' @return \describe{
#'        \item{\code{marsStormPeakIntensity_inhr}}{Output will be a number representing the event's peak intensity in inches/hour.}
#' }
#'
#' @export

marsStormPeakIntensity_inhr <- function(rainfall_in) {
  # If length of rainfall_in argument is zero, return NA
  if(length(rainfall_in) == 0){
    return(NA)
  }
  # Validate all rainfall obs are > 0
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0")
  }
  
  # Multiplier to convert interval from 15min intervals to hours
  mult <- 4
  
  # Calculate peak intensity for a given hour
  maximum <- max(rainfall_in)
  peak <- maximum * mult

  return(round(peak, 4))
}

# marsStormAvgIntensity -------------------------------
# NOTES: Function to export storm average intensity from events processed using marsDetectEvents function

# IN: dtime_est A vector of times at which rainfall was collected in the storm
# IN: rainfall_in The depth of water that fell at each time, in inches
# OUT:  The average intensity over the length of the storm, in inches per hour

#' @rdname storm
#'
#' @param dtime Vector of datetimes for a given storm
#' @param rainfall_in Vector of rainfall_in for a given storm (in inches)
#' @return \describe{
#'        \item{\code{marsStormAverageIntensity_inhr}}{Output will be a number representing the event's average intensity in inches/hour.}
#' }
#'
#' @export

marsStormAverageIntensity_inhr <- function(dtime, rainfall_in) {

  if(length(dtime) == 0 | length(rainfall_in) == 0){
    return(NA)
  }

  # 1. QC check (all others covered in called functions)
  if(!(length(dtime) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }

  # 2. Calculate average intensity
  result <- marsStormDepth_in(rainfall_in) / marsStormDuration_hr(dtime)
  return(round(result, 4))
}

