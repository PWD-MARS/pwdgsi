% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars_data_fetch_write_functions.R
\name{marsFetchMonitoringData}
\alias{marsFetchMonitoringData}
\title{Fetch monitoring data for an SMP}
\usage{
marsFetchMonitoringData(
  con,
  target_id,
  ow_suffix,
  source = c("gage", "radar"),
  start_date,
  end_date,
  sump_correct = TRUE,
  rain_events = TRUE,
  rainfall = TRUE,
  level = TRUE,
  daylight_savings = FALSE,
  debug = FALSE
)
}
\arguments{
\item{con}{An ODBC connection to the MARS Analysis database returned by odbc::dbConnect}

\item{target_id}{vector of chr, SMP ID, where the user has requested data}

\item{ow_suffix}{vector of chr, OW suffixes corresponding to SMP IDs, where the user has requested data}

\item{source}{string, one of "gage" or "radar" to select rain gage events or radar rainfall events}

\item{start_date}{string, format: "YYYY-MM-DD", start of data request range}

\item{end_date}{string, format: "YYYY-MM-DD", end of data request range}

\item{sump_correct}{logical, TRUE if water level should be corrected for to account for sump depth}

\item{rain_events}{logical, TRUE if rain event data should be included in result}

\item{rainfall}{logical, TRUE if rainfall data should be included in result}

\item{level}{logical, TRUE if water level should be included in result}

\item{daylight_savings}{logical, WILL NOT WORK in current version}

\item{debug}{logical, whether to print lookup times and outputs}
}
\value{
Output will be a list consisting of the follow elements filtered by level data:

    \item{Rain Event Data}{dataframe, output from \code{\link{marsFetchRainEventData}}}
    \item{Rain Gage Data}{dataframe, output from \code{\link{marsFetchRainfallData}}}
    \item{Level Data}{dataframe, output from \code{\link{marsFetchLevelData}}, plus rainfall_gage_event_uids}
}
\description{
Returns a list with data frames: Rain Event Data, Rain Gage Data, and Level Data. All data is filtered based on available water level data.
}
\seealso{
\code{\link{marsFetchRainfallData}}, \code{\link{marsFetchLevelData}}, \code{\link{marsFetchRainEventData}}
}
