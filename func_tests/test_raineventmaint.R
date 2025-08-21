library(pool)

# Test production db data 
conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL) 

old_rain_newdata <- dbGetQuery(conn_old, "select gage_uid, dtime_edt, rainfall_in from data.tbl_gage_rain
                          WHERE dtime_edt BETWEEN '2024-03-01' AND '2024-03-31'")


old_rain_newevents <- old_rain_newdata %>%
  dplyr::group_by(gage_uid) %>%
  dplyr::arrange(dtime_edt) %>%
  dplyr::mutate(event_id = old_events(dtime_edt, rainfall_in)) %>%
  #Drop the last "complete" event in case it straddles the month boundary
  #It will get processed the when the next batch of data comes in
  dplyr::filter(!is.na(event_id), event_id != max(event_id, na.rm = TRUE)) %>%
  dplyr::group_by(gage_uid, event_id) %>%
  dplyr::summarize(eventdatastart = dplyr::first(dtime_edt),
            eventdataend = dplyr::last(dtime_edt),
            eventduration_hr = old_duration(dtime_edt),
            eventpeakintensity_inhr = old_peak(dtime_edt, rainfall_in),
            eventavgintensity_inhr = old_average(dtime_edt, rainfall_in),
            eventdepth_in = marsStormDepth_in(rainfall_in)) %>%
  dplyr::select(-event_id)

old_gage17 <- old_rain_newdata |> dplyr::filter(gage_uid == 17)
old_gage17_filter <- old_gage17 |> dplyr::filter(dplyr::between(dtime_edt, lubridate::ymd_hms("2024-03-27 15:15:00", tz = "America/New_York"),
                                          lubridate::ymd_hms("2024-03-28 04:30:00", tz = "America/New_York")))


conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

new_rain_newdata <- dbGetQuery(conn_sand, "select gage_uid, dtime, rainfall_in from data.tbl_gage_rain
                          WHERE dtime BETWEEN '2024-03-01' AND '2024-03-31'")


new_rain_newevents <- new_rain_newdata %>%
  dplyr::group_by(gage_uid) %>%
  dplyr::arrange(dtime) %>%
  dplyr::mutate(event_id = old_events(dtime, rainfall_in)) %>%
  #Drop the last "complete" event in case it straddles the month boundary
  #It will get processed the when the next batch of data comes in
  dplyr::filter(!is.na(event_id), event_id != max(event_id, na.rm = TRUE)) %>%
  dplyr::group_by(gage_uid, event_id) %>%
  dplyr::summarize(eventdatastart = dplyr::first(dtime),
                   eventdataend = dplyr::last(dtime),
                   eventduration_hr = old_duration(dtime),
                   eventpeakintensity_inhr = old_peak(dtime, rainfall_in),
                   eventavgintensity_inhr = old_average(dtime, rainfall_in),
                   eventdepth_in = marsStormDepth_in(rainfall_in)) %>%
  dplyr::select(-event_id)

new_gage17 <- new_rain_newdata |> dplyr::filter(gage_uid == 17)
new_gage17_filter <- new_gage17 |> dplyr::filter(dplyr::between(dtime, lubridate::ymd_hms("2024-03-27 15:15:00", tz = "America/New_York"),
                                                                lubridate::ymd_hms("2024-03-28 04:30:00", tz = "America/New_York")))
