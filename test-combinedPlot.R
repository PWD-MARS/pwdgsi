library(pool)

conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

rain <- old_rain(conn_old, "1267-2-1", "gage", "2024-03-27 15:15:00", "2024-03-28 04:30:00")
waterlevel_ft <- seq(.4, 3, by = 2.6/27) |> sort(decreasing = TRUE)

old_mars <- old_combinedPlot(event = rain$gage_event_uid[[1]], 
                             structure_name = "1267-2-1",
                             sim_datetime = NA,
                             sim_level_ft = NA,
                             obs_datetime = rain$dtime_est, 
                             obs_level_ft = waterlevel_ft,
                             storage_depth_ft = 6, 
                             orifice_show = TRUE,
                             orifice_height_ft = 1,
                             rainfall_datetime = rain$dtime_est,
                             rainfall_in = rain$rainfall_in,
                             metrics_show = FALSE)

poolClose(conn_old)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

rain_new <- marsFetchRainfallData(conn_sand, "1267-2-1", "gage", "2024-03-27 15:15:00", "2024-03-28 04:30:00") |>
  # Filter out the 00:00:00 value missing in old rain data for matching
  dplyr::filter(dtime != lubridate::ymd_hms("2024-03-28 00:00:00", tz = "America/New_York"))

new_mars <- marsCombinedPlot(event = rain_new$gage_event_uid[[1]], 
                               structure_name = "1267-2-1",
                               obs_datetime = rain_new$dtime, 
                               obs_level_ft = waterlevel_ft,
                               storage_depth_ft = 6, 
                               orifice_show = TRUE,
                               orifice_height_ft = 1,
                               rainfall_datetime = rain_new$dtime,
                               rainfall_in = rain_new$rainfall_in,
                               metrics_show = FALSE)

poolClose(conn_sand)