library(pool)

# Test production db data 
conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = "EST")


rain <- old_rain(conn_old, "1267-2-1", "gage", "2024-03-27 15:15:00", "2024-03-28 04:30:00") |> dplyr::select(dtime_est)
waterlevel_ft <- seq(.4, 3, by = 2.6/27) |> sort(decreasing = TRUE)


old_mars <- old_udout(rain$dtime_est,
                      waterlevel_ft,
                      1,
                      8)

pool::poolClose(conn_old)

# New function
conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

new_rain <- marsFetchRainfallData(conn_sand, "1267-2-1", "gage", "2024-03-27 15:15:00", "2024-03-28 04:30:00")


poolClose(conn_sand)

new_mars <- marsSimulatedLevelSeries_ft(dtime = new_rain$dtime,
                                     rainfall_in = new_rain$rainfall_in,
                                     event = new_rain$gage_event_uid[1],
                                     infil_footprint = 17639,
                                     dcia_ft2 = 7.7,
                                     orifice_height_ft = 1,
                                     orifice_diam_in = 1.875,
                                     storage_depth = 8.3,
                                     storage_vol_ft3 = 22842
                                     )
