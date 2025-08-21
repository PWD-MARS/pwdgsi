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

old_mars <- old_rainPlot(rain$dtime_est, rain$rainfall_in, rain$gage_event_uid[[1]])

poolClose(conn_old)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

rain_new <- marsFetchRainfallData(conn_sand, "1267-2-1", "gage", "2024-03-27 15:15:00", "2024-03-28 04:30:00")

new_mars <- marsRainfallPlot(rain_new$dtime, rain_new$rainfall_in, 38379)

poolClose(conn_sand)