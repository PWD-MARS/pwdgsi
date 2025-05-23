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
outflow <- c(1:27, 0)

old_mars <- old_peakRelease(rain$dtime_est, outflow)


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
new_outflow <- c(1:28, 0)

new_mars <- marsPeakReleaseRate_cfs(new_rain$dtime, new_outflow)
poolClose(conn_sand)
