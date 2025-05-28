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
rainfall <- c(0.2, 0.1, 0, 0.5, 0, rep(0, 23))

old_test <- tibble::tibble(dtime = rain$dtime_est,
                           waterlevel_ft = waterlevel_ft,
                           rainfall_in = rainfall)

old_mars <- old_infil(event = 39379,
                   dtime = old_test$dtime,
                   rainfall_in = old_test$rainfall_in,
                   dcia_ft2 = 135885, 
                   storage_depth_ft = 8.23,
                   storage_vol_ft3 = 22842,
                   waterlevel_ft = old_test$waterlevel_ft)

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
waterlevel_ft <- seq(.4, 3, by = 2.6/28) |> sort(decreasing = TRUE)
rainfall <- c(0.2, 0.1, 0, 0.5, 0, rep(0, 24))

poolClose(conn_sand)

new_test <- tibble::tibble(dtime = new_rain$dtime,
                           waterlevel_ft = waterlevel_ft,
                           rainfall = rainfall)

new_mars <- marsInfiltrationRate_inhr(event = 39379,
                                      dtime = new_test$dtime,
                                      rainfall_in = new_test$rainfall,
                                      dcia_ft2 = 135885, 
                                      storage_depth_ft = 8.23,
                                      storage_vol_ft3 = 22842,
                                      waterlevel_ft = new_test$waterlevel_ft)

# Data from sandbox_dtime

