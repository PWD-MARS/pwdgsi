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

# Spring forward
old_mars <- old_baro(con = conn_old, 
                     target_id = "1267-2-1",
                     start_date = "2024-03-01",
                     end_date = "2024-03-31",
                     data_interval = "15 mins")

pool::poolClose(conn_old)

# sandbox_dtime data
conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

# Data for spring fowards
new_mars <- marsFetchBaroData(con = conn_sand, 
                                  target_id = "1267-2-1",
                                  start_date = "2024-03-01",
                                  end_date = "2024-03-31",
                                  data_interval = "15 mins")
poolClose(conn_sand)

# Change old data to EDT
old_mars2 <- old_mars |>  
  dplyr::rename(dtime = dtime_est) |>
   dplyr::mutate(dtime = lubridate::with_tz(dtime, tz = "America/New_York"))

# Old function adds an extra day (slightly weird with timezones I think)
diffs <- dplyr::symdiff(new_mars, old_mars2)

# No diffs for change
old_10 <- old_mars2 |> dplyr::filter(dplyr::between(dtime, lubridate::ymd("2024-03-10"), lubridate::ymd("2024-03-11")), smp_id == '9-1-1')
new_10 <- new_mars |> dplyr::filter(dplyr::between(dtime, lubridate::ymd("2024-03-10"), lubridate::ymd("2024-03-11")), smp_id == '9-1-1')

diffs10 <- dplyr::setdiff(old_10, new_10)


conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = "EST")


old_mars <- old_baro(con = conn_old, 
                     target_id = "1267-2-1",
                     start_date = "2024-10-01",
                     end_date = "2024-11-30",
                     data_interval = "15 mins")

pool::poolClose(conn_old)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)


new_mars <- marsFetchBaroData(con = conn_sand, 
                              target_id = "1267-2-1",
                              start_date = "2024-10-01",
                              end_date = "2024-11-30",
                              data_interval = "15 mins")
poolClose(conn_sand)

old_mars2 <- old_mars |>  
  dplyr::rename(dtime = dtime_est) |>
  dplyr::mutate(dtime = lubridate::with_tz(dtime, tz = "America/New_York"))

diffs <- dplyr::symdiff(new_mars, old_mars2)


old_10 <- old_mars2 |> dplyr::filter(dplyr::between(dtime, lubridate::ymd("2024-10-01"), lubridate::ymd("2024-11-15")))
new_10 <- new_mars |> dplyr::filter(dplyr::between(dtime, lubridate::ymd("2024-10-01"), lubridate::ymd("2024-11-15")))

# Same as above
diffs10 <- dplyr::symdiff(old_10, new_10)

