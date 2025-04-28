library(pool)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "Jon_sandbox",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  tz = NULL)

edit_fetch <- fetchRain(con = conn_sand, 
                             target_id = "1267-2-1",
                             source = "gage",
                             start_date = "2024-03-01",
                             end_date = "2024-04-01")

mars_fetch <- marsFetchRainfallData(con = conn_sand, 
                                    target_id = "1267-2-1",
                                    source = "gage",
                                    start_date = "2024-03-01",
                                    end_date = "2024-03-31")
poolClose(conn_sand)


mars_fetch <- mars_fetch |> dplyr::rename(dtime = dtime_est)
mars_fetch_est <- mars_fetch |> dplyr::mutate(
  dtime = lubridate::force_tz(dtime, "America/New_York"),
  dtime = lubridate::with_tz(dtime, "EST"))

edit_fetch_est <- edit_fetch |> 
  dplyr::mutate(dtime = lubridate::with_tz(dtime, "EST"))

# The 105 not in EDT is because it has EST but it's really EDT.
not_in_edit <- dplyr::setdiff(mars_fetch_est, edit_fetch_est)
