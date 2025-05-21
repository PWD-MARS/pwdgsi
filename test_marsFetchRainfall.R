library(pool)

# Test jump forward in March
conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

# Current production db
old_mars <- old_rain(con = conn_old, 
                      target_id = "1267-2-1",
                      source = "gage",
                      start_date = "2024-03-01",
                      end_date = "2024-03-31")

pool::poolClose(conn_old)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

# Data from sandbox_dtime
new_mars <- marsFetchRainfallData(con = conn_sand, 
                                    target_id = "1267-2-1",
                                    source = "gage",
                                    start_date = "2024-03-01",
                                    end_date = "2024-03-31")
poolClose(conn_sand)

# Manually change old data to be the equivalent of America/New_York (different check from with_tz)
old_mars <- old_mars |> dplyr::select(dtime_est, gage_uid, rainfall_in) |> 
                 dplyr::rename(dtime = dtime_est) |>
                 dplyr::mutate(dtime = dplyr::if_else(dtime >= lubridate::ymd_hms("2024-03-10 02:00:00", tz = "EST"), 
                                               dtime - lubridate::hours(1), dtime))

# Differences are only midnights which don't exist in production
diffs <- dplyr::symdiff(new_mars |> dplyr::select(gage_uid, dtime, rainfall_in),
                        old_mars)

# Check fall back in November
conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)


old_mars <- old_rain(con = conn_sand,
                     target_id = "1267-2-1",
                     source = "gage",
                     start_date = "2024-10-01",
                     end_date = "2024-11-30")
poolClose(conn_sand)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)


new_mars <- marsFetchRainfallData(con = conn_sand,
                                  target_id = "1267-2-1",
                                  source = "gage",
                                  start_date = "2024-10-01",
                                  end_date = "2024-11-30")

poolClose(conn_sand)

old_mars <- old_mars |> dplyr::select(dtime_est, gage_uid, rainfall_in) |> 
  dplyr::rename(dtime = dtime_est) |>
  dplyr::mutate(dtime = dplyr::if_else(dtime <= lubridate::ymd_hms("2024-11-03 02:00:00", tz = "EST"), 
                                       dtime - lubridate::hours(1), dtime))

diffs <- dplyr::symdiff(new_mars |> dplyr::select(gage_uid, dtime, rainfall_in),
                        old_mars)
