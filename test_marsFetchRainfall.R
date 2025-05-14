library(pool)

# Jump forward in March
conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)


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


new_mars <- marsFetchRainfallData(con = conn_sand, 
                                    target_id = "1267-2-1",
                                    source = "gage",
                                    start_date = "2024-03-01",
                                    end_date = "2024-03-31")
poolClose(conn_sand)

# Check to make sure values >DST are different by an hour
old_mars <- old_mars |> dplyr::select(dtime_est, gage_uid, rainfall_in) |> 
                 dplyr::rename(dtime = dtime_est) |>
                 dplyr::mutate(dtime = dplyr::if_else(dtime >= lubridate::ymd_hms("2024-03-10 02:00:00", tz = "EST"), 
                                               dtime - lubridate::hours(1), dtime))


diffs <- dplyr::setdiff(new_mars |> dplyr::select(gage_uid, dtime, rainfall_in),
                        old_mars)
# # Fall back in November
# conn_sand <- dbPool(
#   drv = RPostgres::Postgres(),
#   host = "PWDMARSDBS1",
#   port = 5434,
#   dbname = "mars_data",
#   user= Sys.getenv("shiny_uid"),
#   password = Sys.getenv("shiny_pwd"),
#   timezone = NULL)
# 
# 
# old_mars <- old_rain(con = conn_sand, 
#                      target_id = "1267-2-1",
#                      source = "gage",
#                      start_date = "2024-11-01",
#                      end_date = "2024-11-30")
# poolClose(conn_sand)
# 
# conn_sand <- dbPool(
#   drv = RPostgres::Postgres(),
#   host = "PWDMARSDBS1",
#   port = 5434,
#   dbname = "sandbox_dtime",
#   user= Sys.getenv("shiny_uid"),
#   password = Sys.getenv("shiny_pwd"),
#   timezone = NULL)
# 
# 
# new_mars <- marsFetchRainfallData(con = conn_sand, 
#                                   target_id = "1267-2-1",
#                                   source = "gage",
#                                   start_date = "2024-11-01",
#                                   end_date = "2024-11-30")
# 
# poolClose(conn_sand)
# 
# dplyr::symdiff(new_mars |> dplyr::select(-gage_uid, -gage_event_uid), old_mars |> dplyr::select(-gage_uid, -gage_event_uid))
