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

old_rain_q <- "SELECT dtime_edt, rainfall_in FROM data.tbl_gage_rain WHERE gage_uid = 17 
AND dtime_edt BETWEEN '2024-03-01' AND '2024-03-31'"

old_rain <- dbGetQuery(conn_old, old_rain_q)

old_mars <- old_events(old_rain$dtime_edt, old_rain$rainfall_in)
# Spring forward

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

new_rain_q <- "SELECT dtime, rainfall_in FROM data.tbl_gage_rain WHERE gage_uid = 17 
AND dtime BETWEEN '2024-03-01' AND '2024-03-31'"

# Has one more rain value because old rain doesn't have 00:00:00 values in the db
new_rain <- dbGetQuery(conn_sand, new_rain_q)

pool::poolClose(conn_sand)

new_mars <- marsDetectEvents(new_rain$dtime, new_rain$rainfall_in)

old_mars_match <- append(old_mars, NA) 

# One is missing from the last storm, which had a 00:00:00 timestamp
diffs <- tibble::tibble(old = old_mars_match,
                      new = new_mars) |>
  tidyr::pivot_longer(cols = old:new,
                      names_to = "version",
                      values_to = "event") |>
  dplyr::group_by(version, event) |>
  dplyr::summarize(total = dplyr::n())
