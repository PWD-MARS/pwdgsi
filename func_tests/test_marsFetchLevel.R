library(pool)

# Test jump forward in March
conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = "EST")

# Current production db
old_mars_level <- old_level(con = conn_old, 
                      target_id = "1267-2-1",
                      ow_suffix = "CS2",
                      start_date = "2024-03-01",
                      end_date = "2024-03-31",
                      sump_correct = TRUE)

pool::poolClose(conn_old)


conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = "America/New_York")

# Data from sandbox_dtime
new_mars1 <- marsFetchLevelData(con = conn_sand, 
                                  target_id = "1267-2-1",
                                  ow_suffix = "CS1",
                                  start_date = "2024-03-01",
                                  end_date = "2024-03-31",
                                  sump_correct = TRUE)
poolClose(conn_sand)

# Rename and move level data to America/New_York
old_mars1 <- old_mars |>
  dplyr::rename(dtime = dtime_est) |>
  dplyr::mutate(dtime = lubridate::with_tz(dtime, tzone = "America/New_York"))

# Diffs show only 4/1 days that are likely due to requesting in EST and then coverting to America/New_York
diffs <- dplyr::symdiff(new_mars1 |> dplyr::select(dtime, level_ft, smp_id), old_mars1 |> dplyr::select(dtime, level_ft, smp_id))

# Test fall back in November
conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = "EST")

# Current production db
old_mars <- old_level(con = conn_old, 
                      target_id = "14-1-1",
                      ow_suffix = "SW1",
                      start_date = "2024-11-01",
                      end_date = "2024-11-30",
                      sump_correct = TRUE)

pool::poolClose(conn_old)


conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = "America/New_York")

# Data from sandbox_dtime
new_mars <- marsFetchLevelData(con = conn_sand, 
                                target_id = "14-1-1",
                                ow_suffix = "SW1",
                                start_date = "2024-11-01",
                                end_date = "2024-11-30",
                                sump_correct = TRUE)
poolClose(conn_sand)

# Rename and move level data to America/New_York
old_mars1 <- old_mars |>
  dplyr::rename(dtime = dtime_est) |>
  dplyr::mutate(dtime = lubridate::with_tz(dtime, tzone = "America/New_York"))

# Diffs show only 4/1 days that are likely due to requesting in EST and then coverting to America/New_York
diffs <- dplyr::symdiff(new_mars |> dplyr::select(dtime, level_ft, smp_id), old_mars1 |> dplyr::select(dtime, level_ft, smp_id))
