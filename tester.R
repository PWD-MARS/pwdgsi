library(pwdgsi)
library(pool)
library(tidyverse)

# Connect to sandbox (Should be EST)
conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "Jon_sandbox",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"))

# Should return EST values
test_sand <- marsFetchRainfallData(con = conn_sand, 
                                   target_id = "1267-2-1",
                                   source = "gage",
                                   start_date = "2024-03-01",
                                   end_date = "2024-03-31") |>
  # Get rid of NAs and event_uids because I don't know if event UIDs should be the same
  filter(!is.na(dtime_est)) |>
  select(-gage_event_uid)

poolClose(conn_sand)

# Connect with current production server which I believe should have the same EST values if we don't change to datetime_local
conn_prod<- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"))

# Pull data that I believe should be EST
test_prod <- marsFetchRainfallData(con = conn_prod, 
                                   target_id = "1267-2-1",
                                   source = "gage",
                                   start_date = "2024-03-01",
                                   end_date = "2024-03-31") |>
  filter(!is.na(dtime_est)) |>
  select(-gage_event_uid)
poolClose(conn_prod)

# Combine the production to sand and check if the rainfall is the same.
# It is not. It shows a difference starting in events post-DST
diffs <- test_sand |>
  left_join(test_prod, by = join_by(dtime_est)) |>
  mutate(same = rainfall_in.x == rainfall_in.y)


# Change sandbox data to EDT to see if it matches production
sand_edt <- test_sand |> mutate(dtime_est = force_tz(dtime_est, tz = "America/New_York"))

# It matches :(
diffs2 <- sand_edt|>
  left_join(test_prod, by = join_by(dtime_est)) |>
  mutate(same = rainfall_in.x == rainfall_in.y)
