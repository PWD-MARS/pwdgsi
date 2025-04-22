library(pwdgsi)
library(pool)
library(tidyverse)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "Jon_sandbox",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"))

test_sand <- marsFetchRainfallData(con = conn_sand, 
                                   target_id = "1267-2-1",
                                   source = "gage",
                                   start_date = "2024-03-01",
                                   end_date = "2024-03-31") |>
  filter(!is.na(dtime_est)) |>
  select(-gage_event_uid)

poolClose(conn_sand)

conn_prod<- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"))

test_prod <- marsFetchRainfallData(con = conn_prod, 
                                   target_id = "1267-2-1",
                                   source = "gage",
                                   start_date = "2024-03-01",
                                   end_date = "2024-03-31") |>
  filter(!is.na(dtime_est)) |>
  select(-gage_event_uid)
poolClose(conn_prod)

diffs <- test_sand |>
  left_join(test_prod, by = join_by(dtime_est)) |>
  mutate(same = rainfall_in.x == rainfall_in.y)


sand_edt <- test_sand |> with_tz(dtime_est, tz = "America/New_York")

diffs2 <- sand_edt|>
  left_join(test_prod, by = join_by(dtime_est)) |>
  mutate(same = rainfall_in.x == rainfall_in.y)
