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

<<<<<<< HEAD
event <- marsFetchRainEventData(conn_old,
                                "1267-2-1",
                                "gage",
                                "2024-03-27 15:15:00",
                                "2024-03-28 04:30:00")
=======
# Pull level and dtime data
old_lvl_q <- "SELECT dtime_est, level_ft, smp_id, ow_suffix FROM data.viw_ow_leveldata_sumpcorrected WHERE smp_id = '1267-2-1' AND ow_suffix = 'CS2' 
AND dtime_est BETWEEN '2024-03-01 00:00:00' AND '2024-03-31 23:59:59'"

old_lvl <- dbGetQuery(conn_old, old_lvl_q)

pool::poolClose(conn_old)

# Check that level datetimes converted to America/New_York are the same as new_mars
old_lvl_adj <- old_lvl |>
  dplyr::rename(dtime = dtime_est) |>
  dplyr::mutate(dtime = lubridate::with_tz(dtime, tzone = "America/New_York"))

# Change dtime_est to America/New_York to match but keep column name for compatability with old function
old_lvl_matched <-
  old_lvl |> 
  dplyr::mutate(dtime_est = lubridate::with_tz(dtime_est, tzone = "America/New_York"))

# Old baseline function results
old_mars <- old_baseline(old_lvl_matched$dtime_est, old_lvl_matched$level_ft)


# sandbox_dtime data
conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

new_lvl_q <- "SELECT dtime, level_ft, smp_id, ow_suffix FROM data.viw_ow_leveldata_sumpcorrected WHERE smp_id = '1267-2-1' AND ow_suffix = 'CS2' 
AND dtime BETWEEN '2024-03-01 00:00:00' AND '2024-03-31 23:59:59'"

new_lvl <- dbGetQuery(conn_sand, new_lvl_q)

pool::poolClose(conn_sand)

# New baseline function results
new_mars <- marsWaterLevelBaseline_ft(new_lvl$dtime, new_lvl$level_ft)


# Diffs between level data is pulling data using EST and then converting to America/New_York
diffs_lvl <- dplyr::symdiff(new_lvl, old_lvl_adj)

# Diff between baseline function output
diff <- dplyr::symdiff(new_mars, old_mars)
# Matches but shows difference due to quering using EST and converting to America/New_York
>>>>>>> 168d59a5f61912f48bc2438539497ee6ea15bb48
