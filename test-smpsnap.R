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


old_mars <- old_checksnap(conn_old, "1267-2-1", "CS2", "2024-03-27")

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
AND dtime BETWEEN '2024-03-02 05:00:00' AND '2024-03-02 15:00:00'"

new_rain <- dbGetQuery(conn_sand, new_rain_q)

pool::poolClose(conn_sand)

# Duration with new function
new_mars <- marsStormPeakIntensity_inhr(new_rain$rainfall_in)

# Same values
new_mars == old_mars