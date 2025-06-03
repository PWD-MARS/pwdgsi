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

old_mars <- old_smpSnap(conn_old, "1267-2-1", "CS1", "2024-03-27")

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

new_mars <- marsFetchSMPSnapshot(conn_sand, "1267-2-1", "CS1", "2024-03-27")

pool::poolClose(conn_sand)

