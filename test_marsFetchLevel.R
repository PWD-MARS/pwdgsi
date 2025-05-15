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
old_mars <- old_level(con = conn_old, 
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