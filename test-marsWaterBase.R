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

event <- marsFetchRainEventData(conn_old,
                                "1267-2-1",
                                "gage",
                                "2024-03-27 15:15:00",
                                "2024-03-28 04:30:00")
