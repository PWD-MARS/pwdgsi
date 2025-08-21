library(pool)

conn_old <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

old_mars <- old_eventPlot(con = conn_old,
                          event_date = "2024-03-27",
                          source = "gage",
                          smp_id = "1267-2-1",
                          ow_suffix = "CS1")

poolClose(conn_old)

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "sandbox_dtime",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

new_mars <- marsEventCombinedPlot(con = conn_sand,
                             event_date = "2024-03-27",
                             source = "gage",
                             smp_id = "1267-2-1",
                             ow_suffix = "CS1")

poolClose(conn_sand)