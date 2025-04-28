library(pool)


check_funs <- function(newest_func, newish_func, og_func) {
  if(identical(newest_func, newish_func)) {
    if (identical(newest_func, og_func)) {
      cat("Yay! Everything matches")
    } else{
      cat("new and og don't match :( ")
    }
    
  } else {
    cat("New and newish don't match")
  }
}

conn_sand <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "Jon_sandbox",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  tz = NULL)

edit_fetch <- edit_fetchRain(con = conn_sand, 
                             target_id = "1267-2-1",
                             source = "gage",
                             start_date = "2024-03-01",
                             end_date = "2024-04-01",
                             DST = FALSE)

fetch <- FetchRain(con = conn_sand, 
          target_id = "1267-2-1",
          source = "gage",
          start_date = "2024-03-01",
          end_date = "2024-03-31")

poolClose(conn_sand)

identical(fetch, edit_fetch)

mars_fetch <- marsFetchRainfallData(con = conn_sand, 
                                    target_id = "1267-2-1",
                                    source = "gage",
                                    start_date = "2024-03-01",
                                    end_date = "2024-03-31")




# 
# 
# 
# 
# check_funs(fetch, edit_fetch, mars_fetch)
# 
# identical(fetch, edit_fetch)
# identical(fetch, mars_fetch)
