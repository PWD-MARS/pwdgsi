if(interactive()) {
  # fetchRain
  test_that("Returns errors", {
    # Connect to db
    conn_sand <- pool::dbPool(
      drv = RPostgres::Postgres(),
      host = "PWDMARSDBS1",
      port = 5434,
      dbname = "Jon_sandbox",
      user= Sys.getenv("shiny_uid"),
      password = Sys.getenv("shiny_pwd"))
    
    # If date in not the right format
    expect_error(fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = "2024-0-01",
                           end_date = "2024-03-31"))
    # If zero rows for that date range
    expect_error(fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = "2030-03-01",
                           end_date = "2030-03-31"))
    # This query should return 219 values
    expect_equal(nrow(fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = "2024-03-01",
                           end_date = "2024-03-31")), 219)
    # Throws an error that the date needs to use the yyyy-mm-dd format
    expect_error(fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = "2024-03-01 02:02:03",
                           end_date = "2024-03-03"), "Please use yyyy-mm-dd for date formats")
    # Check tz is EDT
    test_tz <- fetchRain(conn_sand,
                         smp_id = "1267-2-1",
                         source = "gage",
                         # Invalid date
                         start_date = "2024-03-01",
                         end_date = "2024-03-03")
    expect_equal(attributes(test_tz$dtime)$tzone, "America/New_York")
    # Returns correct data if date is a date value
    expect_equal(fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = "2024-03-01",
                           end_date = "2024-03-31"),
                 fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = lubridate::ymd("2024-03-01"),
                           end_date = "2024-03-31"))
    
    
    # Test that source works correctly
      gage <- fetchRain(conn_sand,
                           smp_id = "1267-2-1",
                           source = "gage",
                           # Invalid date
                           start_date = "2024-03-01",
                           end_date = "2024-03-31")
      expect_equal(names(gage), c("dtime", 
                                  "rainfall_in", 
                                  "gage_uid", 
                                  "gage_event_uid"))
      radar <- fetchRain(conn_sand,
                        smp_id = "1267-2-1",
                        source = "radar",
                        # Invalid date
                        start_date = "2024-03-01",
                        end_date = "2024-03-31")
      expect_equal(names(radar), c("dtime", 
                                  "rainfall_in", 
                                  "radar_uid", 
                                  "radar_event_uid"))
    poolClose(conn_sand)
  })
}