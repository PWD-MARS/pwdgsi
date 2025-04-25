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
    expect_error(edit_fetchRain(conn_sand,
                                target_id = "1267-2-1",
                                source = "gage",
                                # Invalid date
                                start_date = "2024-0-01",
                                end_date = "2024-03-31"))
    # If zero rows for that date range
    expect_error(edit_fetchRain(conn_sand,
                                target_id = "1267-2-1",
                                source = "gage",
                                # Invalid date
                                start_date = "2030-03-01",
                                end_date = "2030-03-31"))
    })
}