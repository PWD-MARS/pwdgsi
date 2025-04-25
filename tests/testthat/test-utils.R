test_that("handles different values correctly", {
  # Works with - and /
  expect_equal(check_date("2010-10-20", "America/New_York"), lubridate::ymd("2010-10-20", tz = "America/New_York"))
  expect_equal(check_date("2000/01/02", "America/New_York"), lubridate::ymd("2000-01-02", tz = "America/New_York"))
  # Handles non-character values
  expect_equal(check_date(lubridate::ymd("2000-10-20"), "America/New_York"), lubridate::ymd("2000-10-20", tz = "America/New_York"))
  # Will return NULL for wrong format 
  expect_null(check_date("20-01-02", "America/New_York"))
  # Return NULL if includes time
  expect_null(check_date("20-01-02 12:01:01", "America/New_York"))
})

test_that("returns the correct dates with time zones", {
  expect_equal(check_date(lubridate::ymd("2010-10-20",
                                         tz = "America/New_York"), "America/New_York"),
               lubridate::ymd("2010-10-20", tz = "America/New_York"))
  # It should always update with the correct time zone.
  expect_equal(check_date(lubridate::ymd("2010-10-20",
                                         tz = "Etc/GMT-4"), "Etc/GMT-4"),
               lubridate::ymd("2010-10-20", tz = "Etc/GMT-4"))
  # Without DST should be ETC/GMT-5
  expect_equal(check_date(lubridate::ymd("2010-10-20",
                                         tz = "America/New_York"), "America/New_York"),
               lubridate::ymd("2010-10-20", tz = "America/New_York"))
  
  
})