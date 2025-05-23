library(pool)

# Test production db data 
dtime_old <- seq(lubridate::ymd_hms("2024-03-27 00:00:00", tz = "EST"),
             lubridate::ymd_hms("2024-03-30 00:00:00", tz = "EST"),
             "15 min")
rainfall_in_old <- c(0, 0, 0, 0.01, 0.02, 0.02, 0.03, 0.03, 0.03, 0.04, 0.04, 0.03, 0.02, 0.01, 0.01, rep(0, 274))
waterlevel_ft_old <- c(1, 1, 1, 1.02, 1.05, 1.08, 1.12, 1.3, 1.34, 1.5, 1.7, 1.9, 2, 2.25, seq(2.25, .05, (-2.25/281)))

old_mars <- old_drainDown(dtime_old, rainfall_in_old, waterlevel_ft_old)

# New function
dtime <- seq(lubridate::ymd_hms("2024-03-27 00:00:00", tz = "America/New_York"),
             lubridate::ymd_hms("2024-03-30 00:00:00", tz = "America/New_York"),
             "15 min")
rainfall_in <- c(0, 0, 0, 0.01, 0.02, 0.02, 0.03, 0.03, 0.03, 0.04, 0.04, 0.03, 0.02, 0.01, 0.01, rep(0, 274))
waterlevel_ft <- c(1, 1, 1, 1.02, 1.05, 1.08, 1.12, 1.3, 1.34, 1.5, 1.7, 1.9, 2, 2.25, seq(2.25, .05, (-2.25/281)))

new_mars <- marsDraindown_hr(dtime, rainfall_in, waterlevel_ft)
# Matches