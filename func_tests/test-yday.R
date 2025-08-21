library(pool)
library(pwdgsi)

# Current function
old_mars <- old_yday(lubridate::ymd_hms("2024-03-01 03:00:00"))

# New function
new_mars <- yday_decimal(lubridate::ymd_hms("2024-03-01 03:00:00", tz = "America/New_York"))

# Same!