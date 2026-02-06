library(pool)
library(tidyverse)

mars <- dbPool(drv = RPostgres::Postgres(),
                host = "PWDMARSDBS1",
                port = 5434,
                dbname = "liner",
                user= Sys.getenv("shiny_uid"),
                password = Sys.getenv("shiny_pwd"),
                timezone = NULL)

test <- baro(mars,
             target_id = "1359-8-2",
             start_date = "2025-11-01",
             end_date = "2025-11-03",
             data_interval = "15 mins")


