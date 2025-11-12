# caclulating instantaneous recession rates>
# recession rate function caclulates rates and assign it to rain timeseries through fkeys
# metadata function attaches sump depth, orifice depth, and rain meta data to the output of the recession rate function
library(readxl)
library(tidyverse)
library(RPostgres)
library(pool)
library(fpp2) # working with time series data
library(zoo) # working with time series data
library(patchwork)
library(data.table)
library(fuzzyjoin)
library(data.table)


# Connect
conn <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user = Sys.getenv("admin_uid"),
  password = Sys.getenv("admin_pwd"),
  timezone = NULL
)

# recession function calculates instantaneous slopes (both positive and negative) and joins with rain data at 15-min intervals
# 4 input arguments: DB conn, Level TS, Dtime Index, and OW UID

recession_rate <- function(ow_uid, dtime, level_ft) {
  # If the level_ft and dtime do not match return NAs
  if (length(dtime) != length(level_ft)) {
    print(paste("Level and dtime timeseries have different lengths!"))
    return(NA)
  }
  
  # both ow_uid and level must be numeric and ow_uid must be numeric
  if (!is.numeric(ow_uid) & (length(ow_uid) == 1 | length(ow_uid) == length(level_ft))) {
    print(paste("OW UID must be a numeric value!"))
    return(NA)
  }
  
  if (!is.numeric(level_ft)) {
    print(paste("Level data must be numeric!"))
    return(NA)
  }

  # join dtime and level_ft
  level_ft_df <- data.frame(ow_uid = ow_uid, dtime = as.POSIXct(dtime), level_ft = level_ft)

  # moving ave smoothing
  level_ft_df$level_ft_smoothed <- rollmean(level_ft_df$level_ft, k = 5, fill = NA)

  output_df <- level_ft_df %>%
    mutate(
      difftime_hr = as.numeric(dtime - lag(dtime)) / 60, # 15 minute steps
      difflevel_ft = level_ft_smoothed - lag(level_ft_smoothed),
      recession_rate_inhr = (difflevel_ft / difftime_hr) * 12
    ) %>%
    select(ow_uid, dtime, level_ft, recession_rate_inhr)
    
}

# meta data function to make TS regular at 15 min interval, add rainfall TS and event meta data, post rain meta data, orifice and sump values
recession_rate_meta <- function(conn, ow_uid, dtime, level_ft) {
  # If the level_ft and dtime do not match return NAs
  if (length(dtime) != length(level_ft)) {
    print(paste("Level and dtime timeseries have different lengths!"))
    return(NA)
  }

  # both ow_uid and level must be numeric and ow_uid must be numeric
  if (!is.numeric(ow_uid) & (length(ow_uid) == 1 | length(ow_uid) == length(level_ft))) {
    print(paste("OW UID must be a numeric value!"))
    return(NA)
  }

  if (!is.numeric(level_ft)) {
    print(paste("Level data must be numeric!"))
    return(NA)
  }

  # calculate recession rates
  level_recession_df <- recession_rate(ow_uid, dtime, level_ft)

  # filtering rain data
  boundaries <- group_by(level_recession_df, ow_uid) %>%
    summarize(
      start = min(dtime),
      end = max(dtime)
    )

  # Pull rain ts
  gage <- dbGetQuery(conn, paste0(
    "select * from admin.tbl_smp_gage left join fieldwork.tbl_ow using(smp_id) where ow_uid in (",
    paste0(unique(ow_uid), collapse = ", '"), ")"
  ))

  events <- dbGetQuery(conn, paste0("select * from data.tbl_gage_event 
    where gage_uid in (", paste(gage$gage_uid, collapse = ", "), ")"))

  rain_ts <- dbGetQuery(conn, paste0("select * from data.viw_gage_rainfall 
    where gage_uid in (", paste(gage$gage_uid, collapse = ", "), ")")) %>%
    dplyr::filter(dtime >= boundaries$start & dtime <= boundaries$end)

  # add post rain meta data
  # loop to make events regular with zero rainfalls
  events_unique <- rain_ts %>%
    select(gage_event_uid) %>%
    distinct() %>%
    na.omit() %>%
    pull()

  regular_rain_ts <- NULL
  for (i in 1:length(events_unique)) {
    rain_ts_event <- rain_ts %>%
      filter(gage_event_uid == events_unique[i])

    # Ensure proper date-time format
    rain_ts_event$dtime <- as.POSIXct(rain_ts_event$dtime)

    # Create zoo object
    ts_zoo_object <- zoo(
      rain_ts_event[, c("gage_uid", "rainfall_in", "gage_event_uid")],
      order.by = rain_ts_event$dtime
    )

    all_times <- seq(from = start(ts_zoo_object), to = end(ts_zoo_object), by = "15 min")

    z_reg <- merge(ts_zoo_object, zoo(, all_times), all = TRUE)

    # Carry forward gage_uid and gage_event_uid
    z_reg$gage_uid <- na.locf(z_reg$gage_uid)
    z_reg$gage_event_uid <- na.locf(z_reg$gage_event_uid)

    # Replace NAs in rainfall with 0 (zero rainfall for missing intervals)
    z_reg$rainfall_in[is.na(z_reg$rainfall_in)] <- 0

    regular_rain_ts <- rbind(regular_rain_ts, z_reg)
  }

  regular_rain_ts_df <- data.frame(dtime = as.POSIXct(index(regular_rain_ts)), data.frame(regular_rain_ts, row.names = NULL))

  joined_df <- level_recession_df %>%
    left_join(regular_rain_ts_df, by = c("dtime"))

  # Tag post-event data
  events_start_stop <- events %>%
    select(gage_event_uid, eventdatastart, eventdataend) %>%
    filter(gage_event_uid %in% joined_df$gage_event_uid)

  rates_between_events <- NULL
  rates_during_events <- joined_df %>%
    filter(!is.na(gage_event_uid)) %>%
    mutate(post_gage_event_uid = NA)
  # assign gage_event_uid to the post event as post_gage_event_uid attribute
  for (j in 1:(nrow(events_start_stop) - 1)) {
    rates_temp <- joined_df %>%
      filter(dtime > events_start_stop$eventdataend[j] & dtime < events_start_stop$eventdatastart[j + 1]) %>%
      mutate(post_gage_event_uid = events_start_stop$gage_event_uid[j])

    rates_between_events <- rbind(rates_between_events, rates_temp)
  }

  complete_rates <- rbind(rates_between_events, rates_during_events) %>%
    arrange(dtime) %>%
    select(dtime, post_gage_event_uid)

  complete_rates <- joined_df %>%
    left_join(complete_rates, by = "dtime")

  # Pull well measurements
  well_meas <- dbGetQuery(conn, paste0(
    "select * from fieldwork.tbl_well_measurements where ow_uid in (",
    paste0(unique(ow_uid), collapse = ", '"), ")"
  )) %>%
    select(custom_sumpdepth_ft, custom_orificedepth_ft, start_dtime, end_dtime)
  
  # Fill missing end_dtime with today and sort
  well_meas <- well_meas %>%
    mutate(end_dtime = replace_na(end_dtime, Sys.Date())) %>%
    arrange(end_dtime)
  
  # If well_meas has only one row, use its values for all rows
  if (nrow(well_meas) == 1) {
    complete_rates_sump_orifice <- complete_rates %>%
      arrange(dtime) %>%
      mutate(
        sump_depth_ft = well_meas$custom_sumpdepth_ft,
        orifice_tostone_ft = well_meas$custom_orificedepth_ft
      )
  } else {
    # Otherwise, assign based on which interval each dtime falls into
    complete_rates_sump_orifice <- complete_rates %>%
      arrange(dtime) %>%
      mutate(
        idx = findInterval(dtime, well_meas$end_dtime, left.open = TRUE) + 1,
        idx = pmin(idx, nrow(well_meas)),  # cap at last row
        sump_depth_ft = well_meas$custom_sumpdepth_ft[idx],
        orifice_tostone_ft = well_meas$custom_orificedepth_ft[idx]
      ) %>%
      select(-idx)
  }
  
  # Create 15-min interval grid
  time_grid <- data.frame(dtime = seq(
    floor_date(min(level_recession_df$dtime, na.rm = TRUE), "15 mins"),
    ceiling_date(max(level_recession_df$dtime, na.rm = TRUE), "15 mins"),
    by = "15 mins"
  ))

  # Join with the time grid to enforce 15-min intervals ---
  result <- time_grid %>%
    left_join(complete_rates_sump_orifice, by = "dtime") %>%
    filter(!is.na(recession_rate_inhr))

  return(result)
}




# calculate recession rates for long term sites 
# Original longterm sites and A/B testing
targeted_sties <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\06 Continued Monitoring Plan\\Continued Monitoring Plan System List.xlsx") %>%
  filter(`Test Group` == "Original-Long-Term Sedimentation Monitoring" | `Test Group` == "A/B Short-Term Remonitoring") %>%
  select(smp_id = `SMP ID`, test_group = `Test Group`, ow_suffix = Location)

ow_uid_list <- dbGetQuery(conn, paste("select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow where smp_id in (", toString(paste("'", targeted_sties$smp_id, "'", sep = "")), ")", sep = "")) 

longterm_targeted_sties <- targeted_sties %>%
  inner_join(ow_uid_list, by = c("smp_id", "ow_suffix")) %>%
  filter(test_group == "Original-Long-Term Sedimentation Monitoring")
  
# create a loop to store recession rates 
longterm_recession_rates <- NULL

for (i in 1:nrow(longterm_targeted_sties)) {
  
  owdata_temp <- dbGetQuery(conn, paste0("select dtime, level_ft from data.tbl_ow_leveldata_raw
    where ow_uid in (", paste(longterm_targeted_sties$ow_uid[i], collapse = ", "), ")"))
  
  longterm_recession_rates_temp <- recession_rate_meta(conn = conn,
                                                       ow_uid = longterm_targeted_sties$ow_uid[i],
                                                       dtime = owdata_temp$dtime,
                                                       level_ft = owdata_temp$level_ft)
  
  longterm_recession_rates <- rbind(longterm_recession_rates, longterm_recession_rates_temp)
  
}


ow_uid_plot <- 708
# post events
postevent_rates_trend_analysis <- longterm_recession_rates %>%
  filter(ow_uid == ow_uid_plot &
           level_ft > 1.41 &
           level_ft < 2.41 &
           recession_rate_inhr < 0 & 
           !is.na(post_gage_event_uid))

# get median of the recession rates
postevent_rates_trend_analysis_grouped <- postevent_rates_trend_analysis %>%
  group_by(post_gage_event_uid) %>%
  summarise(median_rate = median(recession_rate_inhr), rain_date = as.Date(min(dtime, na.rm = T)))

# plot
post_trend_plot <- ggplot(postevent_rates_trend_analysis_grouped, aes(x = rain_date, y = median_rate)) + 
  geom_point() +
  ylim(0, -5) +
  ggtitle("187-3-3 Tree trench, Post Rain Median Recession Data (1 ft above the bottom of the sump)") +
  scale_x_date(
    date_breaks = "1 year",     # show a tick every year
    date_labels = "%Y"          # format labels as 4-digit years
  )

#during events
duringevent_rates_trend_analysis <- longterm_recession_rates %>%
  filter(ow_uid == ow_uid_plot &
           level_ft > 1.41 &
           level_ft < 2.41 &
         recession_rate_inhr < 0 &
           is.na(post_gage_event_uid))

# get median of the recession rates
duringevent_rates_trend_analysis_grouped <- duringevent_rates_trend_analysis %>%
  group_by(gage_event_uid) %>%
  summarise(median_rate = median(recession_rate_inhr), rain_date = as.Date(min(dtime, na.rm = T)))

# plot
during_trend_plot <- ggplot(duringevent_rates_trend_analysis_grouped, aes(x = rain_date, y = median_rate)) + 
  geom_point() +
  ylim(0, -5) +
  ggtitle("During Rain Median Recession Data") +
  scale_x_date(
    date_breaks = "1 year",     # show a tick every year
    date_labels = "%Y"          # format labels as 4-digit years
  )


# during and post event
all_rates_trend_analysis_grouped <- longterm_recession_rates %>%
  filter(ow_uid == ow_uid_plot & recession_rate_inhr < 0 & level_ft > 1.41 & level_ft < 2.41) %>%
  mutate(all_event = ifelse(is.na(gage_event_uid), post_gage_event_uid, gage_event_uid)) %>%
  group_by(all_event) %>%
  summarise(median_rate = median(recession_rate_inhr), rain_date = as.Date(min(dtime, na.rm = T)))

# plot
all_trend_plot <- ggplot(all_rates_trend_analysis_grouped, aes(x = rain_date, y = median_rate)) + 
  geom_point() +
  ylim(0, -5) +
  ggtitle("During and Post Rain Median Recession Data") +
  scale_x_date(
    date_breaks = "1 year",     # show a tick every year
    date_labels = "%Y"          # format labels as 4-digit years
  )

combined_trend <- post_trend_plot/during_trend_plot/all_trend_plot
combined_trend
