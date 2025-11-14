# caclulating instantaneous recession rates across water
library(readxl)
library(tidyverse)
library(RPostgres)
library(pool)
library(fpp2)           # working with time series data
library(zoo)            # working with time series data
library(patchwork)
library(data.table)


#Connect
marsDBCon <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user= Sys.getenv("admin_uid"),
  password = Sys.getenv("admin_pwd"),
  timezone = NULL)


#Slope function
FarshadSlope <- function(level_dtime, series, rain_dtime, rain_depth_in, gage_event_uid, sump_depth_ft, orrifice_elev_ft){
  #Goals: 
  #Find the instantaneous slope at each timestep there
  
  rain_df <- data.frame(rain_dtime = as.POSIXct(rain_dtime), rain_depth_in, gage_event_uid)
  joined_df  <- data.frame(level_dtime = as.POSIXct(level_dtime), series) %>%
    full_join(rain_df, by = c("level_dtime" = "rain_dtime"))
  
  # Create 15-min interval grid
  time_grid <- data.frame(datetime = seq(
    floor_date(min(level_dtime, na.rm = TRUE), "15 mins"),
    ceiling_date(max(level_dtime, na.rm = TRUE), "15 mins"),
    by = "15 mins"
  ))
  
  # Join with the time grid to enforce 15-min intervals ---
  result <- time_grid %>%
    left_join(joined_df, by = c("datetime" = "level_dtime"))
  
  # moving ave smoothing
  result$series_smoothed <- rollmean(result$series, k = 5, fill = NA)
  
  #Find the descending limb
  output_df <- result %>%
    mutate(difftime_hr = as.numeric(datetime - lag(datetime))/60, #15 minute steps
           difflevel_ft = series_smoothed - lag(series_smoothed),
           rawslope_inhr = (difflevel_ft / difftime_hr) * 12,
           sump_depth_ft = sump_depth_ft,
           orrifice_elev_ft = orrifice_elev_ft) %>%
    mutate(below_sump = ifelse(series < sump_depth_ft, T, F)) %>%
    mutate(below_orrifice = ifelse(series < orrifice_elev_ft, T, F))
  
  
  output_final <- output_df %>%
    select(dtime = datetime, level_ft = series, rain_depth_in, gage_event_uid,rawslope_inhr, sump_depth_ft, orrifice_elev_ft, below_sump, below_orrifice)
  return(output_final)
  
}

# Original longterm sites and A/B testing
targeted_sties <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\06 Continued Monitoring Plan\\Continued Monitoring Plan System List.xlsx") %>%
  filter(`Test Group` == "Original-Long-Term Sedimentation Monitoring" | `Test Group` == "A/B Short-Term Remonitoring") %>%
  select(smp_id = `SMP ID`, test_group = `Test Group`, ow_suffix = Location)

ow_uid_list <- dbGetQuery(marsDBCon, paste("select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow where smp_id in (", toString(paste("'", targeted_sties$smp_id, "'", sep = "")), ")", sep = "")) 

targeted_sties <- targeted_sties %>%
  inner_join(ow_uid_list, by = c("smp_id", "ow_suffix"))

well_measurements <- dbGetQuery(marsDBCon, paste("select * from fieldwork.tbl_well_measurements")) %>%
  filter(ow_uid %in% targeted_sties$ow_uid) %>%
  select(ow_uid, custom_sumpdepth_ft, custom_orificedepth_ft, start_dtime, end_dtime) 

targeted_sties_complete <- targeted_sties %>%
  left_join(well_measurements, by = "ow_uid") %>%
  distinct()


smp_id <- "18-1-1"
ow_suffix <- "OW1"
sump_depth_ft <- 1.65
orifice_tostone_ft <- 0.89
orrifice_elev_ft <- sump_depth_ft + orifice_tostone_ft


sites <- dbGetQuery(marsDBCon, paste("select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow where smp_id in ('", smp_id, "') and ow_suffix = '", ow_suffix, "'", sep = ""))

owdata <- dbGetQuery(marsDBCon, paste0("select ow_uid, dtime, greatest(0, level_ft) as level_ft from data.tbl_ow_leveldata_raw
    where ow_uid in (", paste(sites$ow_uid, collapse = ", "), ")"))


boundaries <- group_by(owdata, ow_uid) %>%
  summarize(n = n(),
            start = min(dtime),
            end = max(dtime)) %>%
  left_join(sites)


#Pull events
cells <- dbGetQuery(marsDBCon, paste0("select * from admin.tbl_smp_gage where smp_id in ('", 
                                      paste0(sites$smp_id, collapse = "', '"), "')"))

events <- dbGetQuery(marsDBCon, paste0("select * from data.tbl_gage_event 
    where gage_uid in (", paste(cells$gage_uid, collapse = ", "), ")")) 

rain_ts <- dbGetQuery(marsDBCon, paste0("select * from data.viw_gage_rainfall 
    where gage_uid in (", paste(cells$gage_uid, collapse = ", "), ")")) %>%
  dplyr::filter(dtime >= boundaries$start & dtime <= boundaries$end) %>%
  dplyr::select(dtime, gage_uid, rainfall_in, gage_event_uid)


# loop to make events regular with zero rainfalls
events_unique <- rain_ts %>%
  select(gage_event_uid) %>%
  distinct() %>%
  na.omit() %>%
  pull()

final_output <- NULL
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
  z_reg$gage_uid       <- na.locf(z_reg$gage_uid)
  z_reg$gage_event_uid <- na.locf(z_reg$gage_event_uid)
  
  # Replace NAs in rainfall with 0 (zero rainfall for missing intervals)
  z_reg$rainfall_in[is.na(z_reg$rainfall_in)] <- 0
  
  final_output <- rbind(final_output, z_reg)
}

# populate for the usage in the function
level_dtime <-owdata$dtime
series <- owdata$level_ft
rain_dtime <- index(final_output)
rain_depth_in <- final_output$rainfall_in
gage_event_uid <- final_output$gage_event_uid

# calulate rates

rates <- FarshadSlope(level_dtime, series, rain_dtime, rain_depth_in , gage_event_uid, sump_depth_ft, orrifice_elev_ft)

# # rates test
# rates_test_df <- rates %>%
#   filter(dtime > as.Date("2018-11-23") & dtime < as.Date("2018-11-28"))
# 
# 
# level_plot <- ggplot(rates_test_df, aes(x = dtime, y = level_ft)) + 
#   geom_point() +
#   geom_vline(xintercept =  as.POSIXct("2018-11-24 16:15:00"), linetype="solid", 
#              color = "blue", size=1.5) +
#   geom_vline(xintercept =  as.POSIXct("2018-11-25 00:15:00"), linetype="solid", 
#              color = "blue", size=1.5) +
#   geom_vline(xintercept =  as.POSIXct("2018-11-26 12:00:00"), linetype="solid", 
#              color = "darkgreen", size=1.5) +
#   geom_vline(xintercept =  as.POSIXct("2018-11-26 18:15:00"), linetype="solid", 
#              color = "darkgreen", size=1.5)+
#   geom_hline(yintercept = 1) +
#   geom_hline(yintercept = 1.84, color = "red") +
#   annotate("text", x = as.POSIXct("2018-11-23 16:15:00"), y = 1.1, label = "Top of Sump")+
#   annotate("text", x = as.POSIXct("2018-11-23 16:15:00"), y = 1.9, label = "Orifice Elev")+
#   ggtitle("14-1-2 OW1- Nov 2018")
#   
#   
# rates_plot <- ggplot(rates_test_df, aes(x = dtime, y = rawslope_inhr)) + 
#   geom_point() +
#   geom_vline(xintercept =  as.POSIXct("2018-11-24 16:15:00"), linetype="solid", 
#              color = "blue", size=1.5) +
#   geom_vline(xintercept =  as.POSIXct("2018-11-25 00:15:00"), linetype="solid", 
#              color = "blue", size=1.5) +
#   geom_vline(xintercept =  as.POSIXct("2018-11-26 12:00:00"), linetype="solid", 
#              color = "darkgreen", size=1.5) +
#   geom_vline(xintercept =  as.POSIXct("2018-11-26 18:15:00"), linetype="solid", 
#              color = "darkgreen", size=1.5)
# 
# combined <- level_plot/rates_plot

# Tag post-event data

events_start_stop <- events %>%
  select(gage_event_uid, eventdatastart, eventdataend) %>%
  filter(gage_event_uid %in% rates$gage_event_uid)

rates_between_events <- NULL
rates_during_events <- rates %>%
  filter(!is.na(gage_event_uid)) %>%
  mutate(post_gage_event_uid = NA)

for (j in 1:(nrow(events_start_stop)-1)) {
  rates_temp <- rates %>%
    filter(dtime > events_start_stop$eventdataend[j] & dtime < events_start_stop$eventdatastart[j+1]) %>%
    mutate(post_gage_event_uid = events_start_stop$gage_event_uid[j])
  
  rates_between_events <- rbind(rates_between_events, rates_temp)
  
}

complete_rates <- rbind(rates_between_events, rates_during_events) %>%
  arrange(dtime)

# filter to grab all negative slopes
# PULL TEMP DATA

# post events
postevent_rates_trend_analysis <- complete_rates %>%
  filter(rawslope_inhr < 0 &
           !is.na(post_gage_event_uid) &
           level_ft < 2.84 &
           level_ft > 1.84)

# get median of the recession rates
postevent_rates_trend_analysis_grouped <- postevent_rates_trend_analysis %>%
  group_by(post_gage_event_uid) %>%
  summarise(median_rate = median(rawslope_inhr), rain_date = as.Date(min(dtime)))

# plot
post_trend_plot <- ggplot(postevent_rates_trend_analysis_grouped, aes(x = rain_date, y = median_rate)) + 
  geom_point() +
  ylim(0, -6) +
  ggtitle("Post Rain Median Recession Data 1 foot above Orifice") +
  scale_x_date(
    date_breaks = "1 year",     # show a tick every year
    date_labels = "%Y"          # format labels as 4-digit years
  )

#during events
duringevent_rates_trend_analysis <- complete_rates %>%
  filter(rawslope_inhr < 0 &
           is.na(post_gage_event_uid) &
           level_ft < orrifice_elev_ft &
           level_ft > sump_depth_ft)

# get median of the recession rates
duringevent_rates_trend_analysis_grouped <- duringevent_rates_trend_analysis %>%
  group_by(gage_event_uid) %>%
  summarise(median_rate = median(rawslope_inhr), rain_date = as.Date(min(dtime)))

# plot
during_trend_plot <- ggplot(duringevent_rates_trend_analysis_grouped, aes(x = rain_date, y = median_rate)) + 
  geom_point() +
  ylim(0, -6) +
  ggtitle("During Rain Median Recession Data 1 foot above Orifice") +
  scale_x_date(
    date_breaks = "1 year",     # show a tick every year
    date_labels = "%Y"          # format labels as 4-digit years
  )


# during and post event
all_rates_trend_analysis_grouped <- complete_rates %>%
  filter(rawslope_inhr < 0 &
           level_ft < orrifice_elev_ft &
           level_ft > sump_depth_ft) %>%
  mutate(all_event = ifelse(is.na(gage_event_uid), post_gage_event_uid, gage_ev ent_uid)) %>%
  group_by(all_event) %>%
  summarise(median_rate = median(rawslope_inhr), rain_date = as.Date(min(dtime)))

# plot
all_trend_plot <- ggplot(all_rates_trend_analysis_grouped, aes(x = rain_date, y = median_rate)) + 
  geom_point() +
  ylim(0, -6) +
  ggtitle("During and Post Rain Median Recession Data 1 foot above Orifice") +
  scale_x_date(
    date_breaks = "1 year",     # show a tick every year
    date_labels = "%Y"          # format labels as 4-digit years
  )

combined_trend <- post_trend_plot/during_trend_plot/all_trend_plot

