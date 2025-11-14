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

#Establish period of record for test sites_examples = 1359-8-2 OW1, event uid = 243797
  # sites <- dbGetQuery(marsDBCon, "select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow
  #   where smp_id in ('1359-8-2')
  #     and ow_suffix = 'OW1'")

sites <- dbGetQuery(marsDBCon, "select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow
    where smp_id in ('9-1-1')
      and ow_suffix = 'OW1'")
  
owdata <- dbGetQuery(marsDBCon, paste0("select ow_uid, dtime, greatest(0, level_ft) as level_ft from data.tbl_ow_leveldata_raw
    where ow_uid in (", paste(sites$ow_uid, collapse = ", "), ")"))
  
  boundaries <- group_by(owdata, ow_uid) %>%
    summarize(n = n(),
              start = min(dtime),
              end = max(dtime)) %>%
    left_join(sites)
  

#Pull events
  cells <- dbGetQuery(marsDBCon, paste0("select * from admin.tbl_smp_radar where smp_id in ('", 
    paste0(sites$smp_id, collapse = "', '"), "')"))

  events <- dbGetQuery(marsDBCon, paste0("select * from data.tbl_radar_event 
    where radar_uid in (", paste(cells$radar_uid, collapse = ", "), ")")) # and
          #eventdatastart between '", starttime, "' and '", endtime, "'")) 

#Slope function
  monicaDescendingLimbSlope <- function(dtime, series, event_uid){
    #Goals: 
      #1. Find the monotonically decreasing descending limb
        #filter to starttime - endtime+24 hours
        #calculate the raw slope, test < 0, RLE to longest chunk
      #2. Find the instantaneous slope at each timestep there
    
    #If the entire storm is 0s, return NA
    if(all(series <= 0)){
      print(paste("No response for event", event_uid))
      return(rep(NA, length(dtime)))
    }
    
    #Find the descending limb
      storm <- data.frame(dtime, series) %>%
        mutate(difftime_hr = as.numeric(dtime - lag(dtime))/60, #15 minute steps
               difflevel_ft = series - lag(series),
               rawslope_inhr = (difflevel_ft / difftime_hr) * 12,
               negative = rawslope_inhr < 0)
      
      rle.storm <- rle(storm$negative)
      
      #Which parts are descending?
      downs <- which(rle.storm$values == TRUE)
        #If there is no descending limb
        if(length(downs) == 0){
          print(paste("Monotonic ascent for event", event_uid))
          return(rep(NA, length(dtime)))
        }
      
      downlengths <- rle.storm$lengths[downs]
      maxdown <- max(downlengths)
      longestpiece <- which(rle.storm$lengths == maxdown & rle.storm$values == TRUE)[1]

      #length of longest descending piece
      limblength <- rle.storm$lengths[longestpiece]

      #where does the descending limb begin?
      limb.start <- ifelse(longestpiece == 1, 1, 1+sum(rle.storm$lengths[1:(longestpiece-1)]))
      limb.end <- limb.start + limblength - 1
      
      tryCatch({descendinglimb <- storm[seq(limb.start, limb.end), ]},
               error = function(e){
                 browser()
               })
               
      
      #Final return series
        before <- rep(NA, limb.start-1) #NA before the ascending limb
        during <- round(abs(descendinglimb$rawslope_inhr), 4) #Slope during the descending limb
        after <- rep(NA, nrow(storm) - limb.end) #NA after the descending limb
        
      return(c(before, during, after))
  }
  
  # a modification of this method that considers all negative slopes after the water level peak, up to 24 hours after the storm ends
  
  
  
  #Slope function
  farshadDescendingLimbSlope <- function(dtime, series, event_uid){
    #Goals: 
    #1. Find the monotonically decreasing descending limb
    #filter to starttime - endtime+24 hours
    #calculate the raw slope, test < 0, RLE to longest chunk
    #2. Find the instantaneous slope at each timestep there
    
    #If the entire storm is 0s, return NA
    if(all(series <= 0)){
      print(paste("No response for event", event_uid))
      return(rep(NA, length(dtime)))
    }
    
    #Find the descending limb
    storm <- data.frame(dtime, series) %>%
      mutate(difftime_hr = as.numeric(dtime - lag(dtime))/60, #15 minute steps
             difflevel_ft = series - lag(series),
             rawslope_inhr = (difflevel_ft / difftime_hr) * 12,
             negative = rawslope_inhr < 0)
    
    maxlevel_index <-  which(storm$series == max(storm$series))
    postpeak_slopes <- storm[maxlevel_index:length(storm$series), ] 
    postpeak_negative_slopes <- postpeak_slopes[which(postpeak_slopes$negative == T), ]
    
    
    
    return_vec <- rep(NA, length(storm$series))
    return_vec[as.numeric(rownames(postpeak_negative_slopes))] <- abs(postpeak_negative_slopes$rawslope_inhr)
    
    # keep only the values that are part of streaks of 3 or more consecutive non-NA values, and set all others to NA
    r <- rle(!is.na(return_vec))
    r$values <- r$values & r$lengths >= 3
    mask <- inverse.rle(r)

    return_vec[!mask] <- NA
    
    return(return_vec)
    
  }
  


  #For each well, filter to just that well
  #For each storm, mutate in the event ID
    #Then calculate the slopes and assign them to a variable
  
  hosttable <- NULL
  for(i in 1:nrow(sites)){
    sitedata <- filter(owdata, ow_uid == sites$ow_uid[i])
    sitestorms <- filter(events, radar_uid == cells$radar_uid[cells$smp_id == sites$smp_id[i]]) #%>% filter(radar_event_uid == 434703 | radar_event_uid == 434704)
    #sitestorms <- filter(events, radar_uid == cells$radar_uid[cells$smp_id == sites$smp_id[i]]) %>% filter(radar_event_uid == 243797 | radar_event_uid == 243798)
    
   
    
    
    for(j in 1:(nrow(sitestorms)-1)){
      stormdata <- filter(sitedata, dtime %within% interval(sitestorms$eventdatastart[j], sitestorms$eventdatastart[j] + hours(7*24) )) #fifelse(sitestorms$eventdataend[j] + hours(72) < sitestorms$eventdatastart[j+1], sitestorms$eventdataend[j] + hours(24), sitestorms$eventdatastart[j+1])
      if(nrow(stormdata) == 0){
        next
      }
      
      
      # spline smoothing
      stiedata_smoothed <- stormdata
      stiedata_smoothed$time_num <- as.numeric(stiedata_smoothed$dtime)
      spline_model <- smooth.spline(x = stiedata_smoothed$time_num, y = stiedata_smoothed$level_ft, spar = 0.1)
      stiedata_smoothed$level_spline <- predict(spline_model)$y
      
      
      # moving average
      stiedata_smoothed$moving_a_five_level <- rollmean(stiedata_smoothed$level_ft, k = 5, fill = NA)
      stiedata_smoothed$moving_m_five_level <- rollmedian(stiedata_smoothed$level_ft, k = 5, fill = NA)
      

      stormdata <- mutate(stiedata_smoothed, radar_event_uid = sitestorms$radar_event_uid[j],
                          rawslope_inhr = monicaDescendingLimbSlope(dtime, level_ft, sitestorms$radar_event_uid[j]),
                          smoothslope_inhr = monicaDescendingLimbSlope(dtime, level_spline, sitestorms$radar_event_uid[j]),
                          moving_5_inhr = monicaDescendingLimbSlope(dtime, moving_a_five_level, sitestorms$radar_event_uid[j]),
                          moving_median_5_inhr = monicaDescendingLimbSlope(dtime, moving_m_five_level, sitestorms$radar_event_uid[j]),
                          farshadspline_inhr = farshadDescendingLimbSlope(dtime, level_spline, sitestorms$radar_event_uid[j]),
                          farshadsraw_inhr = farshadDescendingLimbSlope(dtime, level_ft, sitestorms$radar_event_uid[j]))
      
      
      
      hosttable <- rbind(hosttable, stormdata)
    }
  }
  
  # successes <- filter(hosttable, complete.cases(hosttable)) %>%
  #   left_join(sites)  
  
  successes <- hosttable
  
  
  # plot the storm
  stormdata$time_num <- as.numeric(stormdata$dtime)
  spline_model <- smooth.spline(x = stormdata$time_num, y = stormdata$level_ft, spar = 0.1)
  stormdata$level_spline <- predict(spline_model)$y
  
  storm <- ggplot(stormdata, aes(x = dtime, y = level_ft)) + 
    geom_point() +
    geom_line(aes(y = level_spline), color = "red") +
    ggtitle("Raw + spine Level")
  
  scatter_raw <- ggplot(successes, aes(x = dtime, y = rawslope_inhr)) + 
    geom_point() +
    ylim(0, 6) +
    ggtitle("Raw Slope Calculations for Longest Streak")
  
  scatter_smooth <- ggplot(successes, aes(x = dtime, y = smoothslope_inhr)) + 
    geom_point() +
    ylim(0, 6) +
    ggtitle("Smoothed (Spline) Slope Calculations for for Longest Streak")
  
  scatter_ma5 <- ggplot(successes, aes(x = dtime, y = moving_5_inhr)) + 
    geom_point() +
    ylim(0, 6) +
    ggtitle("Smoothed (Moving Average of 5 points) Slope Calculations for for Longest Streak")
  
  scatter_me5 <- ggplot(successes, aes(x = dtime, y = moving_median_5_inhr)) + 
    geom_point() +
    ylim(0, 6) +
    ggtitle("Smoothed (Moving Median of 5 points) Slope Calculations for for Longest Streak")
  
  scatter_farshad_spline <- ggplot(successes, aes(x = dtime, y = farshadspline_inhr)) + 
    geom_point() +
    ylim(0, 6) +
    ggtitle("Farshad's function Post-Peak + Spline Slope Calculations")
  
  scatter_farshad_raw <- ggplot(successes, aes(x = dtime, y = farshadsraw_inhr)) + 
    geom_point() +
    ylim(0, 6) +
    ggtitle("Farshad's function Post-Peak for raw slopes")
  
  combined_plot <- (storm + plot_spacer()) / (scatter_raw + scatter_smooth) / (scatter_ma5 + scatter_me5) / (scatter_farshad_raw + scatter_farshad_spline)
  
  spline <- successes %>%
    select(slope = smoothslope_inhr) %>%
    mutate(Category = "Spline (longest Running)")
  
  raw <- successes %>%
    select(slope = rawslope_inhr) %>%
    mutate(Category = "Raw (longest Running)")
  
  ma <- successes %>%
    select(slope = moving_5_inhr) %>%
    mutate(Category = "MovingAve (longest Running)")
  
  farshad_fun_spline <-  successes %>%
    select(slope = farshadspline_inhr) %>%
    mutate(Category = "Farshad_Fun_Spline (Post-Peak)")
  
  farshad_fun_raw <-  successes %>%
    select(slope = farshadsraw_inhr) %>%
    mutate(Category = "Farshad_Fun_Raw (Post-Peak)")
  
  box_df <- rbind(raw, spline, ma, farshad_fun_spline, farshad_fun_raw)
  
  box_raw <- ggplot(box_df, aes(x = Category, y = slope)) + geom_boxplot() + 
    ggtitle("Boxplot of slopes by inch") + 
    theme(text = element_text(size = 20))
  
  
  
  
# only spline 
  # plot the storm
  stormdata$time_num <- as.numeric(stormdata$dtime)
  spline_model <- smooth.spline(x = stormdata$time_num, y = stormdata$level_ft, spar = 0.1)
  stormdata$level_spline <- predict(spline_model)$y
  
  
  # identify timeseries with successful rate calculations
  start_farshad_spline <- stormdata %>%
    filter(!is.na(farshadspline_inhr)) %>%
    select(dtime) %>%
    pull() %>%
    min()
  
  stop_farshad_spline <- stormdata %>%
    filter(!is.na(farshadspline_inhr)) %>%
    select(dtime) %>%
    pull() %>%
    max()
  
  storm_annotated <- ggplot(stormdata, aes(x = dtime, y = level_ft)) + 
    geom_point() +
    geom_line(aes(y = level_spline), color = "red") +
    ggtitle("1359-8-2 OW1, radar_event_uid = 434703 (0.9 in) and 434704 (0.8 in) Jan 2023, Green lines: Start + Stop of 1st Rain, Purple lines: Start + Stop of 2nd Rain, Blue lines: Start + Stop of Rate Calculation") + 
    geom_vline(xintercept =  start_farshad_spline, linetype="solid", 
                 color = "blue", size=1.5) +
    geom_vline(xintercept =  stop_farshad_spline, linetype="solid", 
               color = "blue", size=1.5) +
    geom_vline(xintercept =  sitestorms[1, ]$eventdatastart, linetype="solid", 
               color = "darkgreen", size=1.5) +
    geom_vline(xintercept =  sitestorms[1, ]$eventdataend, linetype="solid", 
               color = "darkgreen", size=1.5) +
    geom_vline(xintercept =  sitestorms[2, ]$eventdatastart, linetype="solid", 
               color = "purple", size=1.5) +
    geom_vline(xintercept =  sitestorms[2, ]$eventdataend, linetype="solid", 
               color = "purple", size=1.5) 
  
  scatter_farshad_spline <- ggplot(successes, aes(x = dtime, y = farshadspline_inhr)) + 
    geom_point() +
    ylim(0, 4) +
    ggtitle("Post-Peak Spline Slope Calculations") 
  
  
  combo_double <- storm_annotated / scatter_farshad_spline
  
  
  
  
  
  