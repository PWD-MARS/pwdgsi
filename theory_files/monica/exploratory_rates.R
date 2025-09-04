library(tidyverse)
library(RPostgres)
library(pool)

#Connect
marsDBCon <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user= Sys.getenv("admin_uid"),
  password = Sys.getenv("admin_pwd"),
  timezone = NULL)

#Establish period of record for test sites
  sites <- dbGetQuery(marsDBCon, "select ow_uid, smp_id, ow_suffix from fieldwork.tbl_ow 
    where smp_id in ('81-1-1', '1296-6-2', '250-1-1', '250-2-1', '589-1-1', '1202-4-1') 
      and ow_suffix = 'OW1'")
  
  owdata <- dbGetQuery(marsDBCon, paste0("select ow_uid, dtime, greatest(0, level_ft) as level_ft from data.tbl_ow_leveldata_raw
    where ow_uid in (", paste(sites$ow_uid, collapse = ", "), ")"))
  
  boundaries <- group_by(owdata, ow_uid) %>%
    summarize(n = n(),
              start = min(dtime),
              end = max(dtime)) %>%
    left_join(sites)
  
  #Print the monitoring overlap between each site
  record <- left_join(owdata, sites) %>% ggplot(aes(x = dtime, y = smp_id)) +
    geom_point() +
    ggtitle("Monitoring Period of Record for Selected SMPs")
  ggsave("periodofrecord.png", record)
  
  
  # #1296-6-2 has only been monitored in recent months, and other sites have data
  #  #for several preceeding years. For better comparison's sake, we will use the
  #  #richest period of overlap, July-Nov 2019 for those four sites.
  # starttime = '2019-01-01'
  # endtime = '2019-12-31'
  
  sites <- filter(sites, smp_id %in% c('81-1-1', '250-1-1', '250-2-1', '589-1-1'))
  
  # owdata <- filter(owdata, dtime %within% interval(starttime, endtime, 
  #   tzone = "America/New_York")) 

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

  #For each well, filter to just that well
  #For each storm, mutate in the event ID
    #Then calculate the slopes and assign them to a variable
  
  hosttable <- NULL
  for(i in 1:nrow(sites)){
    sitedata <- filter(owdata, ow_uid == sites$ow_uid[i])
    sitestorms <- filter(events, radar_uid == cells$radar_uid[cells$smp_id == sites$smp_id[i]])
    
    for(j in 1:nrow(sitestorms)){
      stormdata <- filter(sitedata, dtime %within% interval(sitestorms$eventdatastart[j], sitestorms$eventdataend[j] + hours(24)))
      if(nrow(stormdata) == 0){
        next
      }
      
      stormdata <- mutate(stormdata, radar_event_uid = sitestorms$radar_event_uid[j],
                          rawslope_inhr = monicaDescendingLimbSlope(dtime, level_ft, sitestorms$radar_event_uid[j]))
      
      
      
      hosttable <- rbind(hosttable, stormdata)
    }
  }
  
  successes <- filter(hosttable, complete.cases(hosttable)) %>%
    left_join(sites)  %>%
    filter(rawslope_inhr < 10)
  
  scatter <- ggplot(successes, aes(x = dtime, y = smp_id)) + 
    geom_point() +
    ggtitle("Successful Slope Calculations for Tested SMPs")
  
  ggsave("FY20successes.PNG", scatter)
  
#oct <- filter(successes, dtime <= ymd('2019-11-01'), dtime >= ymd('2019-10-01'))
stats <- data.frame(smp_id = c("250-1-1", "250-2-1", "81-1-1", "589-1-1"),
                    note = c("Elevated, Orifice", "Elevated, Orifice", "Elevated, Capped", "Sumped, Orifice"),
                    stringsAsFactors=FALSE) %>%
  mutate(string = paste(smp_id, note, sep = "\n"))

octt <- left_join(successes, stats)

boxes <- ggplot(octt, aes(x = string, y = rawslope_inhr)) + geom_boxplot() + ggtitle("Boxplot of Slopes for Period of Record")
ggsave("boxplots.png", boxes)


successes_inches <- mutate(successes, level_in = level_ft * 12, 
                           stage_in = as.factor(trunc(level_in)),
                           under_thresh = rawslope_inhr < 0.25) %>%
  filter(under_thresh == FALSE) %>%
  filter(level_in <= 12) %>%
  filter(rawslope_inhr < 10) %>%
  left_join(stats)

slopes <- ggplot(successes_inches, aes(x = stage_in, y = rawslope_inhr, color = note)) + geom_boxplot() + 
  ggtitle("Boxplot of slopes by inch") + 
  theme(text = element_text(size = 20)) +
  xlab("Inches above bottom of storage") +
  ylab("Recession rate (in/hr)")
ggsave("slopes.png", slopes, width = 15, height = 8, units = "in")

successes_inches <- mutate(successes_inches, system_id = gsub("(\\d+-\\d+-\\d+)", "\\1", smp_id))

systems <- dbGetQuery(marsDBCon, "select smp_id, assumption_orificeheight_ft from external.viw_greenit_subsurface_unlined")

greenit <- left_join(successes_inches, systems)

mutate(greenit, below_orifice = level_ft < assumption_orificeheight_ft) -> odata

orifices <- ggplot(odata, aes(x = below_orifice, y = rawslope_inhr, color = note)) + geom_boxplot() + 
  ggtitle("Boxplot of slopes by above/below orifice") + 
  theme(text = element_text(size = 20)) +
  xlab("Measured Below Orifice?") +
  ylab("Recession rate (in/hr)")
ggsave("orifices.png", orifices, width = 15, height = 8, units = "in")

