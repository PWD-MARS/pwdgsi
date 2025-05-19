old_baseline <- function(dtime_est, level_ft, max_infil_rate_inhr = 1){
  
  #get difference in timesteps to determine steps needed in moving average
  step_diff <- as.numeric(difftime(dtime_est[length(dtime_est)], dtime_est[length(dtime_est) - 1], units = "mins"))

  if(step_diff == 5){ #5 minute interval
    steps <- 3
    #apply a 15-minute simple moving average
    level_ft <- zoo::rollmean(level_ft, steps, fill = NA)
  }else{ #15 minute interval
    steps <- 1
  }
  level_ft
  # 
  # #create dataframe of dtime_est and level, and discard NAs formed at the edges during the moving average
  # df <- data.frame(dtime_est, level_ft) %>% dplyr::filter(!is.na(level_ft))
  # 
  # last_point <- round(df$level_ft[length(df$level_ft)], 4)
  # 
  # test <- df %>% #Check if difference between level over x timesteps is less than 0.01ft
  #   dplyr::mutate(lag_1 = dplyr::lag(level_ft, steps), 
  #                 diffr = abs(level_ft - lag_1)) %>% 
  #   dplyr::arrange(desc(dtime_est)) %>% 
  #   head(5)
  # 
  # #0.25 in/hr or lower is considered "not infiltrating". This is converted to ft/(15 minutes)
  # depth_change <- max_infil_rate_inhr*1/12*15/60
  # 
  # if(min(test$diffr, na.rm = TRUE) > depth_change){
  #   return(NA)
  # }else{
  #   return(round(last_point, 4))
  # }
  
}