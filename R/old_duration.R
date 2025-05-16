old_duration <- function(dtime_est) {
  
  if(length(dtime_est) == 0){
    return(NA)
  }
  
  # 1. QC checks
  if(!identical(order(dtime_est), 1:length(dtime_est))) {
    stop("Datetime data is not sorted in ascending order.")
  }
  
  if(!all(!duplicated(dtime_est))) {
    stop("Datetime data can not contain duplicates.")
  }
  
  if(!(class(dtime_est)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }
  
  # 2. Calculate storm duration
  event_start <- dtime_est[1]
  #Notes: Assumes 15-minute time increments. As written, code should 
  #only be applied to ONE EVENT at a time
  
  duration_calc <- difftime(dtime_est[length(dtime_est)], event_start, units = "hours")
  duration <- as.double(duration_calc) + 0.25 
  #15-minutes added to account for time subtraction
  
  return(round(as.double(duration), 4))
}