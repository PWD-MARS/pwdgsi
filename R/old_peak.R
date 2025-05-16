old_peak <- function(dtime_est, rainfall_in) {
  
  if(length(dtime_est) == 0 | length(rainfall_in) == 0){
    return(NA)
  }
  
  # 1. QC checks
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0")
  }
  
  if(!(identical(order(dtime_est), 1:length(dtime_est)))) {
    stop("Datetime data is not sorted in ascending order")
  }
  
  if(!all(!duplicated(dtime_est))) {
    stop("Datetime data can not contain duplicates")
  }
  
  if(!(class(dtime_est)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct")
  }
  
  if(!(length(dtime_est) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }
  
  # Alert user if event only contains one measurement 
  if(length(dtime_est) == 1){
    message("Datetime data only contains one measurement.")
  }
  mult <- 4
  
  # 3. Calculate peak intensity
  # Assumes that the interval is 15 minutes.
  maximum <- max(rainfall_in)
  peak <- maximum * mult
  
  return(round(peak, 4))
}