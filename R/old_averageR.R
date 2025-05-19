old_average <- function(dtime_est, rainfall_in) {
  
  if(length(dtime_est) == 0 | length(rainfall_in) == 0){
    return(NA)
  }
  
  # 1. QC check (all others covered in called functions)
  if(!(length(dtime_est) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }
  
  # 2. Calculate average intensity
  result <- marsStormDepth_in(rainfall_in) / marsStormDuration_hr(dtime_est)
  return(round(result, 4))
}
