old_yday <- function(dtime_est){
  
  lubridate::yday(dtime_est) + lubridate::hour(dtime_est)/24 + lubridate::minute(dtime_est)/(24*60) + lubridate::second(dtime_est)/(24*60*60)
}