#' Check if a date is the proper yyyy-mm-dd format
#'
#' @param date Date value to check
#' @param tz Timezone character value
#'
#' @return The date with yyyy-mm-dd format or NULL if wrong format with warning
#' @export
#'
check_date <- function(date, tz) {
  tryCatch(lubridate::parse_date_time(date, c("%Y-%m-%d", "%Y/%m/%d"),
                                      tz = tz), warning = function(e) {
    message("Date is not formatted correctly")
  },
  date)
}
