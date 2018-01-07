#' @title Find MODIS dates included in selected processing period
#' @description Accessory function to find the folders corresponding to the requested
#' dates period within the full list retrieved by get_moddirs
#' @param dates 2- element string array specifying start/end dates (yyyy.mm.dd)
#' for which the http addresses of folders in lpdaac should be retrieved
#' (e.g., c("2015.1.1", "2015.12.31)
#' @param date_dirs data frame full list of folders in lpdaac archive for product of interest
#' @return array of folder names containing data for the MODIS product acquired in
#' the period specified by "dates"
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @author Lorenzo Busetto, phD (2017) \email{lbusett@@gmail.com}
#' @note License: GPL 3.0
#'
get_mod_dates <- function(dates, date_dirs) {
  
  tmp_dates     <- as.Date(dates, format = "%Y.%m.%d")
  tmp_date_dirs <- as.Date(date_dirs, format = "%Y.%m.%d")
  if (length(dates) == 1) {
    date_dirs <- date_dirs[tmp_date_dirs == tmp_dates[1]]
  }
  if (length(dates) == 2) {
    date_dirs <- date_dirs[tmp_date_dirs >= tmp_dates[1] &
                             tmp_date_dirs <= tmp_dates[2]]
  }
  if (length(dates) == 4) {
    # deals with the case of seasonal download, when yy != start_year !=
    # end_year
    date_dirs <- c(
      date_dirs[tmp_date_dirs >= tmp_dates[1] & tmp_date_dirs <= tmp_dates[2]],
      date_dirs[tmp_date_dirs >= tmp_dates[1] & tmp_date_dirs <= tmp_dates[4]]
    )
  }
  
  return(date_dirs)
}
