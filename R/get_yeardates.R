#' @title identify dates to be processed for a year
#' @description helper function needed to identify the ranges of dates to
#'   be processed for a given year as a function of `download_range` selection
#'   and starting/ending dates and years
#' @param yy `numeric` year for which the processing dates need to be identified
#' @param start_year `numeric` start year of current `MODIStsp_process` run
#' @param end_year `numeric` end year of current `MODIStsp_process` run.
#' @param start_date `character`` Start date for images download and preprocessing
#'   (yyyy.mm.dd) of current `MODIStsp_process` run.
#' @param end_date `character` Start date for images download and preprocessing
#'   (yyyy.mm.dd) of current `MODIStsp_process` run.
#' @inheritParams MODIStsp_process
#' @return OUTPUT_DESCRIPTION
#' @rdname get_yeardates
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
get_yeardates <- function(download_range,
                          yy,
                          start_year, end_year,
                          start_date, end_date) {

  if (download_range == "full") {
    # Create string representing the dates to be processed in the case
    # of "full" time processing

    if (yy == start_year & yy == end_year) {
      dates <- c(start_date, end_date)
    }

    if (yy == start_year & yy != end_year) {
      dates <- c(start_date, paste0(as.character(yy), ".12.31"))
    }

    if (yy != start_year & yy != end_year) {
      dates <- c(paste0(as.character(yy), ".1.1"),
                 paste0(as.character(yy), ".12.31"))
    }

    if (yy != start_year & yy == end_year) {
      dates <- c(paste0(as.character(yy), ".1.1"), end_date)
    }

  } else {

    # Create string representing the dates to be processed in the case
    # of splitted processing

    # the starting month-day
    start_seas <- as.Date(strftime(as.Date(start_date, format = "%Y.%m.%d"),
                                   "0-%m-%d"))
    # the ending month-day
    end_seas   <- as.Date(strftime(as.Date(end_date, format = "%Y.%m.%d"),
                                   "0-%m-%d"))

    # TRUE if the period includes new year's eve, false if not
    nye_incl   <- start_seas > end_seas

    if (!nye_incl) {
      dates    <- c(gsub(paste0("^", start_year), yy, start_date),
                    gsub(paste0("^", end_year), yy, end_date))
    } else {

      if (yy == start_year & yy != end_year) {
        dates  <- c(gsub(paste0("^", start_year), yy, start_date),
                    paste0(as.character(yy), ".12.31"))
      }

      if (yy != start_year & yy != end_year) {

        dates  <- c(paste0(as.character(yy), ".1.1"),
                    gsub(paste0("^", end_year), yy, end_date),
                    gsub(paste0("^", start_year), yy, start_date),
                    paste0(as.character(yy), ".12.31"))
      }

      if (yy != start_year & yy == end_year) {
        dates  <- c(paste0(as.character(yy), ".1.1"),
                    gsub(paste0("^", end_year), yy, end_date))
      }

    }

  }
  dates
}
