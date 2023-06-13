#' Clip a table so that only complete intervals are present (for subsequent aggregation)
#'
#' @param x tibble with 'STATION' and 'DATE' variable
#' @param by character, one of "year" (default) or "month"
#' @param min_count numeric defaults to 364, but adjust to 28 for month
#' @return tibble clipped to include only complete intervals
complete_intervals = function(x, by = c("year", "month")[1], min_count = 364){
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  dplyr::mutate(x, interval_ = format(.data$DATE, fmt)) |>
  dplyr::group_by(STATION, interval_) |>
  dplyr::group_map(
    function(tbl, key){
      if (nrow(tbl) < min_count){
        return(NULL)
      } else {
        return(tbl)
      }
    }, .keep = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::select(-dplyr::all_of("interval_"))
}


#' Aggregate into annual means (even precip)
#'
#' @export
#' @param x tibble of GHCNd station data
#' @return monthly summary (by station) tibble
aggregate_annual = function(x){
  lut = station_lut(x)
  nm = lut$NAME |>
    rlang::set_names(lut$STATION)
  
  complete_intervals(x, by = "year", min_count = 364) |>
    dplyr::mutate(date = as.Date(format(DATE, "%Y-01-01")), .after = STATION) |>
    dplyr::select(-dplyr::all_of(c("DATE", "ELEVATION"))) |>
    dplyr::rename(DATE = date) |>
    dplyr::group_by(STATION, DATE) |>
    dplyr::summarize(across(dplyr::where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
    dplyr::mutate(NAME = nm[STATION], .after = STATION) |>
    dplyr::mutate(YEAR = as.numeric(format(DATE, "%Y")), .after = DATE)
}



#' Aggregate into monthly means (even precip)
#'
#' @export
#' @param x tibble of GHCNd station data
#' @return monthly summary (by station) tibble
aggregate_monthly = function(x){
  lut = station_lut(x)
  nm = lut$NAME |>
    rlang::set_names(lut$STATION)
  
  complete_intervals(x, by = "month", min_count = 28) |>
  dplyr::mutate(date = as.Date(format(DATE, "%Y-%m-01")), .after = STATION) |>
  dplyr::select(-dplyr::all_of(c("DATE", "ELEVATION"))) |>
  dplyr::rename(DATE = date) |>
  dplyr::group_by(STATION, DATE) |>
  dplyr::summarize(across(dplyr::where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
  dplyr::mutate(NAME = nm[STATION], .after = STATION) |>
  dplyr::mutate(MONTH = factor(format(DATE, "%b"), levels = month.abb), .after = DATE)
}

#' Compute anomalies for numeric columns in a grouped table
#' 
#' @export
#' @param x tibble
#' @return the input tibble transformed into per group anomalies (anom = value - mean(value)) 
anomaly = function(x){
  nmx = colnames(x)
  r = dplyr::reframe(x, across(dplyr::where(is.numeric), \(x) x - mean(x, na.rm = TRUE)))
  ix <- which(!(nmx %in% names(r)))
  if (length(ix) > 0){
    r = dplyr::bind_cols(r, dplyr::select(x, ix)) |>
      dplyr::select(nmx)
  }
  r
}