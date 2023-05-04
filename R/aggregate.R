#' Aggregate into monthly means (even precip)
#'
#' @export
#' @param x tibble of GHCNd station data
#' @return monthly summary (by station) tibble
aggregate_monthly = function(x){
  lut = station_lut(x)
  nm = lut$NAME |>
    rlang::set_names(lut$STATION)
  dplyr::mutate(x, date = as.Date(format(DATE, "%Y-%m-01")), .after = STATION) |>
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