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
    dplyr::select(-DATE, -ELEVATION) |>
    dplyr::rename(DATE = date) |>
    dplyr::group_by(STATION, DATE) |>
    dplyr::summarize(across(dplyr::where(is.numeric), \(x) mean(x, na.rm = TRUE))) |>
    dplyr::mutate(NAME = nm[STATION], .after = STATION)
}