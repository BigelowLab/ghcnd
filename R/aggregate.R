aggregate_monthly = function(x){
  y = dplyr::mutate(x, date = format(DATE, "%Y-%m-01")) |>
    dplyr::select(-DATE, -ELEVATION) |>
    dplyr::group_by(STATION, yearmon) |>
    dplyr::summarize(across(dplyr::where(is.numeric), \(x)mean(x,na.rm = TRUE)))
    
}