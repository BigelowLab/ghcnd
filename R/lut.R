#' Generate a look up table for id, name and geometry
#' 
#' @export
#' @param x tibble of GHCNd station data
#' @return table of unique identifiers
station_lut <- function(x){
  
  dplyr::select(x, dplyr::any_of(c("STATION", "NAME"))) |>
    dplyr::group_by(STATION)|>
    dplyr::group_map(
      function(tbl, key) { dplyr::slice(tbl, 1) } , 
      .keep = TRUE) |>
    dplyr::bind_rows()
}

#' Conversion function for weather elements of type
#'
#' @export
#' @param x numeric vector
#' @param mult numeric multiplier
#' @return modified input vector
wx_num <- function(x, mult = 1){
  x*mult
}

#' Conversion function for weather elements of type hhmm
#'
#' @export
#' @param x numeric vector of hhmm
#' @param mult ignored
#' @return time in fractional hours
wx_tim <- function(x, mult = 1){
  s = sprintf("%0.4i", as.numeric(x))
  h = as.numeric(substring(s, 1, 2))
  m = as.numeric(substring(s,3))
  h + m*60
}

#' Conversion function for weather elements of type text
#'
#' @export
#' @param x numeric vector of codes
#' @param mult ignored
#' @return code as 2-digit character
wx_txt <- function(x, mult = 1){
  s = sprintf("%0.2i", as.numeric(x))
}

#' Retrieve a look up table describing weather elements
#' 
#' @export
#' @return tibble of name, fun (for conversion), mult, units and description
element_lut <- function(){
  dplyr::tribble(
    ~name, ~fun, ~mult, ~units, ~description,
    "PRCP", wx_num, 0.1,"mm","Precipitation (tenths of mm)",
    "SNOW", wx_num, 1,"mm","Snowfall (mm)",
    "SNWD", wx_num, 1,"mm","Snow depth (mm)",
    "TMAX", wx_num, 0.1,"C","Maximum temperature (tenths of degrees C)",
    "TMIN", wx_num, 0.1,"C","Minimum temperature (tenths of degrees C)",
    "ACMC", wx_num, 1,"%","Average cloudiness midnight to midnight from 30-second ceilometer data (percent)",
    "ACMH", wx_num, 1,"%","Average cloudiness midnight to midnight from manual observations (percent)",
    "ACSC", wx_num, 1,"%","Average cloudiness sunrise to sunset from 30-second ceilometer data (percent)",
    "ACSH", wx_num, 1,"%","Average cloudiness sunrise to sunset from manual observations (percent)",
    "ADPT", wx_num, 0.1,"C","Average Dew Point Temperature for the day (tenths of degrees C)",
    "ASLP", wx_num, 0.1,"hPa","Average Sea Level Pressure for the day (hPa * 10)",
    "ASTP", wx_num, 0.1,"hPa","Average Station Level Pressure for the day (hPa * 10)",
    "AWBT", wx_num, 0.1,"C","Average Wet Bulb Temperature for the day (tenths of degrees C)",
    "AWDR", wx_num, 1,"°","Average daily wind direction (degrees)",
    "AWND", wx_num, 0.1,"m/s","Average daily wind speed (tenths of meters per second)",
    "DAEV", wx_num, 1,"","Number of days included in the multiday evaporation total (MDEV)",
    "DAPR", wx_num, 1,"","Number of days included in the multiday precipiation total (MDPR)",
    "DASF", wx_num, 1,"","Number of days included in the multiday snowfall total (MDSF)",	  
    "DATN", wx_num, 1,"","Number of days included in the multiday minimum temperature (MDTN)",
    "DATX", wx_num, 1,"","Number of days included in the multiday maximum temperature (MDTX)",
    "DAWM", wx_num, 1,"","Number of days included in the multiday wind movement (MDWM)",
    "DWPR", wx_num, 1,"","Number of days with non-zero precipitation included in multiday precipitation total (MDPR)",
    "EVAP", wx_num, 0.1,"mm","Evaporation of water from evaporation pan (tenths of mm)",
    "FMTM", wx_tim, 1,"hour","Time of fastest mile or fastest 1-minute wind (hours and minutes, i.e., HHMM)",
    "FRGB", wx_num, 1,"cm","Base of frozen ground layer (cm)",
    "FRGT", wx_num, 1,"cm","Top of frozen ground layer (cm)",
    "FRTH", wx_num, 1,"cm","Thickness of frozen ground layer (cm)",
    "GAHT", wx_num, 1,"cm","Difference between river and gauge height (cm)",
    "MDEV", wx_num, 0.1,"mm","Multiday evaporation total (tenths of mm; use with DAEV)",
    "MDPR", wx_num, 0.1,"mm","Multiday precipitation total (tenths of mm; use with DAPR and DWPR, if available)",
    "MDSF", wx_num, 1,"","Multiday snowfall total", 
    "MDTN", wx_num, 0.1,"C","Multiday minimum temperature (tenths of degrees C; use with DATN)",
    "MDTX", wx_num, 0.1,"C","Multiday maximum temperature (tenths of degress C; use with DATX)",
    "MDWM", wx_num, 1,"km","Multiday wind movement (km)",
    "MNPN", wx_num, 0.1,"C","Daily minimum temperature of water in an evaporation pan (tenths of degrees C)",
    "MXPN", wx_num, 0.1,"C","Daily maximum temperature of water in an evaporation pan (tenths of degrees C)",
    "PGTM", wx_tim, 1,"hour","Peak gust time (hours and minutes, i.e., HHMM)",
    "PSUN", wx_num, 1,"%","Daily percent of possible sunshine (percent)",
    "RHAV", wx_num, 1,"%","Average relative humidity for the day (percent)",
    "RHMN", wx_num, 1,"%","Minimum relative humidity for the day (percent)",
    "RHMX", wx_num, 1,"%","Maximum relative humidity for the day (percent)",
    "TAVG", wx_num, 0.1,"","Average temperature (tenths of degrees C)",
    "THIC", wx_num, 0.1,"mm","Thickness of ice on water (tenths of mm)",	
    "TOBS", wx_num, 0.1,"C","Temperature at the time of observation (tenths of degrees C)",
    "TSUN", wx_num, 1,"minutes","Daily total sunshine (minutes)",
    "WDF1", wx_num, 1,"°","Direction of fastest 1-minute wind (degrees)",
    "WDF2", wx_num, 1,"°","Direction of fastest 2-minute wind (degrees)",
    "WDF5", wx_num, 1,"°","Direction of fastest 5-second wind (degrees)",
    "WDFG", wx_num, 1,"°","Direction of peak wind gust (degrees)",
    "WDFI", wx_num, 1,"°","Direction of highest instantaneous wind (degrees)",
    "WDFM", wx_num, 1,"°","Fastest mile wind direction (degrees)",
    "WDMV", wx_num, 1,"km","24-hour wind movement (km)",	   
    "WESD", wx_num, 0.1,"mm","Water equivalent of snow on the ground (tenths of mm)",
    "WESF", wx_num, 0.1,"mm","Water equivalent of snowfall (tenths of mm)",
    "WSF1", wx_num, 0.1,"m/s","Fastest 1-minute wind speed (tenths of meters per second)",
    "WSF2", wx_num, 0.1,"m/s","Fastest 2-minute wind speed (tenths of meters per second)",
    "WSF5", wx_num, 0.1,"m/s","Fastest 5-second wind speed (tenths of meters per second)",
    "WSFG", wx_num, 0.1,"m/s","Peak gust wind speed (tenths of meters per second)",
    "WSFI", wx_num, 0.1,"m/s","Highest instantaneous wind speed (tenths of meters per second)",
    "WSFM", wx_num, 0.1,"m/s","Fastest mile wind speed (tenths of meters per second)",
    "SN"  , wx_num, 0.1,"°", "Minimum soil temperature (tenths of degrees C)",
    "SX"  , wx_num, 0.1,"°", "Maximum soil temperature (tenths of degrees C)")
    #"WT"  , wx_txt, NA, NA, "Weather Type",
    #"WV"  , wx_txt, NA, NA, "Weather Type in the Vicinity")
}





#' Transform variables in a GHCNd station dataset as appropriate 
#' 
#' @export
#' @param x tibble of GHCNd station data
#' @param lut tibble, look up table for element transforms
element_transform = function(x, lut = element_lut()){
  
  # step through the rows of the lut,
  # find the matching element
  # transform in place
  xnames = colnames(x)
  lut = mutate(lut, pattern = paste0("^", name))
  for (i in seq_len(nrow(lut))){
    ix = grep(lut$pattern[i], xnames)
    if (length(ix) > 0){
      #cat(xnames[ix], "\n")
      for (j in ix) x[[xnames[j]]] = suppressWarnings(lut$fun[[i]](x[[xnames[j]]], mult = lut$mult[i]))
    }
  }
  x
}


decode_soiltemp <- function(x){

  # SN*# = Minimum soil temperature (tenths of degrees C)
  #   where * corresponds to a code
  # for ground cover and # corresponds to a code for soil 
  # depth.  
  # 
  # Ground cover codes include the following:
  # 0 = unknown
  # 1 = grass
  # 2 = fallow
  # 3 = bare ground
  # 4 = brome grass
  # 5 = sod
  # 6 = straw multch
  # 7 = grass muck
  # 8 = bare muck
  # 
  # Depth codes include the following:
  # 1 = 5 cm
  # 2 = 10 cm
  # 3 = 20 cm
  # 4 = 50 cm
  # 5 = 100 cm
  # 6 = 150 cm
  # 7 = 180 cm
  # 
  # SX*# = Maximum soil temperature (tenths of degrees C) 
  #   where * corresponds to a code for ground cover 
  # and # corresponds to a code for soil depth. 
  # See SN*# for ground cover and depth codes. 
}

decode_weathertype = function(x){

  # WT** = Weather Type where ** has one of the following values:
  #   
  # 01 = Fog, ice fog, or freezing fog (may include heavy fog)
  # 02 = Heavy fog or heaving freezing fog (not always distinquished from fog)
  # 03 = Thunder
  # 04 = Ice pellets, sleet, snow pellets, or small hail 
  # 05 = Hail (may include small hail)
  # 06 = Glaze or rime 
  # 07 = Dust, volcanic ash, blowing dust, blowing sand, or 
  # blowing obstruction
  # 08 = Smoke or haze 
  # 09 = Blowing or drifting snow
  # 10 = Tornado, waterspout, or funnel cloud 
  # 11 = High or damaging winds
  # 12 = Blowing spray
  # 13 = Mist
  # 14 = Drizzle
  # 15 = Freezing drizzle 
  # 16 = Rain (may include freezing rain, drizzle, and freezing drizzle) 
  # 17 = Freezing rain 
  # 18 = Snow, snow pellets, snow grains, or ice crystals
  # 19 = Unknown source of precipitation 
  # 21 = Ground fog 
  # 22 = Ice fog or freezing fog
  # 
  # WV** = Weather in the Vicinity where ** has one of the following 
  # values:
  #   
  # 01 = Fog, ice fog, or freezing fog (may include heavy fog)
  # 03 = Thunder
  # 07 = Ash, dust, sand, or other blowing obstruction
  # 18 = Snow or ice crystals
  # 20 = Rain or snow shower# 
}