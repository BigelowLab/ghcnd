#' Test a URL
#' 
#' @param x URL
#' @return TRUE if no error occured
check_url =  function(x){
  !httr::http_error(httr::HEAD(x))
}


#' Retrieve the base URI for GHCNd
#' 
#' @export
#' @param ... char, path segments to append to the base uri
#' @return char URI
ghcnd_uri = function(...){
  uri = "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily"
  file.path(uri, ...)
}


#' Retrieve the GHCNd station list
#' 
#' ------------------------------
#' Variable   Columns   Type
#' ------------------------------
#' ID            1-11   Character
#' LATITUDE     13-20   Real
#' LONGITUDE    22-30   Real
#' ELEVATION    32-37   Real
#' STATE        39-40   Character
#' NAME         42-71   Character
#' GSN FLAG     73-75   Character
#' HCN/CRN FLAG 77-79   Character
#' WMO ID       81-85   Character
#'
#' @export
#' @param baseuri char, the baseuri
#' @param bbox st_bbox, sfc, sf or NULL, used to subset the returned stations
#' @return tibble or sf of ghcnd_station_list class
fetch_station_list = function(bbox = NULL, baseuri = ghcnd_uri("doc")){
  uri = file.path(baseuri, "ghcnd-stations.txt")
  if (!check_url(uri)){
    stop("unable to access staion list at ", uri)
  }
  x = readr::read_fwf(uri,
    col_positions = readr::fwf_positions(
      start = c(1,13,22,32,39,42,73,77,81),
      end = c(11,20,30,37,40,71,75,79,85),
      col_names = c("id", "lat", "lon", "elev", "state", "name", 
        "gsn_flag", "hcn_flag", "wmoid")),
    col_types = "cnnnccccc") |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  if (!is.null(bbox)){
    if (inherits(bbox, "bbox")) bbox = sf::st_as_sfc(bbox)
    x = dplyr::slice(x, (sf::st_contains(bbox, x))[[1]])
  }
  
  class(x) <- c("ghcnd_station_list", class(x))
  x
}


#' Fetch one or more station(s)
#' 
#' Natively returns a list of sf objects, potentially one per station id. Why always
#' a list?  Because stations may have differing fields.
#' 
#' @export
#' @param id char, the station identifier(s) or a ghcnd_station_list class
#' @param form char, one of 'sf' or 'raw' for raw text
#' @param bind logical, if TRUE try to bind the sf/text objects by row.  Be advised, this might
#'   not always work.
#' @param transform logical, if TRUE then transform values to real units
#' @param drop_attributes logical, if TRUE drop variables that end with the name "_ATTRIBUTES"
#' @param baseuri char, the base uri
#' @return list of sf objects or text vectors, one per station OR if \code{bind = TRUE}
#'   then a single sf object or text vector
fetch_station <- function(id = c('USW00014764', "USW00094623", 
                                 "USC00172426", "USC00170100"),
                          form = c("sf", "raw")[1],
                          bind = TRUE, 
                          transform = TRUE,
                          drop_attributes = TRUE,
                          baseuri = ghcnd_uri("access")){
  if (inherits(id, "ghcnd_station_list")){
    id = id$id
  }
  if (tolower(form[1]) == "raw"){
    xx = lapply(id, 
      function(stationid){
        uri = file.path(baseuri, paste0(stationid, ".csv"))
        if (!check_url(uri)){
          message("unable to connect to ", uri)
          return(NULL)
        }
        x = try(suppressWarnings(readLines(uri)))
        if (inherits(x, "try-error")){
          message("unable to connect to ", uri)
          return(NULL)
        }
        x
      })
    if (bind) xx <- unlist(xx)
  } else {
    xx = lapply(id, 
      function(stationid){
        uri = file.path(baseuri, paste0(stationid, ".csv"))
        if (!check_url(uri)){
          message("unable to connect to ", uri)
          return(NULL)
        }
        x = try(suppressWarnings(readr::read_csv(uri, show_col_types = FALSE)))
        if (inherits(x, "try-error")){
          message("unable to connect to ", uri)
          return(NULL)
        } else {
          x = sf::st_as_sf(x, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
        }
        
        if (drop_attributes) x <- dplyr::select(x, -dplyr::ends_with("_ATTRIBUTES"))
        if (transform) x <- element_transform(x)
        x
      })
    if (bind) xx = dplyr::bind_rows(xx)
  }
  xx
}


#' Fetch the inventory listing
#' 
#' VII. FORMAT OF "ghcnd-inventory.txt"
#' ------------------------------
#'   Variable   Columns   Type
#' ------------------------------
#' ID           1-11   Character
#' LATITUDE     13-20   Real
#' LONGITUDE    22-30   Real
#' ELEMENT      32-35   Character
#' FIRSTYEAR    37-40   Integer
#' LASTYEAR     42-45   Integer
#' ------------------------------
#'
#' @export
#' @param uri char the uri of the inventory
#' @param stations, null or the output of \code{fetch_station_list}.  If provided,
#'   then station name is added to the output.  Otherwise, only id is available.
#' @param add_duration logical, if TRUE add \code{duration} as a variable
#' @return tibble
fetch_inventory = function(
    uri = "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt",
    add_duration = TRUE,
    stations = NULL){
  
  
  if (!check_url(uri)){
    message("unable to connect to ", uri)
    return(NULL)
  }
  
  x= readr::read_fwf(uri,
                  col_positions = readr::fwf_positions(
                    start = c(1,13,22,32,37,42),
                    end = c( 11,20,30,35,40,45),
                    col_names = c("id", "lat", "lon", "element", "firstyear", "lastyear")),
                  col_types = "cnncnn") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  if (add_duration){
    x = dplyr::mutate(x, duration = .data$lastyear - .data$firstyear + 1)
  }
  
  if (!is.null(stations)){
    x = dplyr::left_join(x, dplyr::select(stations, dplyr::all_of(c("id", "name"))) |> 
                              sf::st_drop_geometry(), 
                        by = "id")
  }
  x  
}
