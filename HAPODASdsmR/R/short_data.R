#' 150 obs. of 9 variables
#'
#' A dataframe as produced by 'convertHAPODAS' from raw HAPODAS data rows 600:749.
#'
#' @format A dataframe with 150 obs. of  9 variables:
#' \describe{
#'   \item{datetime}{POSIXct, format: "%Y-%m-%d %H:%M:%S"}
#'   \item{lats}{num: Decimal degree latitude}
#'   \item{longs}{num: Decimal degree longitude}
#'   \item{label}{Factor: Unique transect line label}
#'   \item{onoff}{Factor w/ 2 levels "OFF","ON": Indicates 'on' or 'off' effort designation}
#'   \item{section}{Factor w/ 2 levels "a","b": Indicates if transect line was coverd in multiple sections}
#'   \item{travel}{num: distance traveled in meters from last datapoint}
#'   \item{tdiff}{num:  time in seconds between this point and the next point}
#'   \item{orriginal}{logi: indicates that this is an original datapoint, not an interpoated one}
#' }
#'
#'

"short"

#' 150 obs. of all HAPODAS variables (AKA *RAW HAPODAS*): needed for 'Distdata()', can also be used for 'convertHAPODAS'
#'
#' short_hapodas <- hapodas[600:749,]
#' usethis:: use_data(short_hapodas)
#'

"short_hapodas"
