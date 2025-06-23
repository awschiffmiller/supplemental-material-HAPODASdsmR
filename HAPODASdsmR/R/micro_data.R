#' 10 obs. of 9 variables
#'
#' A dataframe as produced by `convertHAPODAS` from raw HAPODAS data rows 547:556,
#' with "label", "onoff", and "section" edited to represent conditions of a larger
#' dataset better.
#'
#' @format A dataframe with 10 obs. of  9 variables:
#' \describe{
#'   \item{datetime}{POSIXct, format: "%Y-%m-%d %H:%M:%S"}
#'   \item{lats}{num: Decimal degree latitude}
#'   \item{longs}{num: Decimal degree longitude}
#'   \item{label}{Factor: Unique transect line label}
#'   \item{onoff}{Factor w/ 2 levels "OFF","ON": Indicates 'on' or 'off' effort designation}
#'   \item{section}{Factor w/ 2 levels "a","b": Indicates if transect line was covered in multiple sections}
#'   \item{travel}{num: distance traveled in meters from last datapoint}
#'   \item{tdiff}{num: time in seconds between this point and the next point}
#'   \item{orriginal}{logi: indicates that this is an original datapoint, not an interpolated one}
#'  }
#'
#'
#'
#'

"micro"
