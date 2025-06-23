#' 200 obs. of 12 variables
#'
#' A dataframe as produced by SplitTransects from "short.rda" dataset
#' through `segmentize` function (see SplitTransects-vignette),
#' with "label", "onoff", and "section" edited to represent conditions of a larger
#' dataset better.
#'
#' @format A dataframe with 200 obs. of  12 variables:
#' \describe{
#'   \item{datetime}{POSIXct, format: "%Y-%m-%d %H:%M:%S"}
#'   \item{lats}{num: Decimal degree latitude}
#'   \item{longs}{num: Decimal degree longitude}
#'   \item{label}{Factor: Unique transect line label}
#'   \item{onoff}{Factor w/ 2 levels "OFF","ON": Indicates 'on' or 'off' effort designation}
#'   \item{section}{Factor w/ 2 levels "a","b": Indicates if transect line was covered in multiple sections}
#'   \item{travel}{num: distance traveled in meters from last datapoint}
#'   \item{orriginal}{logi: indicates that this is an original datapoint, not an interpolated one}
#'   \item{cum_dist}{num: result of cumsum_by_group function}
#'   \item{mod}{num: result of seg_assign function}
#'   \item{cutpoint}{chr: result of seg_assign function}
#'   \item{segnum}{int: result of seg_num function}
#' }
#'
#'
#'          short <- short
#'          short_interp <- interp_by_secs(short)
#'          short_interp$cum_dist <- cumsum_by_group(short_interp$travel,
#'                                                   grpVarLst = list(short_interp$label,
#'                                                                    short_interp$onoff,
#'                                                                    short_interp$section))
#'          short_segs <- seg_assign(short_interp, seg.length = 1000)
#'          short_segs <- seg_num(short_segs)
#'          shortSegments <- segmentize(short_segs)
#'
#'          usethis::use_data(shortSegments, overwrite = TRUE)
#' #'

"shortSegments"
