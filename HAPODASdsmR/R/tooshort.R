#' combine shortest segments with full segments within the same transect
#'
#' adds segmets < 1/2 seg length to previous segment IF label, onoff, and  section designations are identical.
#'
#' @param data a dataframe with specified columns:
#'
#' datetime: POSIXct class, date and time of datapoint. One row for every second
#'
#' lats: numeric class, decimal latitude of datapoint. Interpolated for every second between orriginal datapoints.
#'
#' longs: numeric class, decimal longitude of datapoint. Interpolated for every second between orriginal datapoints.
#'
#' label: character or factor class, unique line label for transect
#'
#' onoff: character or factor class, on or off effort designation for line
#'
#' section: character or factor class, section of line
#'
#' travel: numeric class, distance traveled since LAST data point IN METERS
#'
#' orriginal: Logical class, designates row as an orriginal datapoint "True" or interpolated point "FALSE"
#'
#' cum_dist: numeric class, from `cumsum_by_group`, cumulative distance in meteres for each unique label/onoff/section combination
#'
#' mod: numeric class, the modulus of cum_dist by seg.length
#'
#' cutpoint: Character class: identifies "Start" and "End" points of each segment. Points inside a segment are designated "0"
#'
#'
#' @param seg.length segments shorter than this are 'too short'
#'
#'
#' @return a dataframe with specified columns:
#'
#' datetime: POSIXct class, date and time of datapoint. One row for every second
#'
#' lats: numeric class, decimal latitude of datapoint. Interpolated for every second between orriginal datapoints.
#'
#' longs: numeric class, decimal longitude of datapoint. Interpolated for every second between orriginal datapoints.
#'
#' label: character or factor class, unique line label for transect
#'
#' onoff: character or factor class, on or off effort designation for line
#'
#' section: character or factor class, section of line
#'
#' travel: numeric class, distance traveled since LAST data point IN METERS
#'
#' orriginal: Logical class, designates row as an orriginal datapoint "True" or interpolated point "FALSE"
#'
#' cum_dist: numeric class, from `cumsum_by_group`, cumulative distance in meteres for each unique label/onoff/section combination
#'
#' mod: numeric class, the modulus of cum_dist by seg.length
#'
#' cutpoint: Character class: identifies "Start" and "End" points of each segment. Points inside a segment are designated "0"
#'
#' tooshort: logical class: identifies if segment is shorter that seg.length
#'
#' @author Abigail Schiffmiller
#' @export
#'
#' @example
#' micro_interp <- interp_by_secs(micro)
#' micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
#'                                          grpVarLst = list (micro_interp$label,
#'                                          micro_interp$onoff, micro_interp$section))
#'
#'
tooshort <- function(data, seg.length){
   df <- df
   tooshort <- NULL
   grpVarLst <- list(df$label,
                     df$onoff,
                     df$section)
   seg_length <- stats::ave(df$mod,
                             grpVarLst,
                             FUN = max)
   for(i in seq_along(seg_length)){
   if(seg_length[i] < (seg.length/2)){tooshort[i] = "TRUE"}
      else{tooshort[i] ="FALSE"}}

   df$tooshort <- tooshort

}
