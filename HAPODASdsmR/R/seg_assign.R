#' Define segment start and end points using modulus of cum_dist
#'
#' Runs cumsum on x seperately for each unique combination of the grouping variables
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
#' X onoff: character or factor class, on or off effort designation for line
#'
#' section: character or factor class, section of line
#'
#' travel: numeric class, distance traveled since LAST data point IN METERS
#'
#' orriginal: Logical class, designates row as an orriginal datapoint
#'
#' cum_dist: numeric class, from `cumsum_by_group`, cumulative distance in meteres for each unique label/onoff/section combination
#'
#' @param seg.length Length IN METERS for each segment to be cut to.
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
#' X onoff: character or factor class, on or off effort designation for line
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
#' @author Abigail Schiffmiller
#' @export
#'
#' @example
#'
#' micro_interp <- interp_by_secs(micro)
#' micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
#'                                          grpVarLst = list (micro_interp$label,
#'                                          micro_interp$onoff, micro_interp$section))
#' micro_segs <- seg_assign(data=micro_interp, seg.length = 75)
#' View(micro_segs)
#'


seg_assign <- function(data, seg.length){
   df <- data[which(data$cum_dist > 0),]
   travel <- df$travel
   modulus <- df$cum_dist%%seg.length
   cutpoint<-NULL

   print("define cutpoints 1:")
   progress_bar = utils::txtProgressBar(min=0, max=length(modulus), style = 3, char="=")

   for(i in seq_along(modulus)){
      mod <- modulus[i]
      trav <- travel[i]
      if((seg.length-trav) >= 0){
         if (mod == 0){cutpoint <- c(cutpoint, 0)}
         if (mod <= trav && mod > 0){cutpoint <- c(cutpoint, 'Start')}
         if (mod > trav && mod < (seg.length-trav)){cutpoint <-c (cutpoint, 0)}
         if (mod >= (seg.length-trav)){cutpoint <- c(cutpoint, 'End')}
      }
      if ((seg.length-trav) < 0){cutpoint <- c(cutpoint, 0)}
      utils::setTxtProgressBar(progress_bar, value = i)
   }

   print("define cutpoints 2:")
   progress_bar = txtProgressBar(min=0, max=length(modulus), style = 3, char="=")

   for(i in 1:(length(cutpoint)-1)){
      if(cutpoint[i+1] == 'Start'){cutpoint[i] <- 'End'}
      else {cutpoint[i] <- cutpoint[i]}
      setTxtProgressBar(progress_bar, value = i)
   }

   df$mod <- modulus
   df$cutpoint <- cutpoint

   df
}

