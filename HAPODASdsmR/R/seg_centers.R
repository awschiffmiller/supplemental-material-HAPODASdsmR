#' segment centerpoints
#'
#' Identifies the centerpoint of each segment
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
#' segnum: interger class, Unique ID number for each segment
#'
#' sec_label: character class, a combination of Label and section for a single identifying value
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
#' segnum: interger class, Unique ID number for each segment
#'
#' sec_label: character class, a combination of Label and section for a single identifying value
#'
#' Effort: numeric class, the length of each merged segment
#'
#'
#' @author Abigail Schiffmiller
#' @export
#'
#' @example
#' micro_interp <- interp_by_secs(micro)
#' micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
#'                                          grpVarLst = list (micro_interp$label,
#'                                          micro_interp$onoff, micro_interp$section))
#' micro_segs <- seg_assign(data=micro_interp, seg.length = 75)
#' micro_segs <- seg_num(micro_segs)
#' mico_segs <- merge_shortsegs(microsegs, cutoff = 35)
#' View(micro_segs)
#'
#'


seg_centers <- function(data){
   df <- data
   uniqueseg <- unique(df$segnum)
   Effort <- df$Effort
   newcutpoint <- "0"

   print("all segment centerpoints:")
   progress_bar = utils::txtProgressBar(min=0, max=length(uniqueseg), style = 3, char="=")

   for(j in 1:length(uniqueseg)){
      tempdf <- subset(df, segnum == uniqueseg[j])
      cutpoint <- tempdf$cutpoint
      cum_dist <- tempdf$cum_dist
       max <- max(cum_dist)
       min <- min(cum_dist)
      # tempdf$Effort <- rep((max-min), length.out=nrow(tempdf))
      center <- (max - min)/2
      mod <- tempdf$mod
      trav <- (tempdf$travel)

         for(i in seq_along(cum_dist)){
            if ((cum_dist[i-1]-min) < (center) && (cum_dist[i]-min) >= (center))
               {cutpoint[i] <- "center"}
            else {cutpoint[i] <- cutpoint[i]}
         }
      newcutpoint <- c(newcutpoint, cutpoint)
      #Effort <- c(Effort, tempdf$Effort)
      utils::setTxtProgressBar(progress_bar, value = j)
   }
   #df$Effort <- Effort[-1]
   df$cutpoint <- newcutpoint[-1]
   if(sum(newcutpoint=="center") < length(uniqueseg)){warning("fewer centerpoints than segments. some segments don't have centers!")}
   if(sum(newcutpoint=="center") > length(uniqueseg)){warning("more centerpoints than segments. some segments have multiple centers!")}
   df
}









