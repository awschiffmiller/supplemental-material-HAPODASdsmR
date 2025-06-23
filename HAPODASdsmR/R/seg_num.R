#' Unique ID for each segment
#'
#' Uniquely identifies each segment (in combination with label, onoff, and  section designations).
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
#' orriginal: Logical class, designates row as an orriginal datapoint "True" or interpolated point "FALSE"
#'
#' cum_dist: numeric class, from `cumsum_by_group`, cumulative distance in meteres for each unique label/onoff/section combination
#'
#' mod: numeric class, the modulus of cum_dist by seg.length
#'
#' cutpoint: Character class: identifies "Start" and "End" points of each segment. Points inside a segment are designated "0"
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
#' View(micro_segs)
#'
#'
seg_num <- function(data){
   #df <- data
   df <- data[(which(data$onoff ==" ON"|data$onoff =="ON")),]
   segnum<-1

   print("numbering segments:")
   progress_bar = utils::txtProgressBar(min=0, max=length(df$cutpoint), style = 3, char="=")

    for(i in seq_along(df$cutpoint)){
       cutpoint<-df$cutpoint[i]
       if(cutpoint=='Start'){
          segnum<-c(segnum,(segnum[i-1]+1))
       }
       if(cutpoint!= 'Start'){
       segnum<- c(segnum, segnum[i-1])
       }
    utils::setTxtProgressBar(progress_bar, value = i)
    }
   df$segnum <- as.integer(segnum)

   df
}

