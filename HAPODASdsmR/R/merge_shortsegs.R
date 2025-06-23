#' combine short end segmnets to previous
#'
#' Reassigns 'segnum' to match previous segment if max segment length is less than specified
#'  AND previous shares same label and section value
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

#' @param cutoff segments shorther than this length will be merged with the provious segment id it has the same line and section number. Defoult value is 1000m
#'
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
#' Effort: length in meteres of segment
#'
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


merge_shortsegs <- function(data, cutoff = 1000){
   df <- data
   cutoff <- cutoff
   dfmod <- df$mod
   dfsegnum <- df$segnum
   df$sec_label <- paste(df$label, df$section, sep = "_")
   uniquelabel <- unique(df$sec_label)
   newsegnum <- 0

   df$seg_length <- stats::ave(dfmod,
                            dfsegnum,
                             FUN = max)

   print("all transects:")
   progress_bar = utils::txtProgressBar(min=0, max=length(uniquelabel), style = 3, char="=")

   for(j in 1:length(uniquelabel)){
      tempdf <- subset(df, sec_label == uniquelabel[j])
      mod <- tempdf$mod
      segnum <- tempdf$segnum
         if(tempdf$seg_length[1] >= cutoff){

                  #print("each transect:")
                  #progress_bar = utils::txtProgressBar(min=0, max=length(segnum), style = 3, char="=")
            for(i in seq_along(segnum)){
               if(tempdf$seg_length[i] < cutoff){
               segnum [i]<-(segnum[i]-1)
               }
               else{segnum [i]<-segnum[i]}
                  #utils::setTxtProgressBar(progress_bar, value = i)
            }
         tempdf$segnum <- as.integer(segnum)
         }
      newsegnum <- c(newsegnum, tempdf$segnum)
      utils::setTxtProgressBar(progress_bar, value = j)
   }
   df$segnum <- newsegnum[-1]

   unique_segnum <- unique(df$segnum)
   Effort <- 0
   for(k in 1: length(unique_segnum)){
      temp <- subset(df,segnum == unique_segnum[k] )
      Effort <- c(Effort,rep((sum(unique(temp$seg_length))),length.out = nrow(temp)))
   }

   df$Effort <- Effort[-1]
   df <- df[,c(1:13,15)]
}









