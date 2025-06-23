#' Converts from 'segmentized' to 'segdata' dataframes
#' takes segmentized dataset with all start and end points and original datapoints and gives "segdata" data set to be used by 'dsm'
#'
#' @param data a dataframe as produced by SplitTransects::segmentize
#'
#' @returns a Dataframe or SpatialPointsDataFrame (if makeSP=T) with one row per segment with following columns:
#'
#' datetime: POSIXct class timestamp of datapoint
#'
#' longitude: numeric class, the 'center' longitude of the segment (unless 'start','endpoint' or another point is specified) (from longs)
#'
#' latitude: numeric class, the 'center' latitude of the segment (unless 'start','endpoint' or another point is specified) (from lats)
#'
#' Effort: numeric class, length in meters of the segment (from mod)
#'
#' Transect.Label: character or factor class, unique line label for transect (from label)
#'
#' Sample.Label: character or factor class, unique segment label (combination of label and segnum)
#'
#' x: (only after running `makeSP`) numeric class, the UTM (eastings) reprojection of longitude, default CRS is Alaska Equal Area Albers
#'
#' y: (only after running `makeSP`) numeric class, the UTM (northings) reprojection of latitude, default CRS is Alaska Equal Area Albers
#'
#'
#'  @author Abigail Schiffmiller
#' @export
#'
#' @example
      #' micro_interp <- interp_by_secs(micro)
      #' micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
      #'                                          grpVarLst = list (micro_interp$label,
      #'                                          micro_interp$onoff, micro_interp$section))
      #' micro_segs <- seg_assign(data=micro_interp, seg.length = 75)
      #' micro_segs <- seg_num(micro_segs)
      #' segments_micro <- segmentize(micro_segs)
#'
#'
#' micro_segdata <- dsm_segdata(segments_micro)
#'



Segdata <- function(data){
    df <- data
        df$Sample.Label <- paste(df$sec_label,df$segnum, sep=".")

        single_row <- df[(df$cutpoint == 'center'),]
        single_row$longitude <- single_row$longs
        single_row$latitude <- single_row$lats

    segdata <- data.frame(datetime=as.POSIXct(single_row$datetime, tz=""),
                          longitude=single_row$longs,
                          latitude=single_row$lats, Effort = single_row$mod,
                          Transect.Label = single_row$label, Sample.Label=as.factor(single_row$Sample.Label))
    segSP <- makeSP(segdata, coordx=segdata$longitude, coordy = segdata$latitude)
    segdata <- data.frame(segSP)
    segdata
}


