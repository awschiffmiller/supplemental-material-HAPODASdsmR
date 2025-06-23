#' makes an 'observation' dataframe
#'
#' makes an 'observation' dataframe which links segment data with distance data
#'
#' @param Dist_data Distdata dataframe from `Distdata`
#' @param Seg_data Segdata dataframe from `dsm_segdata`
#'
#' @return Returns a Dataframe or SpatialPointsDataFrame (if makeSP=T) with one row per sighting with specified columns:
#'
#' datetime: POSIXct class, timestamp of datapoint (FROM Dist_data)
#'
#' longs: numeric class, decimal longitude of datapoint (FROM Dist_data)
#'
#' lats: numeric class, decimal latitude of datapoint (FROM Dist_data)
#'
#' label: factor class, unique line label for transect (FROM Dist_data)
#'
#' distance: the perpendicular distance from transect line to the detection (FROM Dist_data)
#'
#' object: unique object identifier (FROM Dist_data)
#'
#' size: group size (FROM Dist_data)
#'
#' Effort: numeric class, length in meters of the segment (FROM Seg_data)
#'
#' Sample.Label: character or factor class, unique segment label (FROM Seg_data)
#'
#' x: (only after running `makeSP`) numeric class, the UTM (eastings) reprojection of longitude, default CRS is Alaska Equal Area Albers
#'
#' y: (only after running `makeSP`) numeric class, the UTM (northings) reprojection of latitude, default CRS is Alaska Equal Area Albers
#'
#'
#' @author Abigail Schiffmiller
#' @export
#'
#' @example
#'  see 'makedistdata.R' file
#'

Obsdata <- function(Dist_data, Seg_data){
   distdata <- Dist_data
   segdata <- Seg_data
   obs1 <- merge(distdata,segdata,
                 by.x = c("datetime", "longs", "lats", "label", "x", "y"),
                 by.y = c("datetime", "longitude", "latitude", "Transect.Label", "x", "y"),
                 all=TRUE)
   for(i in seq_along(obs1$Sample.Label)){
      if(is.na(obs1$Sample.Label[i]) == T){
         obs1$Sample.Label[i] <- obs1$Sample.Label[i-1]
         obs1$Effort[i] <- obs1$Effort[i-1]
      }
   }
   obs1 <- data.frame(datetime=as.POSIXct(obs1$datetime, tz=""),
                      longs=obs1$longs, lats= obs1$lats,
                      label=as.factor(obs1$label), distance = obs1$distance,
                      object=as.integer(obs1$object), size = obs1$size,
                      Effort=obs1$Effort, Sample.Label = as.factor(obs1$Sample.Label))
   obstrue <- stats::complete.cases(obs1)
   obsonly <- obs1[obstrue,]

   obsonly
}
