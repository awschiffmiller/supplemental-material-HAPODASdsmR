#' converts raw HAPODAS dataframe to DistData data for DSM and detection function
#'
#' converts raw HAPODAS dataframe to distance data: Holds detection data including ID, species, distance, covariates for detection function
#'
#' @param data a raw HAPODAS dataset, column names unaltered.
#' @param covariates a list of covariate column names
#'
#' @return Returns a Dataframe or SpatialPointsDataFrame (if makeSP=T) with one row per sighting with specified columns:
#'
#' datetime: POSIXct class, timestamp of datapoint, links to obsdata
#'
#' lats: numeric class, decimal latitude of datapoint, links to obsdata
#'
#' longs: numeric class, decimal longitude of datapoint, links to obsdata
#'
#' label: factor class, unique line label for transect, links to obsdata
#'
#' distance *** : REQUIRED for package `Distance`: the perpendicular distance from transect line to the detection (from HAPODAS$perpDist_km)
#'
#' object *** : REQUIRED for package `Distance`: unique object identifier (from HAPODAS$SIGHTNUMBER), links to obsdata
#'
#' size *** : REQUIRED for package `Distance`: group size
#'
#' species: SPECIESCODE
#'
#' detected: *required for mrds* 1 if detected by the observer and 0 if missed **(always 1 for single observer)**
#'
#' observer : *required for mrds* observer number (1 or 2) **(always 1 for single observer)**
#'
#' other factor covariates used for detection function
#'
#' x: (only after running `makeSP`) numeric class, the UTM (eastings) reprojection of longitude, default CRS is Alaska Equal Area Albers
#'
#' y: (only after running `makeSP`) numeric class, the UTM (northings) reprojection of latitude, default CRS is Alaska Equal Area Albers
#'
#' '***' indicates a required item for this dataframe for `dsm` or `Distance`
#'
#' @author Abigail Schiffmiller
#' @export
#'
#' @example
#'  see 'makedistdata.R' file
#'



Distdata <- function(data, covariates){
   data <- data
      distance <- (data$perpDist_km)
      object <- (data$SIGHTNUMBER)
      size <- (data$BESTGSIZ)
      species <- as.factor(data$SPECIESCODE)
   all_covs <- data.frame(covariates, stringsAsFactors = T)
   all_covs <- lapply(all_covs, factor)
   newdat <- data.frame(datetime=as.POSIXct(data$date_time, tz=""), longs=data$DLONG,
                        lats=data$DLAT, label=as.factor(data$Line_Label),
                        object=as.integer(object), species, distance, size,
                        detected = as.integer(rep(1, times=length(object))),
                        observer = as.integer(rep(1, times=length(object))),all_covs)
   newdat <- as.data.frame(newdat, stringsAsFactors=T)
   newdat <- newdat[which(newdat$object != "NA"),]
   distSP <- makeSP(newdat, coordx=newdat$longs, coordy = newdat$lats)
   distdata <- data.frame(distSP)
   distdata
}
