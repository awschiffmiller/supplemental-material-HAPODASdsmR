#' turns a dataframe into SP class
#'
#' turns a dataframe into SP class. To be used with `HAPODASdsmR` functions: `dsm_segdata`, `Distdata`, and `Obsdata` for DSM analysis
#'
#' @param dataset a dataframe to be converted to an SP class
#' @param coordx the variable of the dataframe to be used as the `x` coordinate
#' @param coordy the variable of the dataframe to be used as the `y` coordinate
#' @param proj the projection to use for the SPDF. Default is akalbers (Alaska Equal Area Albers)
#'
#' #' @return Returns a SpatialPointsDataFrame version of the input dataset with coordinates defined as `x` and `y`
#'
#'

makeSP <- function(dataset, coordx, coordy, proj = akalbers){
   dataset <- dataset
   dataset$x <- coordx
   dataset$y <- coordy
   proj <- proj
   newdat <- dataset
   sp::coordinates(newdat)<-c("x", "y")
   sp:: proj4string(newdat) <- "+proj=longlat +datum=WGS84"
   newdat <- sp::spTransform(newdat, CRSobj = proj)
   newdat
}


