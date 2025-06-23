#' converts raw HAPODAS dataframe to work with SplitTransects functions
#'
#' converts raw HAPODAS dataframe to work with SplitTransects functions
#'
#' @param data a raw HAPODAS dataset, column names unaltered.
#'
#' @return Returns a dataframe with specified columns:
#'
#' datetime: POSIXct class, date and time of datapoint
#'
#' lats: numeric class, decimal latitude of datapoint
#'
#' longs: numeric class, decimal longitude of datapoint
#'
#' label: factor class, unique line label for transect
#'
#' onoff: factor class, on or off effort designation for line
#'
#' section: factor class, section of line
#'
#' travel: numeric class, distance traveled since last data point IN METERS, converted from Nautical miles by multiplying by 1852 (m/nm)
#'
#' tdiff: numeric class, time difference in seconds to next point
#'
#' orriginal: Logical class, designates row as an orriginal datapoint


#'
#' @author Abigail Schiffmiller
#'
#' @export
#'
#'

#'
convertHAPODAS <- function(data){
   data <- data
   #data <- data[which(data$ONOFF==" ON"),]

   orriginal <- rep("TRUE", length.out=nrow(data))
   datetime <- as.POSIXct(data$date_time, tz="")
   tdiff <- c(diff(datetime),0)#number of seconds between this timestamp and the next one
   tdiff[tdiff < 0]<-0
   tdiff[tdiff > 3600]<-0
   newdat <- data.frame(datetime=as.POSIXct(data$date_time, tz=""), lats=data$DLAT,
              longs=data$DLONG, label=as.factor(data$Line_Label),
              onoff=as.factor(data$ONOFF),
              section=as.factor(data$Segment),
              travel=(1852*data$matlabOnNotYesE), tdiff= as.numeric(tdiff),
              orriginal=as.logical(orriginal))
   newdat
}

