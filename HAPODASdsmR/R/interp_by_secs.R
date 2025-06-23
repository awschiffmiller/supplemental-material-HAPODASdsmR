#' interpolate location for every second along transect
#'
#' interpolate location for every second along transect
#' and keep the transect metadata/identifying
#' info for new rows, for line transect dataset.
#'
#' @param dataset a dataframe with specified columns:
#'
#' datetime: POSIXct class, date and time of datapoint
#'
#' lats: numeric class, decimal latitude of datapoint
#'
#' longs: numeric class, decimal longitude of datapoint
#'
#' label: character or factor class, unique line label for transect
#'
#' onoff: character or factor class, on or off effort designation for line
#'
#' section: character or factor class, section of line
#'
#' travel: numeric class, distance traveled since last data point IN METERS
#'
#' tdiff: tdiff class, time difference in seconds from previous point
#'
#' orriginal: Logical class, designates row as an orriginal datapoint

#' @return Returns a dataframe with columns:
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
#' orriginal: Logical class, designates row as an orriginal datapoint
#'
#' @author Abigail Schiffmiller
#' @export
#'
#'
#' @examples
#' require(tictoc)
#' tic()
#' micro_interp  <- interp_by_secs(micro)
#' toc()
#' head(micro_interp)
#'

interp_by_secs <- function(dataset){

   length <- nrow(dataset)
      tdiff <- as.numeric(dataset$tdiff)
      datetime <- as.POSIXct(dataset$datetime, tz="")
      datetime <- lubridate::force_tz(datetime,tzone = "America/Anchorage")
      lats <- dataset$lats
      longs <- dataset$longs
      label <- as.character(dataset$label)
      onoff <- as.character(dataset$onoff)
      section <- as.character(dataset$section)
      travel <- dataset$travel
      orriginal <- dataset$orriginal

   if(tdiff[1]!= 0){
         foo_tdiff <- rep(tdiff[1], length.out=tdiff[1])
         foo_datetime <- c(datetime[1], datetime[1]+1:(tdiff[1]-1))
         foo_lats <- stats::approx(lats[1:2], method = "linear", n = tdiff[1])$y
         foo_longs <- stats::approx(longs[1:2], method = "linear", n = tdiff[1])$y
         foo_label <- rep(label[1],length.out=tdiff[1])
         foo_onoff <- rep(onoff[1], length.out=tdiff[1])
         foo_section <- rep(section[1], length.out=tdiff[1])
         foo_travel <- rep((travel[(1+1)]/tdiff[1]), length.out=tdiff[1])
         foo_orriginal <- c(orriginal[1], rep("FALSE",length.out=(tdiff[1]-1)))
                   }
   if(tdiff[1] == 0){
         foo_tdiff <- 0
         foo_datetime <- datetime[1]
         foo_lats <- stats::approx(lats[1:2], method = "linear", n = tdiff[1]+1)$y
         foo_longs <- stats::approx(longs[1:2], method = "linear", n = tdiff[1]+1)$y
         foo_label <- rep(label[1],length.out=tdiff[1]+1)
         foo_onoff <- rep(onoff[1], length.out=tdiff[1]+1)
         foo_section <- rep(section[1], length.out=tdiff[1]+1)
         foo_travel <- 0
         foo_orriginal <- (orriginal[1])
                   }

      print("interpolating a point every second:")
      progress_bar = utils::txtProgressBar(min=0, max=length(length), style = 3, char="=")

   for(i in 2:(length-1)){
      diffs <- tdiff[i]
         if(diffs > 0){
         foo_datetime <- c(foo_datetime, datetime[i]+0:(tdiff[i]-1))
         foo_lats  <-  c(foo_lats, stats::approx(lats[i:(i+1)], method = "linear", n = tdiff[i])$y)
         foo_longs  <-  c(foo_longs, stats::approx(longs[i:(i+1)], method = "linear", n = tdiff[i])$y)
         foo_label <- c(foo_label, rep(label[i],length.out=tdiff[i]))
         foo_onoff <- c(foo_onoff, rep((onoff[i]), length.out=tdiff[i]))
         foo_section <- c(foo_section, rep((section[i]),length.out=tdiff[i]))
         foo_travel <- c(foo_travel, rep((travel[(i+1)]/tdiff[i]), length.out=tdiff[i]))
         foo_orriginal <- c(foo_orriginal, orriginal[i], rep("FALSE",length.out=(tdiff[i]-1)))
             }
         if(diffs==0){
         foo_datetime <- c(foo_datetime, datetime[i])
         foo_lats  <-  c(foo_lats, lats[i])
         foo_longs  <-  c(foo_longs, longs[i])
         foo_label <- c(foo_label, label[i])
         foo_onoff <- c(foo_onoff, onoff[i])
         foo_section <- c(foo_section, section[i])
         foo_travel <- c(foo_travel, travel[i+1])
         foo_orriginal <- c(foo_orriginal, orriginal[i])
         }
      utils::setTxtProgressBar(progress_bar, value = i)
   }
         foo_datetime <- c(foo_datetime, datetime[length])
         foo_lats  <-  c(foo_lats, lats[length])
         foo_longs  <-  c(foo_longs, longs[length])
         foo_label <- c(foo_label, label[length])
         foo_onoff <- c(foo_onoff, onoff[length])
         foo_section <- c(foo_section, section[length])
         foo_travel <- c(foo_travel, travel[length])
         foo_orriginal <- c(foo_orriginal, orriginal[length])

         interpolated <- data.frame(datetime=as.POSIXct(foo_datetime, tz=""), lats=foo_lats, longs=foo_longs,
                            label=as.factor(foo_label), onoff=as.factor(foo_onoff),
                            section=foo_section, travel=foo_travel, orriginal=as.logical(foo_orriginal))


return(interpolated)
 }
