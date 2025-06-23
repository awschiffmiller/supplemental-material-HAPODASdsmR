#' making micro_distdata and short_distdata
#'
#'  micro: 1 obs. of 13 variables
#'  short: 9 obs. of 13 variables
#'
#' A dataframe of the necessary columns to do detection functions ising package `Distance`
#'
#' @format A dataframe with 200 obs. of  12 variables:
#' \describe{
#'   \item{datetime}{POSIXct, format: "%Y-%m-%d %H:%M:%S" tz=AKDT}
#'   \item{lats}{num: Decimal degree latitude}
#'   \item{longs}{num: Decimal degree longitude}
#'   \item{label}{Factor: Unique transect line label}
#'   \item{distance}{num: REQUIRED for package `Distance`: the perpendicular distance from transect line to the detection}
#'   \item{object}{int: REQUIRED for package `Distance`: unique object identifier}
#'   \item{size}{int: REQUIRED for package `Distance`: group size}
#'   \item{species}{Factor: species code, see 'DSMdatatables vignette for names'species_names.rda' for names}
#'   \item{detected}{integer: *required for mrds* 1 if detected by the observer and 0 if missed **(always 1 for single observer)**}
#'   \item{observer}{integer: *required for mrds* 1 if detected by the observer and 0 if missed **(always 1 for single observer)**}
#'   \item{covariates}{Factor: list of factor covariates to use in MCDS}
#' }
#'
#'
#'  use this ad base hapodas data:
#'         hapodas <- hapodas <- read.csv("~/UAF/MS/data and analysis/HAPODAS/HAPODAS DATA/HAPODAS_1_19_21.csv")
#'


#'
#' micro_hapodas <- hapodas[547:556,]
#'
#' micro_distdata <- Distdata(data=micro_hapodas, covariates = list(CUE=micro_hapodas$CUE,
#'                                                                 SIGHTOBS=micro_hapodas$SIGHTOBS,
#'                                                                 Visibility=micro_hapodas$Visibility,
#'                                                                 BEAUFORT=micro_hapodas$BEAUFORT,
#'                                                                 FOGRAIN=micro_hapodas$FOGRAIN))
#'
#' usethis::use_data(micro_distdata)
#'
"micro_distdata"



#' #'
#' short_hapodas <- hapodas[600:749,]
#'
#' short_distdata <- Distdata(data=short_hapodas, covariates = list(CUE=short_hapodas$CUE,
#'                                                                  SIGHTOBS=short_hapodas$SIGHTOBS,
#'                                                                  Visibility=short_hapodas$Visibility,
#'                                                                  BEAUFORT=short_hapodas$BEAUFORT,
#'                                                                  FOGRAIN=short_hapodas$FOGRAIN))
#'
#' usethis::use_data(short_distdata)


"short_distdata"



