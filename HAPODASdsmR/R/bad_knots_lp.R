#' find all the bad knots with DSM
#'
#' @param knots dataframe of knots used in soap_check
#' @param k k to be used in DSM
#'
#' @return dataframe of knots without the ones identified as "on or outside bounds" by dsm
#'

bad_knots_lp <- function(knots, dsmk, nmax=200){
    knotsedt <- knots
   K <<- dsmk ## dsm looking outside the function environment, this creates K outside for dsm to use
   Nmax <<- nmax
   repeat{
      errmssg <- try({dsm_trial <- dsm(abundance.est~s(x, y, bs="so", k = K,
                                                       xt=list(bnd=surveyArea, nmax=Nmax)),
                                       ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS, segment.data = hapodas_Segdata,
                                       observation.data=hbw_Obs_t, knots=knotsedt,##
                                       family=tw(), method="REML")})
      errtst <- grepl("Error in crunch.knots", errmssg)

      if(errtst == TRUE){
         badknot <- as.integer(regmatches(errmssg, regexpr("\\s[0-9]+\\s", errmssg)))
         knotsedt <- knotsedt[-badknot,]
      } else {break}
   }

   knotsedt
}

