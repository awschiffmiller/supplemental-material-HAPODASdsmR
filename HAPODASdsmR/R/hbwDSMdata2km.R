
#' Data for HBW DSM vignette, 2km segments
#' data constucted in file: "~/UAF/MS/data and analysis/HAPODAS/dsmdata_species.Rmd"
#'
#'  usethis::use_data(hbw_Dist, hbw_Obs, hapodas_Segdata, hbw_hr_cds.05, hbw_hr_mcds_CUE_SIGHTOBS, overwrite=TRUE)
#'
#'
#'
#' HBW Distdata
#' made with `Distdata` function. Only Humpback whale sightings.
#'
'hbw_Dist'

#' HBW Obsdata
#' made with `Obsdata` function. Only Humpback whale sightings.
#'
'hbw_Obs'

#' hapodas Segdata
#' made with `Segdata` function. All hapodas segments
#'
'hapodas_Segdata'

#'
#'
#' HBW ddf.obj: an CDS detection function for humpback whales with:
#'  hazard rate key function
#'  the most distant 5% of sighings truncated
#'
#'
#' hist(hbw_Dist$distance, breaks= 15)
#' hbw_hr_cds.05<-Distance::ds(hbw_Dist, truncation="5%", key='hr',
#'                                        adjustment=NULL, formula= ~1)
#' summary(hbw_hr_cds.05)
#' plot(hbw_hr_cds.05, main= "Humpback whale: CDS Hazard Rate")
#'
'hbw_hr_cds.05'


#' HBW ddf.obj: an MCDS detection function for humpback whales with:
#'  hazard rate key function
#'  sighting cue (`CUE`) covariate
#'  sighting observer (`SIGHTOBS`) covariate
#'  the most distant 5% of sighings truncated
#'
#'
#' hist(hbw_Dist$distance, breaks= 15)
#' hbw_hr_mcds_CUE_SIGHTOBS<-Distance::ds(hbw_Dist, truncation="5%", key='hr',
#'                                        adjustment=NULL, formula= ~CUE+SIGHTOBS)
#' summary(hbw_hr_mcds_CUE_SIGHTOBS)
#' plot(hbw_hr_mcds_CUE_SIGHTOBS, main= "Humpback whale: MCDS Hazard Rate+ CUE + SIGHTOBServer")
#'
'hbw_hr_mcds_CUE_SIGHTOBS'

