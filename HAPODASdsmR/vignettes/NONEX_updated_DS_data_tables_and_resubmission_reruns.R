# ---
#   title: "resubmission reruns"
# author: "Abigail Schiffmiller"
# date: "2026-05-20"
# output: html_document
# ---
  # libraries ####
  #
# library(Distance)
# library(dsm)
# library(ggplot2)
# library(knitr)
# library(tweedie)
# library(mgcv)
# library(plyr)
# library(dplyr)
# library(terra)
# library(sf)

req_packs <- c("Distance",
               "dsm",
               "ggplot2",
               #"knitr",
               "tweedie",
               "mgcv",
               "plyr",
               "dplyr",
               "terra",
               "sf",
               "tictoc",
               "mgcViz")

missing_packs <- req_packs[!(req_packs %in% installed.packages()[, "Package"])]

if (length(missing_packs) > 0) {
  install.packages(missing_packs, dependencies = TRUE)
}

lapply(req_packs, library, character.only = TRUE)
library(sf)






# functions ####
#
summarize_dsm <- function(model, predvar, model_name){
  summ <- summary(model)
  k_vals <- tryCatch(sapply(model$smooth, function(x) x$bs.dim),error = function(e) NA)
  nmax_vals <- tryCatch(sapply(model$smooth, function(x) x$xt$nmax),error = function(e) NA)
  pv_sum <- summary(predvar)
  sigma <- sqrt(log(1 + pv_sum$cv^2))
  data.frame(response = model$family$family,
             terms    = paste(rownames(summ$s.table), collapse=", "),
             k        = paste(k_vals, collapse=", "),
             smooth.param = paste(round(model$sp, digits=2), collapse=", "),
             #nmax     = paste(nmax_vals, collapse = ", "),
             bnd.vert.ret = paste (model$smooth[[1]]$sd$nb, collapse=","),
             AIC      = AIC(model),
             edf      = sum(summ[["edf"]]),
             ref.df   = (sum(summary(model)$s.table[, "Ref.df"])),
             EDFpct   = 100 * (sum(summ[["edf"]]))/(sum(summary(model)$s.table[, "Ref.df"])),
             DevExp   = (summ[["dev.expl"]]*100),
             n        = (summ[["n"]]),
             deviance = model[["deviance"]],
             #pvalue  = summ[["p.pv"]][["(Intercept)"]],
             rank     = model$rank,
             abundance  = pv_sum$pred.est,
             lcl95      = pv_sum$pred.est * exp(-1.96 * sigma),
             ucl95      = pv_sum$pred.est * exp( 1.96 * sigma),
             cv.det     = pv_sum$detfct.cv,
             cv.gam     = pv_sum$gam.cv,
             cv.total   = pv_sum$cv,
             se.total   = pv_sum$se
  )
}


save_model <- function(model_object) {
  # Extract the variable name dynamically
  obj_name <- deparse(substitute(model_object))
  # Save to .rds file with the same name
  saveRDS(model_object, file = paste0(output.loc,obj_name, ".rds"))
}

###



#
data.loc <- "~/UAF/MS/GPTedits_supplemental-material-HAPODASdsmR/HAPODASdsmR/data/data/"  # data location

# data.loc <- "~/HAPODAS/data/"
# script.loc <- "~/HAPODAS/scripts/"
# output.loc <- "~/HAPODAS/output/"
###

# load all formated data ####
#
## areas and area boundries ####
##

# inner soapfilm boundry - prediction area
load(file= paste0(data.loc,"inner_soapBnd.Rdata"))  # sf format, 9 polygons

# soap film surface
load(file= paste0(data.loc,"soap_surface.Rdata"))  # list, 9 polygons - same as inner_soapBnd

# Soap Knots (5km spacing)
load(file= paste0(data.loc,"knots5kmgrid.Rdata"))  # knots within inner_soapBnd & on soap_surface

# buffered soapfilm boundry - model area
load(file= paste0(data.loc,"buff_soapBnd_0NA.Rdata")) # inner_soapBnd buffered outward 3.75km to accomodate all knots

# Prediction Grid- 2x2km grid with all covariates
load(file= paste0(data.loc,"km2Predgrid.Rdata"))  # to buffer edge
load(file= paste0(data.loc,"km2Predgrid_inner.Rdata"))  # to inner boundary edge

# unsimplified survey area

## A&B strata area

## all strata area

#

## DSM data (seg, obs, dist)
#DSM data tables for 2km segments
##
# Observation data
load(file= paste0(data.loc,"hbw_Obs.Rdata")) # sf format w/ x&y for soapfilm compatibility

# Segment data (with covariates)
load(file= paste0(data.loc,"Segdat_covs.Rdata")) # sf format w/ x&y for soapfilm compatibility

# distance data
load(file= paste0(data.loc,"hbw_Dist.Rdata")) # sf format w/ x&y for soapfilm compatibility

# ***************************

## load detection functions

load(file= paste0(data.loc,"hbw_hr_cds.05.rda")) # HBW detection funciton cds
#load(file= paste0(data.loc,"hbw_hr_mcds_CUE_SIGHTOBS.rda"))
load(file= paste0(data.loc,"hbw.hr.mcds.CUE_SIGHTOBS.rda")) # full HBW detection function mcds
# ***************************

## DS data
load(file= paste0(data.loc,"strataAreas.Rdata")) # strata labels and area of each
load(file= paste0(data.loc,"RelizedEffort.Rdata")) # actually sampled track and transit lines
load(file= paste0(data.loc,"full_HAPODAS.Rdata")) # full hapodas data file- all lines and observations

# ***************************

## Modify data as needed: ####
### truncate observation data to detection function width
##
hbw_Obs_t <- hbw_Obs[hbw_Obs$distance <= hbw_hr_mcds_CUE_SIGHTOBS$ddf$meta.data$width, ]  ## 395 -> 375
#

#remove data/segments outside soap
##
# remove segments outside soapfilm  ## 1670 ->
onoff <- st_within(Segdat_covs,
                   st_union(inner_soapBnd),sparse = FALSE)[,1]
Segdat_covs_soap <- Segdat_covs[onoff,]

# remove observations outside soapfilm  ## 375 -> 373
onoff <- st_within( hbw_Obs_t,
                    st_union(inner_soapBnd),sparse = FALSE)[,1]
hbw_Obs_t_soap <- hbw_Obs_t[onoff,]

###


#load all saved objects ####
#
file_list <- list.files(
  path = output.loc,
  pattern = "\\.rds$",
  full.names = TRUE
)

for(f in file_list) {
  obj_name <- tools::file_path_sans_ext(basename(f))
  assign(obj_name, readRDS(f))
}
###


# dbDS ####


## DS data tables ####


#
hapodas <- data.frame(date_time = full_hapodas$date_time
                      ,dlat = full_hapodas$DLAT
                      ,dlong = full_hapodas$DLONG
                      ,Effort = ((1852*full_hapodas$matlabOnNotYesE)/1000) #convert nautice miles to km
                      ,onoff = full_hapodas$ONOFF
                      ,Sample.Label = paste(as.character(full_hapodas$Line_Label), full_hapodas$Segment, sep="")
                      ,EType = full_hapodas$EffortType
                      ,Stratum = as.character(full_hapodas$Stratum_Label)
                      ,StratumArea = full_hapodas$StratumArea
                      ,SIGHTOBS = full_hapodas$SIGHTOBS
                      ,CUE = full_hapodas$CUE
                      ,Species = as.character(full_hapodas$SPECIESCODE)  # hbw = 76
                      ,distance = full_hapodas$perpDist_km # perpenducular distance
                      ,size = full_hapodas$BESTGSIZ
                      ,object = full_hapodas$SIGHTNUMBER
                      ,detected = rep(1,nrow(full_hapodas))
                      ,observer = rep(1,nrow(full_hapodas))
)


das <- hapodas[,-c(1:3,10:17)]

OnEff <- das[das$onoff != "OFF",]

Trackdas <- OnEff[OnEff$EType=="Trackline",] # only trackline, not transit


samp <- Trackdas %>%
  group_by(Sample.Label) %>%
  summarise(
    Region.Label = first(Stratum),
    Area = first(StratumArea),
    Etype = first(EType),
    Effort = sum(Effort, na.rm = TRUE),
    .groups='drop')

# # replace samp$Etype with EffortType from RelizedEffort by matching Sample.Label and remove transits
# samp$Etype <- RelizedEffort$EffortType[
#   match(samp$Sample.Label, RelizedEffort$Sample.Label)
# ]
#
# samp <- samp[which(samp$Etype=="Trackline"),]

###

### Region tables ####
#

#sampled strata only, no unsampled inlets

reg <- samp %>%
  group_by(Region.Label) %>%
  summarise(
    Area = first(Area),
    .groups='drop')

Strata <- strataAreas[, c(2:4,10)]
Strata$DIST_ID <- as.character(Strata$DIST_ID)

reg_join <- full_join(
  reg, Strata, by = c("Region.Label" = "DIST_ID"))

reg.tab <- reg_join %>%
  filter(channel %in% c("Inlet", "MBW"))

region.table_Sstrata <- reg.tab[,c(1,3,5)]

region.table_Sstrata <- region.table_Sstrata %>%
  rename(Area = Area_sqkm)

sum(region.table_Sstrata$Area)  # 21622.94

## build region table- MBW v sampled inlets ####

region.table_MBWvSinlets <- region.table_Sstrata %>%
  group_by(channel) %>%
  summarise(Area = sum(Area, na.rm= TRUE))

region.table_MBWvSinlets <- region.table_MBWvSinlets %>%
  rename(Region.Label = channel)

sum(region.table_MBWvSinlets$Area)  # 21622.94
###


### Sample tables ####
#
## build sample table- sampled strata  ####


sample.table_Sstrata <- samp %>%
  left_join(
    region.table_Sstrata %>%
      select(Region.Label, channel),
    by = "Region.Label"
  ) %>%
  distinct(Sample.Label, .keep_all = TRUE)

sample.table_Sstrata <- sample.table_Sstrata[,-3]   #remove area


## build sample table- MBW v sampled inlets ####

sample.table_MBWvSinlets <- sample.table_Sstrata[,-2]

sample.table_MBWvSinlets <- sample.table_MBWvSinlets %>%
  rename(Region.Label = channel)

sample.table_MBWvSinlets <- sample.table_MBWvSinlets %>%
  left_join(region.table_MBWvSinlets, by = "Region.Label")
###


### Observation tables ####
#
## build obs.tables

hbw_df <- hapodas[which(hapodas$Species =="76"),]

### remove sightings by observers 999 and 907 (made by off-duy observers)
# 414 -> 397
hbw_df <- hbw_df %>%
  dplyr::filter(!SIGHTOBS %in% c(907, 999))

### remove off-effort sightings
#  397 -> 395
hbw_df <- hbw_df %>%
  filter(onoff != "OFF")

### remove "Transit" observations
# 395 -> 263
obs <- hbw_df %>%
  filter(EType != "Transit")


## BUILD!!!


obs <- data.frame(object = obs$object, Sample.Label = obs$Sample.Label)

obs.table_Sstrata <- obs %>%
  left_join(
    sample.table_Sstrata %>%
      select(Sample.Label, Region.Label),
    by = "Sample.Label"
  )


obs.table_MBWvSinlets <- obs %>%
  left_join(
    sample.table_MBWvSinlets %>%
      select(Sample.Label, Region.Label),
    by = "Sample.Label"
  )
###

### tables for no statification ####
#
region.table_allasone <- data.frame(Region.Label = "whole", Area = sum(region.table_Sstrata$Area) )

sample.table_allasone <- sample.table_MBWvSinlets %>%
  mutate(Region.Label = "whole") %>%
  select(-Area)# %>%


obs.table_allasone <- obs.table_MBWvSinlets %>%
  mutate(Region.Label = "whole")

# tables for soap prediction area ####
region.table_soap <- data.frame(Region.Label = "soap", Area = 23303.86)

sample.table_soap <- sample.table_MBWvSinlets %>%
  mutate(Region.Label = "soap") %>%
  select(-Area)


obs.table_soap <- obs.table_MBWvSinlets %>%
  mutate(Region.Label = "soap")


###

## DDF

#
# loaded at top

# hbw_hr_mcds_CUE_SIGHTOBS<- ds(hbw_df, truncation="5%", key='hr', adjustment=NULL,
#               transect="line",
#               formula= ~CUE+SIGHTOBS)
###



## NEW dbDS : Run DS for abundance!  ####

### all Strata ####
#

Nhat_Sstrata_ds <- Distance::ds(hbw_df, truncation="5%", key='hr', adjustment=NULL,
                                transect="line",
                                formula= ~CUE+SIGHTOBS,
                                region_table = region.table_Sstrata,#sampled MBW strata and sampled Inlet strata, all separate
                                sample_table = sample.table_Sstrata,
                                obs_table = obs.table_Sstrata,
                                er_var = "R2")

Nhat_Sstrata_summary <- summary(Nhat_Sstrata_ds)
# Nhat_Sstrata_summary[["dht"]][["individuals"]][["N"]]
# (Nhat_Sstrata <- Nhat_Sstrata_summary$dht$individuals$N$Estimate[
#   Nhat_Sstrata_summary$dht$individuals$N$Label == "Total"])
# (ucl_Sstrata <- Nhat_Sstrata_summary$dht$individuals$N$ucl[
#   Nhat_Sstrata_summary$dht$individuals$N$Label == "Total"])
# (lcl_Sstrata <- Nhat_Sstrata_summary$dht$individuals$N$lcl[
#   Nhat_Sstrata_summary$dht$individuals$N$Label == "Total"])
Nhat_Sstrata_summary[["dht"]][["individuals"]][["N"]][40,]
Nhat_Sstrata_summary$dht$individuals$N[Nhat_Sstrata_summary$dht$individuals$N$Label == "Total",]
###


#### plottingdbDS ####


### MBW v Inlet strata ####
#



Nhat_MBWvSinlets_ds <- Distance::ds(hbw_df, truncation="5%", key='hr', adjustment=NULL,
                                    transect="line",
                                    formula= ~CUE+SIGHTOBS,
                                    region_table = region.table_MBWvSinlets,# Sampled MBW strata, grouped, & sample Inlets, grouped
                                    sample_table = sample.table_MBWvSinlets,
                                    obs_table = obs.table_MBWvSinlets,
                                    er_var = "R2")

Nhat_MBWvSinlets_summary <- summary(Nhat_MBWvSinlets_ds)
#Nhat_MBWvSinlets_summary[["dht"]][["individuals"]][["N"]]
# (Nhat_MBWvSinlets <- Nhat_MBWvSinlets_summary$dht$individuals$N$Estimate[
#   Nhat_MBWvSinlets_summary$dht$individuals$N$Label == "Total"])
# (ucl_MBWvSinlets <- Nhat_MBWvSinlets_summary$dht$individuals$N$ucl[
#   Nhat_MBWvSinlets_summary$dht$individuals$N$Label == "Total"])
# (lcl_MBWvSinlets <- Nhat_MBWvSinlets_summary$dht$individuals$N$lcl[
#   Nhat_MBWvSinlets_summary$dht$individuals$N$Label == "Total"])
Nhat_MBWvSinlets_summary$dht$individuals$N[Nhat_MBWvSinlets_summary$dht$individuals$N$Label == "Total",]

###

### Grouped, no strata ####
#



Nhat_allasone_ds <- Distance::ds(hbw_df, truncation="5%", key='hr', adjustment=NULL,
                                 transect="line",
                                 formula= ~CUE+SIGHTOBS,
                                 region_table = region.table_allasone,#all sampled MBW and inlets, grouped together
                                 sample_table = sample.table_allasone,
                                 obs_table = obs.table_allasone,
                                 er_var = "R2")

Nhat_allasone_summary <- summary(Nhat_allasone_ds)
#Nhat_allasone_summary[["dht"]][["individuals"]][["N"]]
# (Nhat_allasone <- Nhat_allasone_summary$dht$individuals$N$Estimate[
#   Nhat_allasone_summary$dht$individuals$N$Label == "Total"])
# (ucl_allasone <- Nhat_allasone_summary$dht$individuals$N$ucl[
#   Nhat_allasone_summary$dht$individuals$N$Label == "Total"])
# (lcl_allasone <- Nhat_allasone_summary$dht$individuals$N$lcl[
#   Nhat_allasone_summary$dht$individuals$N$Label == "Total"])
Nhat_allasone_summary$dht$individuals$N[Nhat_allasone_summary$dht$individuals$N$Label == "Total",]
###
### ON SOAPFILM, grouped ####

#

Nhat_soap_ds <- Distance::ds(hbw_df, truncation="5%", key='hr', adjustment=NULL,
                             transect="line",
                             formula= ~CUE+SIGHTOBS,
                             region_table = region.table_soap,#all sampled MBW and inlets, grouped together
                             sample_table = sample.table_soap,
                             obs_table = obs.table_soap,
                             er_var = "R2")

Nhat_soap_summary <- summary(Nhat_soap_ds)
#Nhat_soap_summary[["dht"]][["individuals"]][["N"]]
# (Nhat_soap <- Nhat_soap_summary$dht$individuals$N$Estimate[
#   Nhat_soap_summary$dht$individuals$N$Label == "Total"])
# (ucl_soap <- Nhat_soap_summary$dht$individuals$N$ucl[
#   Nhat_soap_summary$dht$individuals$N$Label == "Total"])
# (lcl_soap <- Nhat_soap_summary$dht$individuals$N$lcl[
#   Nhat_soap_summary$dht$individuals$N$Label == "Total"])
Nhat_soap_summary$dht$individuals$N[Nhat_soap_summary$dht$individuals$N$Label == "Total",]




# SOAPfilm models  ####
## originally reportd models with original settings ####

#
tictoc::tic()
soapxy <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)),
              ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
              segment.data = Segdat_covs_soap,
              observation.data=hbw_Obs_t_soap,
              convert.units = 1/1000, knots=knots5kmgrid,
              family=tw(), method="REML")
save_model(soapxy)
soapxy_predvar <- dsm_var_gam(soapxy,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_predvar)
tictoc::toc()


tictoc::tic()
soapxy_sS <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                   s(slope, bs="ts"),
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy_sS)
soapxy_sS_pred <- predict(soapxy_sS,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_sS_pred)
soapxy_sS_predvar <- dsm_var_gam(soapxy_sS,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_sS_predvar)
tictoc::toc()

tictoc::tic()
soapxy_sSsO <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                     s(slope, bs="ts") +
                     s(ChannelType, bs="ts") ,
                   ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                   segment.data = Segdat_covs_soap,
                   observation.data=hbw_Obs_t_soap,
                   convert.units = 1/1000, knots=knots5kmgrid,
                   family=tw(), method="REML")
save_model(soapxy_sSsO)
soapxy_sSsO_pred <- predict(soapxy_sSsO,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_sSsO_pred)
soapxy_sSsO_predvar <- dsm_var_gam(soapxy_sSsO,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_sSsO_predvar)
tictoc::toc()

tictoc::tic()
soapxy_sSsOslD <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                        s(slope, bs="ts") +
                        s(ChannelType, bs="ts") +
                        s(logDEPTH, bs="ts") ,
                      ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                      segment.data = Segdat_covs_soap,
                      observation.data=hbw_Obs_t_soap,
                      convert.units = 1/1000, knots=knots5kmgrid,
                      family=tw(), method="REML")
save_model(soapxy_sSsOslD)
soapxy_sSsOslD_predvar <- dsm_var_gam(soapxy_sSsOslD,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_sSsOslD_predvar)
tictoc::toc()

tictoc::tic()
soapxy_sSsOslD2 <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                         s(slope, bs="ts") +
                         s(ChannelType, bs="ts") +
                         s(logDEPTH, bs="tp") ,
                       ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                       segment.data = Segdat_covs_soap,
                       observation.data=hbw_Obs_t_soap,
                       convert.units = 1/1000, knots=knots5kmgrid,
                       family=tw(), method="REML")
save_model(soapxy_sSsOslD2)
soapxy_sSsOslD2_predvar <- dsm_var_gam(soapxy_sSsOslD2,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_sSsOslD2_predvar)
tictoc::toc()

###


## original soapxy with higher ks  ####

## K=10,40,60,80
#
tictoc::tic()
soapxy10k <- dsm(abundance.est ~ s(x, y, bs="so", k=10, xt=list(bnd=buff_soapBnd_0NA, nmax=150)),
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy10k)
soapxy10k_predvar <- dsm_var_gam(soapxy10k,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy10k_predvar)
tictoc::toc()



tictoc::tic()
soapxy40k <- dsm(abundance.est ~ s(x, y, bs="so", k=40, xt=list(bnd=buff_soapBnd_0NA, nmax=150)),
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy40k)
soapxy40k_predvar <- dsm_var_gam(soapxy40k,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy40k_predvar)
tictoc::toc()


tictoc::tic()
soapxy60k <- dsm(abundance.est ~ s(x, y, bs="so", k=60, xt=list(bnd=buff_soapBnd_0NA, nmax=150)),
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy60k)
soapxy60k_predvar <- dsm_var_gam(soapxy60k,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy60k_predvar)
tictoc::toc()


###

## originals K=30  ####

#
tictoc::tic()
soapxy30k <- dsm(abundance.est ~ s(x, y, bs="so", k=30, xt=list(bnd=buff_soapBnd_0NA, nmax=150)),
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy30k)
soapxy30k_predvar <- dsm_var_gam(soapxy30k,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy30k_predvar)
tictoc::toc()


tictoc::tic()
soapxy30k_sS <- dsm(abundance.est ~ s(x, y, bs="so", k=30, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                      s(slope, bs="ts"),
                    ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                    segment.data = Segdat_covs_soap,
                    observation.data=hbw_Obs_t_soap,
                    convert.units = 1/1000, knots=knots5kmgrid,
                    family=tw(), method="REML")
save_model(soapxy30k_sS)
soapxy30k_sS_predvar <- dsm_var_gam(soapxy30k_sS,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy30k_sS_predvar)
tictoc::toc()

tictoc::tic()
soapxy30k_sSsO <- dsm(abundance.est ~ s(x, y, bs="so", k=30, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                        s(slope, bs="ts") +
                        s(ChannelType, bs="ts") ,
                      ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                      segment.data = Segdat_covs_soap,
                      observation.data=hbw_Obs_t_soap,
                      convert.units = 1/1000, knots=knots5kmgrid,
                      family=tw(), method="REML")
save_model(soapxy30k_sSsO)
soapxy30k_sSsO_predvar <- dsm_var_gam(soapxy30k_sSsO,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy30k_sSsO_predvar)
tictoc::toc()

tictoc::tic()
soapxy30k_sSsOslD <- dsm(abundance.est ~ s(x, y, bs="so", k=30, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                           s(slope, bs="ts") +
                           s(ChannelType, bs="ts") +
                           s(logDEPTH, bs="ts") ,
                         ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                         segment.data = Segdat_covs_soap,
                         observation.data=hbw_Obs_t_soap,
                         convert.units = 1/1000, knots=knots5kmgrid,
                         family=tw(), method="REML")
save_model(soapxy30k_sSsOslD)
soapxy30k_sSsOslD_predvar <- dsm_var_gam(soapxy30k_sSsOslD,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy30k_sSsOslD_predvar)
tictoc::toc()

tictoc::tic()
soapxy30k_sSsOslD2 <- dsm(abundance.est ~ s(x, y, bs="so", k=30, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                            s(slope, bs="ts") +
                            s(ChannelType, bs="ts") +
                            s(logDEPTH, bs="tp") ,
                          ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                          segment.data = Segdat_covs_soap,
                          observation.data=hbw_Obs_t_soap,
                          convert.units = 1/1000, knots=knots5kmgrid,
                          family=tw(), method="REML")
save_model(soapxy30k_sSsOslD2)
soapxy30k_sSsOslD2_predvar <- dsm_var_gam(soapxy30k_sSsOslD2,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy30k_sSsOslD2_predvar)
tictoc::toc()
###


## originals K=80  ####

#
tictoc::tic()
soapxy80k <- dsm(abundance.est ~ s(x, y, bs="so", k=80, xt=list(bnd=buff_soapBnd_0NA, nmax=150)),
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy80k)
soapxy80k_predvar <- dsm_var_gam(soapxy80k,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy80k_predvar)
tictoc::toc()


tictoc::tic()
soapxy80k_sS <- dsm(abundance.est ~ s(x, y, bs="so", k=80, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                       s(slope, bs="ts"),
                    ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                    segment.data = Segdat_covs_soap,
                    observation.data=hbw_Obs_t_soap,
                    convert.units = 1/1000, knots=knots5kmgrid,
                    family=tw(), method="REML")
save_model(soapxy80k_sS)
soapxy80k_sS_predvar <- dsm_var_gam(soapxy80k_sS,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy80k_sS_predvar)
tictoc::toc()

tictoc::tic()
soapxy80k_sSsO <- dsm(abundance.est ~ s(x, y, bs="so", k=80, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                         s(slope, bs="ts") +
                         s(ChannelType, bs="ts") ,
                      ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                      segment.data = Segdat_covs_soap,
                      observation.data=hbw_Obs_t_soap,
                      convert.units = 1/1000, knots=knots5kmgrid,
                      family=tw(), method="REML")
save_model(soapxy80k_sSsO)
soapxy80k_sSsO_predvar <- dsm_var_gam(soapxy80k_sSsO,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy80k_sSsO_predvar)
tictoc::toc()

tictoc::tic()
soapxy80k_sSsOslD <- dsm(abundance.est ~ s(x, y, bs="so", k=80, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                            s(slope, bs="ts") +
                            s(ChannelType, bs="ts") +
                            s(logDEPTH, bs="ts") ,
                         ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                         segment.data = Segdat_covs_soap,
                         observation.data=hbw_Obs_t_soap,
                         convert.units = 1/1000, knots=knots5kmgrid,
                         family=tw(), method="REML")
save_model(soapxy80k_sSsOslD)
soapxy80k_sSsOslD_predvar <- dsm_var_gam(soapxy80k_sSsOslD,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy80k_sSsOslD_predvar)
tictoc::toc()

tictoc::tic()
soapxy80k_sSsOslD2 <- dsm(abundance.est ~ s(x, y, bs="so", k=80, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                             s(slope, bs="ts") +
                             s(ChannelType, bs="ts") +
                             s(logDEPTH, bs="tp") ,
                          ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                          segment.data = Segdat_covs_soap,
                          observation.data=hbw_Obs_t_soap,
                          convert.units = 1/1000, knots=knots5kmgrid,
                          family=tw(), method="REML")
save_model(soapxy80k_sSsOslD2)
soapxy80k_sSsOslD2_predvar <- dsm_var_gam(soapxy80k_sSsOslD2,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy80k_sSsOslD2_predvar)
tictoc::toc()
###


## original models with linear terms covariates ####

#


tictoc::tic()
soapxy_S <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                  slope,
                ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                segment.data = Segdat_covs_soap,
                observation.data=hbw_Obs_t_soap,
                convert.units = 1/1000, knots=knots5kmgrid,
                family=tw(), method="REML")
save_model(soapxy_S)
soapxy_S_predvar <- dsm_var_gam(soapxy_S,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_S_predvar)
tictoc::toc()



tictoc::tic()
soapxy_SO <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                   slope +
                   ChannelType ,
                 ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                 segment.data = Segdat_covs_soap,
                 observation.data=hbw_Obs_t_soap,
                 convert.units = 1/1000, knots=knots5kmgrid,
                 family=tw(), method="REML")
save_model(soapxy_SO)
soapxy_SO_predvar <- dsm_var_gam(soapxy_SO,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_SO_predvar)
tictoc::toc()



tictoc::tic()
soapxy_SOlD <- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                     slope +
                     ChannelType +
                     logDEPTH ,
                   ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
                   segment.data = Segdat_covs_soap,
                   observation.data=hbw_Obs_t_soap,
                   convert.units = 1/1000, knots=knots5kmgrid,
                   family=tw(), method="REML")
save_model(soapxy_SOlD)
soapxy_SOlD_predvar <- dsm_var_gam(soapxy_SOlD,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)
save_model(soapxy_SOlD_predvar)
tictoc::toc()



###

### other covariates ####



SOlDld2s<- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                s(slope, bs="ts") +
                s(ChannelType, bs="ts") +
                s(logDEPTH, bs="ts") +
                s(d2s, bs="ts"),
             ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
             segment.data = Segdat_covs_soap,
             observation.data=hbw_Obs_t_soap,
             convert.units = 1/1000, knots=knots5kmgrid,
             family=tw(), method="REML")
SOlDld2s_predvar <- dsm_var_gam(SOlDld2s,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)

Ss<- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
                  s(slope, bs="ts"),# +
                  # s(ChannelType, bs="ts") +
                  # s(logDEPTH, bs="ts") +
                  # s(log(d2s)),
               ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
               segment.data = Segdat_covs_soap,
               observation.data=hbw_Obs_t_soap,
               convert.units = 1/1000, knots=knots5kmgrid,
               family=tw(), method="REML")
Ss_predvar <- dsm_var_gam(Ss,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)

Sct<- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
           # s(slope, bs="ts"),# +
          s(ChannelType, bs="ts"),# +
         # s(logDEPTH, bs="ts") +
         # s(log(d2s)),
         ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
         segment.data = Segdat_covs_soap,
         observation.data=hbw_Obs_t_soap,
         convert.units = 1/1000, knots=knots5kmgrid,
         family=tw(), method="REML")
Sct_predvar <- dsm_var_gam(Sct,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)


Sld2s<- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
             # s(slope, bs="ts"),# +
             #s(ChannelType, bs="ts"),# +
          # s(logDEPTH, bs="ts") +
          s(d2s, bs="ts"),
          ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
          segment.data = Segdat_covs_soap,
          observation.data=hbw_Obs_t_soap,
          convert.units = 1/1000, knots=knots5kmgrid,
          family=tw(), method="REML")
Sld2s_predvar <- dsm_var_gam(Sld2s,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)

SlD<- dsm(abundance.est ~ s(x, y, bs="so", k=5, xt=list(bnd=buff_soapBnd_0NA, nmax=150)) +
               # s(slope, bs="ts"),# +
               #s(ChannelType, bs="ts"),# +
                s(logDEPTH, bs="ts"),# +
               #s(log(d2s)),
            ddf.obj = hbw_hr_mcds_CUE_SIGHTOBS,
            segment.data = Segdat_covs_soap,
            observation.data=hbw_Obs_t_soap,
            convert.units = 1/1000, knots=knots5kmgrid,
            family=tw(), method="REML")
SlD_predvar <- dsm_var_gam(SlD,km2Predgrid_inner, off.set=km2Predgrid_inner$off.set)



cov_model_list <- mget(c("Sct", "Ss", "Sld2s", "SlD", "SOlDld2s"))
cov_predvar_list <- mget(c("Sct_predvar", "Ss_predvar", "Sld2s_predvar", "SlD_predvar", "SOlDld2s_predvar"))
# run this over each model in the list and make a table
cov_summary_table <- do.call(rbind, Map(summarize_dsm, cov_model_list,cov_predvar_list,names(cov_model_list)))

cov_summary_table
save(cov_summary_table, file=paste0(output.loc,"cov_summary_table.rda"))

# compare  ####
#
# make a named list of models
model_list <- mget(c(
  "soapxy", "soapxy_sS", "soapxy_sSsO", "soapxy_sSsOslD", "soapxy_sSsOslD2" #originals
  ,"soapxy_S", "soapxy_SO", "soapxy_SOlD" # original-linear predictors
  #,"soapxy10k",  "soapxy40k",  "soapxy60k", # soapxy higher k
  #,"soapxy30k", "soapxy30k_sS", "soapxy30k_sSsO", "soapxy30k_sSsOslD", "soapxy30k_sSsOslD2"# originals K=30
  ,"soapxy80k", "soapxy80k_sS", "soapxy80k_sSsO", "soapxy80k_sSsOslD", "soapxy80k_sSsOslD2"# originals K=80
))

predvar_list <- mget(c(
  "soapxy_predvar",  "soapxy_sS_predvar",  "soapxy_sSsO_predvar",
  "soapxy_sSsOslD_predvar","soapxy_sSsOslD2_predvar", #originals
  "soapxy_S_predvar",  "soapxy_SO_predvar",  "soapxy_SOlD_predvar", # original-linear predictors
  #"soapxy10k_predvar",  "soapxy40k_predvar",  "soapxy60k_predvar", # soapxy higher k
 # "soapxy30k_predvar",  "soapxy30k_sS_predvar",  "soapxy30k_sSsO_predvar",
 # "soapxy30k_sSsOslD_predvar",  "soapxy30k_sSsOslD2_predvar",# originals K=30
 # "soapxy80k_predvar",  "soapxy80k_sS_predvar",  "soapxy80k_sSsO_predvar",
  "soapxy80k_sSsOslD_predvar",  "soapxy80k_sSsOslD2_predvar"# originals K=80
))

# run this over each model in the list and make a table
dsm_summary_table <- do.call(rbind, Map(summarize_dsm, model_list,predvar_list,names(model_list)))

dsm_summary_table
save(dsm_summary_table, file=paste0(output.loc,"dsm_summary_table.rda"))

# sort models from lowest (best) to highest AIC
summary_table_AIC <- summary_table[order(summary_table$AIC),]

#separate model groups
originals_sumtab <- do.call(rbind,
                            Map(summarize_dsm,
                                 model=mget(c(
                                 "soapxy", "soapxy_sS", "soapxy_sSsO", "soapxy_sSsOslD", "soapxy_sSsOslD2")), #originals,
                                 predvar=mget(c(
                                 "soapxy_predvar",  "soapxy_sS_predvar",  "soapxy_sSsO_predvar",  "soapxy_sSsOslD_predvar",
                                 "soapxy_sSsOslD2_predvar")),
                                 names(mget(c(
                                 "soapxy", "soapxy_sS", "soapxy_sSsO", "soapxy_sSsOslD", "soapxy_sSsOslD2")))
                                       ))
originals_sumtab <- originals_sumtab[order(originals_sumtab$AIC),]
###

originals_lp <- do.call(rbind,
                            Map(summarize_dsm,
                                model=mget(c(
                                   "soapxy_S", "soapxy_SO", "soapxy_SOlD")), #originals- linear predictors,
                                predvar=mget(c(
                                   "soapxy_S_predvar",  "soapxy_SO_predvar",  "soapxy_SOlD_predvar")),
                                names(mget(c(
                                   "soapxy_S", "soapxy_SO", "soapxy_SOlD")))
                            ))
originals_lp <- originals_lp[order(originals_lp$AIC),]
###



originals_kk <- do.call(rbind,
                        Map(summarize_dsm,
                            model=mget(c(
                               "soapxy10k",  "soapxy40k",  "soapxy60k")), #originals- linear predictors,
                            predvar=mget(c(
                               "soapxy10k_predvar",  "soapxy40k_predvar",  "soapxy60k_predvar")),
                            names(mget(c(
                               "soapxy10k",  "soapxy40k",  "soapxy60k")))
                        ))
originals_kk <- originals_kk[order(originals_kk$AIC),]
###

originals_30k <- do.call(rbind,
                        Map(summarize_dsm,
                            model=mget(c(
                               "soapxy30k", "soapxy30k_sS", "soapxy30k_sSsO", "soapxy30k_sSsOslD", "soapxy30k_sSsOslD2")), #30k
                            predvar=mget(c(
                               "soapxy30k_predvar",  "soapxy30k_sS_predvar",  "soapxy30k_sSsO_predvar",
                               "soapxy30k_sSsOslD_predvar",  "soapxy30k_sSsOslD2_predvar")),
                            names(mget(c(
                               "soapxy30k", "soapxy30k_sS", "soapxy30k_sSsO", "soapxy30k_sSsOslD", "soapxy30k_sSsOslD2")))
                        ))
originals_30k <- originals_30k[order(originals_30k$AIC),]
###

originals_80k <- do.call(rbind,
                         Map(summarize_dsm,
                             model=mget(c(
                                "soapxy80k", "soapxy80k_sS", "soapxy80k_sSsO", "soapxy80k_sSsOslD", "soapxy80k_sSsOslD2")), #80k
                             predvar=mget(c(
                                "soapxy80k_predvar",  "soapxy80k_sS_predvar",  "soapxy80k_sSsO_predvar",
                                "soapxy80k_sSsOslD_predvar",  "soapxy80k_sSsOslD2_predvar")),
                             names(mget(c(
                                "soapxy80k", "soapxy80k_sS", "soapxy80k_sSsO", "soapxy80k_sSsOslD", "soapxy80k_sSsOslD2")))
                         ))
originals_80k <- originals_80k[order(originals_80k$AIC),]
###

originals_sumtab
originals_lp
originals_kk
originals_30k
originals_80k

summary_tab_grouped <- rbind(originals_sumtab,
                         originals_lp,
                         originals_kk,
                         originals_30k,
                         originals_80k)

save(summary_tab_grouped, file=paste0(output.loc,"summary_tab_grouped.rda"))


###

# record/plot concurvity for covaraiates  ####
library(cowplot)
#
#pdf(file= paste0(output.loc,"concurvity_plots.pdf"), width = 8, height = 6)
p1<-vis_concurvity(soapxy)
p2<-vis_concurvity(soapxy_sS)
p3<-vis_concurvity(soapxy_sSsO)
p4<-vis_concurvity(soapxy_sSsOslD)
p5<-vis_concurvity(soapxy_sSsOslD2)
#dev.off()

concurvity_plots <- plot_grid(p1,p2,p3,p4,p5, labels = c("a)", "b)", "c)", "d)", "e)"))
###
# re-Plotting: ####
library(sf)
library(ggplot2)

# Read shapefile ####
# Point to the .shp file (all associated files .shx, .dbf, etc. must be present)
SEAKstrata <- st_read("~/UAF/MS/GPTedits_supplemental-material-HAPODASdsmR/HAPODASdsmR/data/data/SEAKstrata.shp")

# Theme ####
map.theme2 <- theme(# legend
  legend.justification = c(1,1),
  legend.position = c(.95,.95),
  legend.spacing = unit(0,"mm"),
  # legend.background = element_rect(fill ="white"),
  legend.direction = "horizontal",
  #legend.key.size = unit(.03, "npc"), #change legend key size
  # # legend.key.height = unit(.5, 'cm'), #change legend key height
  legend.key.width = unit(.045, "npc"), #change legend key width
  legend.title = element_text( hjust=.5), #change legend title font size
  legend.text = element_text(size=10), #change legend text font size
  # pannel
  panel.background=element_rect(fill="white"),
  panel.grid = element_line(linetype = "blank"),
  # axis
  axis.title=element_text(size=12),
  axis.text=element_text(size=10),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(face="bold"))

# Plot dbDS Density, log(density), and CV ####

### Strata Plotting DF ####

dsStrata_df <- Nhat_Sstrata_summary$dht$individuals$D |>
  dplyr::select(Label, Estimate, cv) |>
  dplyr::rename(
    DIST_ID = Label,
    Density = Estimate,
    Dcv = cv
  )

dsStrata_df$Dens_adj <- dsStrata_df$Density
dsStrata_df$Dens_adj[dsStrata_df$Dens_adj == 0] <- 1e-10

dsStrata_df <- dsStrata_df |>
  mutate(
    Density = Density ,
    LogDensity4 = log(Dens_adj*4)
  )

SEAK_plot <- left_join(
  SEAKstrata,
  dsStrata_df,
  by = "DIST_ID"
)





### Density ####
ds_density_strata <- ggplot(SEAK_plot) +
  geom_sf(
    aes(fill = Density*4),
    color = "grey50",
    linewidth = 0.05
  ) +
  coord_sf(
    xlim = c(950000, 1480000),
    ylim = c(750000, 1210000),
    expand = FALSE,
    datum = st_crs(3338)
  ) +
  labs(
    fill = expression(paste("density/4 ", km^2)),
    x = "Easting (m)",
    y = "Northing (m)",
    #title = "dbDS"
  ) +
  scale_fill_viridis_c(
    option = "B",
    begin = .12,
    na.value = "grey85",
    limits = c(0,4),oob = scales::squish,
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top"
    )
  ) +
  map.theme2


ds_density_strata



### logDensity ####
ds_logdensity_strata <- ggplot(SEAK_plot) +
  geom_sf(
    aes(fill = LogDensity4)
  ) +
  coord_sf(
    xlim = c(950000, 1480000),
    ylim = c(750000, 1210000),
    expand = FALSE,
    datum = st_crs(3338)
  ) +
  labs(
    fill = expression(paste("log(density)/4 ", km^2)),
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  scale_fill_viridis_c(
    option = "B",
    na.value = "grey85",
    limits = c(-6.5, 1.5),oob = scales::squish,
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top"
    )
  ) +
  map.theme2


ds_logdensity_strata



## CV ####

ds_dCV_strata <- ggplot(SEAK_plot) +
  geom_sf(
    aes(fill = Dcv)
  ) +
  coord_sf(
    xlim = c(950000, 1480000),
    ylim = c(750000, 1210000),
    expand = FALSE,
    datum = st_crs(3338)
  ) +
  labs(
    fill = "prediction uncertainty\n(CV)",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  scale_fill_viridis_c(
    option = "H",
    na.value = "grey85",
    limits = c(0,2),oob = scales::squish,
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top"
    )
  ) +
  map.theme2

ds_dCV_strata


# Plot sf-DSM Densities ####

## sf plotting df ####
soapxy_sSsO <- readRDS(paste0(data.loc,"soapxy_sSsO.rds"))
soapxy_sSsO_predvar <- readRDS(paste0(data.loc,"soapxy_sSsO_predvar.rds"))
soapxy_sSsO_pred <- readRDS(paste0(data.loc,"soapxy_sSsO_pred.rds"))

dsmPred_df <- km2Predgrid_inner

dsmPred_df$pred <- as.numeric(soapxy_sSsO_pred$fit)
dsmPred_df$var  <- soapxy_sSsO_predvar$pred.var
dsmPred_df$se   <- as.numeric(soapxy_sSsO_pred$se.fit)

eps <- 1e-10
dsmPred_df$cv <- dsmPred_df$se/dsmPred_df$pred
dsmPred_df$log_pred <- log(dsmPred_df$pred + eps)



#### map window ####
xlim <- c(950000, 1480000)
ylim <- c(750000, 1210000)

### Density ####
soap_density_map <- ggplot(dsmPred_df) +
  geom_tile(aes(x = x, y = y, fill = pred)) +
  coord_sf(
    xlim = c(950000, 1480000),
    ylim = c(750000, 1210000),
    expand = FALSE,
    datum = st_crs(3338)
  ) +
  labs(
    fill = expression(paste("density/4 ", km^2)),
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  scale_fill_viridis_c(
    option = "B",
    begin = .12,
    na.value = "grey85",
    limits = c(0,4),oob = scales::squish,
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top"
    )
  ) +
  map.theme2

soap_density_map


### log density ####

soap_log_map <- ggplot(dsmPred_df) +
  geom_tile(aes(x = x, y = y, fill = log_pred)) +
  coord_sf(
    xlim = c(950000, 1480000),
    ylim = c(750000, 1210000),
    expand = FALSE,
    datum = st_crs(3338)
  ) +
  labs(
    fill = expression(paste("log(density)/4 ", km^2)),
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  scale_fill_viridis_c(
    option = "B",
    limits = c(-6.5, 1.5),oob = scales::squish,
    na.value = "grey85",
    guide = guide_colorbar(direction = "horizontal",
                           title.position = "top")
  ) +
  map.theme2

soap_log_map


### CV ####

soap_cv_map <- ggplot(dsmPred_df) +
  geom_tile(aes(x = x, y = y, fill = cv)) +
  coord_sf(
    xlim = c(950000, 1480000),
    ylim = c(750000, 1210000),
    expand = FALSE,
    datum = st_crs(3338)
  ) +
  labs(
    fill =  "prediction uncertainty\n(CV)",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  scale_fill_viridis_c(
    option = "H",
    limits = c(0, 2),oob = scales::squish,
    na.value = "grey85",
    guide = guide_colorbar(direction = "horizontal",
                           title.position = "top")
  ) +
  map.theme2 +
  theme(
    legend.position = c(0.95, 0.98))

soap_cv_map




## combo figure  ####
library(patchwork)
row1 <- soap_density_map + ds_density_strata +
  plot_layout(ncol = 2)

row2 <- soap_log_map + ds_logdensity_strata +
  plot_layout(ncol = 2)

row3 <- soap_cv_map + ds_dCV_strata +
  plot_layout(ncol = 2)

final_plot <- (row1 / row2 / row3) +
  plot_layout(guides = "keep") +
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "",
    tag_suffix = ")"
  ) &
  theme(
      plot.tag = element_text(face = "bold", size = 12)
    )



library(grid)

final_plot +
  plot_annotation(
    theme = theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10))
  ) &
  annotation_custom(
    grob = textGrob("sf-DSM", x = 0.25, y = 1.02, gp = gpar(fontface = "bold"))
  ) &
  annotation_custom(
    grob = textGrob("dbDS", x = 0.75, y = 1.02, gp = gpar(fontface = "bold"))
  )



final_plot  #pdf 10x13.8 inches





# figure DSM model ####
###### **add rugs to S and O!! fewer contour lines xy **
# library(viridis)

soapxy_sSsO_gamplot <- getViz(soapxy_sSsO)
soapxy_sSsO_xy <- plot(soapxy_sSsO_gamplot, select = 1 , n = 250, n2 = 500) +
   l_fitRaster()+
   l_fitContour(bins =7)+
   coord_equal(xlim=c(957817.9, 1464914.6), ylim=c(766888, 1201093)) +
   map.theme2 +
   scale_fill_viridis_c(na.value = "white")+
   labs(title= NULL,x= "Easting (m)", y = "Northing (m)", fill= "s(x,y)", subtitle = 'a)')+
   guides(fill=guide_colorbar(direction="horizontal",title.position = "top"))
soapxy_sSsO_xy + labs(subtitle = 'a)')



slopeB.col <- rocket(n=1, alpha = .45, begin=.8, end=1)
slopeL.col <- rocket(n=1, alpha = 1, begin=.6, end=1)

soapxy_sSsO_S <- plot(soapxy_sSsO_gamplot, select = 2)+ l_ciPoly(fill=slopeB.col )+
   l_fitLine(linetype = 1,size = 1, color=slopeL.col) + l_rug(alpha=.25)+
   labs(x= "Slope", y = "s(Slope),edf=0.94", subtitle='b)')+ map.theme2
   soapxy_sSsO_S

openB.col <- mako(n=1, alpha = .4, begin=.87, end=1)
openL.col <- mako(n=1, alpha = 1, begin=.75, end=1)

soapxy_sSsO_O <- plot(soapxy_sSsO_gamplot, select = 3)+ l_ciPoly(fill=openB.col)+
   l_fitLine(linetype = 1, size =1, color=openL.col) + l_rug(alpha=.25)+
   labs(x= "Openness", y = "s(Openness),edf=0.85",subtitle='c)')+map.theme2

soapxy_sSsO_O




























