# supplemental-material-HAPODASdsmR
supplemental material: Using Density Surface Models in a Highly Complex Survey Area to Estimate Humpback Whale Distribution and Abundance- code and HAPODASdsmR package for splitting transect lines, creating DSM data tables, soapfilm creation.

HAPODASdsmR: 
Rpackage created to facilitate splitting line transect surveys for cetaceans in southeast Alaska (SEAK) into short segments and ad adapting resulting segmented data for use in a Density Surface Model (DSM) with soap film smoother.
developed for MS thesis work 2021-2023

The code and functions associated with segmenting the line transect data and building, from our raw survey data, the ‘seg_data’, ‘obs_data’, and ‘dist_data’ tables needed for the Rpackage `dsm` (Miller et al. 2021), as well as scripts used to run design based distance sampling models ont he same data are found in this repo. All code and models were originally written and run in R 4.1 with the `sp` package. portions of the code (as detailed below) have been updated for R 4.5 and the `sf` package.

The transect segmentation process (section 2.5.2 of manuscript) is covered in the `SplitTransects-vignette.Rmd`. This was written for `sp` and has not been updated for compatibility with `sf`.

The process for building DSM tables is covered by the `DSMdatatables.Rmd` vignette. The process as updated for `sf` is in the non-executable(without associated data) file `NONEX_updated_DS_data_tables_and_resubmission_reruns.R`.

Also in the vignettes folder, though not actually a vignette (without associated data available and not executable), the `supplement-hbw_DF_DSM.Rmd` document contains the code used to build the soap film surface, including creating the knot-grid and the buffer (section 2.5.1), and run DSM models with both smoothed and unsmoothed spatial (x/y) components, and various combinations of environmental covariates. The file `NONEX_EDITEDsupplement-hbw_DF_DSM.Rmd` is the one updated for `sf`.

The file `NONEX_updated_DS_data_tables_and_resubmission_reruns.R` also includes code for running DSM model variations with different formulations.




Miller DL, Rexstad E, Burt L, Bravington M V., Hedley S (2021) Dsm: Density Surface Modelling of Distance Sampling Data.

