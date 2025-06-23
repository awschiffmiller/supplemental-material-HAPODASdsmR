context("split HAPODAS transects: seg_assign")
library(HAPODASdsmR)
#
# test_that("correct cutpoints: micro",{
#    micro_interp <- interp_by_secs(micro)
#    micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
#                                             grpVarLst = list (micro_interp$label,
#                                             micro_interp$onoff, micro_interp$section))
#    micro_segs <- seg_assign(data=micro_interp, seg.length = 75)
#    micromaxmin<- c(max(micro_segs$mod),min(micro_segs$mod))
#    expmaxmin <- c(74.889886545, 0)
#    expect_equal(micromaxmin, expmaxmin)
#    cutpointtab <- table(micro_segs$cutpoint)
#    exptab <- structure(c(`0` = 459L, End = 38L, Start = 37L), .Dim = 3L, .Dimnames = structure(list(
#     c("0", "End", "Start")), .Names = ""), class = "table")
#    expect_equal(cutpointtab,exptab)
#
#    })
#
#
# test_that("correct cutpoints: short",{
#    short_interp <- interp_by_secs(short)
#    short_interp$cum_dist <- cumsum_by_group(short_interp$travel,
#                                             grpVarLst = list (short_interp$label,
#                                             short_interp$onoff, short_interp$section))
#    short_segs <- seg_assign(data=short_interp, seg.length = 1000)
#    shortmaxmin<- c(max(short_segs$mod),min(short_segs$mod))
#    expmaxmin <- c(999.725291807999, 0)
#    expect_equal(shortmaxmin, expmaxmin)
#    cutpointtab <- table(short_segs$cutpoint)
#    exptab <- structure(c(`0` = 10771L, End = 29L, Start = 30L), .Dim = 3L, .Dimnames = structure(list(
#     c("0", "End", "Start")), .Names = ""), class = "table")
#    expect_equal(cutpointtab,exptab)
#
#    })

