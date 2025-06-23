context("split HAPODAS transects: seg_num")
library(HAPODASdsmR)
#
# test_that("number the segments: micro",{
#    micro_interp <- interp_by_secs(micro)
#    micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
#                                             grpVarLst = list (micro_interp$label,
#                                             micro_interp$onoff, micro_interp$section))
#    micro_segs <- seg_assign(data=micro_interp, seg.length = 75)
#    micro_segs <- seg_num(micro_segs)
#    segnumtab <- table(micro_segs$segnum)
#    exptab <- structure(c(`1` = 2L, `2` = 14L, `3` = 15L, `4` = 15L, `5` = 7L,
# `6` = 15L, `7` = 15L, `8` = 15L, `9` = 15L, `10` = 15L, `11` = 15L,
# `12` = 15L, `13` = 15L, `14` = 14L, `15` = 15L, `16` = 15L, `17` = 15L,
# `18` = 15L, `19` = 14L, `20` = 15L, `21` = 15L, `22` = 15L, `23` = 15L,
# `24` = 15L, `25` = 15L, `26` = 14L, `27` = 14L, `28` = 14L, `29` = 14L,
# `30` = 6L, `31` = 15L, `32` = 15L, `33` = 15L, `34` = 15L, `35` = 15L,
# `36` = 15L, `37` = 15L, `38` = 16L), .Dim = 38L, .Dimnames = structure(list(
#     c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#     "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
#     "22", "23", "24", "25", "26", "27", "28", "29", "30", "31",
#     "32", "33", "34", "35", "36", "37", "38")), .Names = ""), class = "table")
#    expect_equal(segnumtab,exptab)
#
#    })
#
#
# test_that("number the segments: short",{
#    short_interp <- interp_by_secs(short)
#    short_interp$cum_dist <- cumsum_by_group(short_interp$travel,
#                                             grpVarLst = list (short_interp$label,
#                                             short_interp$onoff, short_interp$section))
#    short_segs <- seg_assign(data=short_interp, seg.length = 1000)
#    short_segs <- seg_num(short_segs)
#    segnumtab <- table(short_segs$segnum)
#    exptab <- structure(c(`1` = 207L, `2` = 209L, `3` = 1456L, `4` = 206L,
# `5` = 209L, `6` = 207L, `7` = 206L, `8` = 200L, `9` = 193L, `10` = 343L,
# `11` = 200L, `12` = 198L, `13` = 212L, `14` = 213L, `15` = 219L,
# `16` = 263L, `17` = 178L, `18` = 209L, `19` = 200L, `20` = 207L,
# `21` = 206L, `22` = 3534L, `23` = 199L, `24` = 198L, `25` = 199L,
# `26` = 198L, `27` = 202L, `28` = 208L, `29` = 210L, `30` = 141L
# ), .Dim = 30L, .Dimnames = structure(list(c("1", "2", "3", "4",
# "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
# "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26",
# "27", "28", "29", "30")), .Names = ""), class = "table")
#    expect_equal(segnumtab,exptab)
#
#    })
