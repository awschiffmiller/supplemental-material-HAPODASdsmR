context("split HAPODAS transects: interp")
library(HAPODASdsmR)
#
# test_that("correct number of lines interpolated: micro",{
#    interp_micro <- interp_by_secs(micro)
#
#    tdiff_z <- sum(micro$tdiff == 0)
#    tdiff_sum <- sum(micro$tdiff)
#    last_rw <- micro$tdiff[nrow(micro)]-1
#    total_rows_exp <- tdiff_z + tdiff_sum - last_rw
#
#    expect_equal(nrow(micro), sum(interp_micro$orriginal == "TRUE"))
#    expect_equal(nrow(interp_micro), total_rows_exp)
# })
#
# test_that("correct number of lines interpolated: short",{
#    interp_short <- interp_by_secs(short)
#    tdiff_z <- sum(short$tdiff == 0)
#    tdiff_sum <- sum(short$tdiff)
#    last_rw <- short$tdiff[nrow(short)]-1
#    total_rows_exp <- tdiff_z + tdiff_sum - last_rw
#
#    expect_equal(nrow(short), sum(interp_short$orriginal == "TRUE"))
#    expect_equal(nrow(interp_short), total_rows_exp)
# })

