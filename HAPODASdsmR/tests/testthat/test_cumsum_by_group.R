context("split HAPODAS transects: cumsum")
library(HAPODASdsmR)
#
# test_that("cumsum restarts for each unique label/onoff/section: micro",{
#       micro_interp <- interp_by_secs(micro)
#       micro_interp$cumdist <- cumsum_by_group(micro_interp$travel,
#                                               grpVarLst = list (micro_interp$label,
#                                                                 micro_interp$onoff,
#                                                                 micro_interp$section))
#       micro_cumdist_subset <- micro_interp$cumdist[c(1:5, 100:105, 200:205, 300:305, 400:405, 529:534)]
#       test_micro_cumdist <- c(0, 0, 5.05013001294118, 10.1002600258824, 15.1503900388235,
# 234.654977841, 239.647636944, 244.640296047, 249.63295515, 254.625614253,
# 259.618273356, 735.9959891358, 741.0655038312, 746.1350185266,
# 751.204533222, 756.2740479174, 761.3435626128, 1242.75246545928,
# 1247.79412398089, 1252.8357825025, 1257.87744102411, 1262.91909954572,
# 1267.96075806733, 184.28591762, 189.551229552, 194.816541484,
# 200.081853416, 205.347165348, 210.61247728, 576.978172333672,
# 581.970001001254, 586.961829668836, 591.953658336418, 596.945487004,
# 931.398007732)
#       expect_equal(micro_cumdist_subset, test_micro_cumdist)
# })
#
#
#
# test_that("cumsum restarts for each unique label/onoff/section: short",{
#       short_interp <- interp_by_secs(short)
#       short_interp$cumdist <- cumsum_by_group(short_interp$travel,
#                                               grpVarLst = list (short_interp$label,
#                                                                 short_interp$onoff,
#                                                                 short_interp$section))
#       short_cumdist_subset <- short_interp$cumdist[c(1:5, 2000:2005, 4000:4005, 6000:6005,
#                                                      8000:8005, 10000:10005, 10826:10830)]
#       test_short_cumdist <- c(4.8849468756, 9.7698937512, 14.6548406268, 19.5397875024, 24.424734378,
# 611.1578561248, 616.161706616867, 621.165557108933, 626.169407601,
# 631.173258093067, 636.177108585133, 2778.63982834267, 2783.35693691573,
# 2788.0740454888, 2792.79115406187, 2797.50826263493, 2802.225371208,
# 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3648.5090125576, 3653.5416702769,
# 3658.5743279962, 3663.6069857155, 3668.6396434348, 3673.6723011541,
# 7643.161341032, 7647.606141032, 7652.050941032, 0, 0)
#       expect_equal(short_cumdist_subset, test_short_cumdist)
# })
