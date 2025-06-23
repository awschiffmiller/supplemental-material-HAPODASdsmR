#' Cumulative sum by grouping variables
#'
#' Runs cumsum on x seperately for each unique combination of the grouping variables
#'
#' @param x vector (numeric or logical). If x is a column in a dataset, dataset must be identified (e.g: dataset$x)
#'
#' @param grpVarLst a `list` of grouping variables each of same length as x. If in dataset, dataset must be identified.
#'
#' @return Returns a vector of the same length as x. Each unique combination of grouping variables has it's own cumulative sum.
#'
#' @author Abigail Schiffmiller
#' @export
#'
#' @example
#'
#' micro_interp <- interp_by_secs(micro)
#' micro_interp$cum_dist <- cumsum_by_group(micro_interp$travel,
#'                                          grpVarLst = list (micro_interp$label,
#'                                          micro_interp$onoff, micro_interp$section))





cumsum_by_group <- function(x, grpVarLst){
   cumsum_vect <- stats::ave(x,
                  grpVarLst,
                  FUN = cumsum)
   cumsum_vect
}
