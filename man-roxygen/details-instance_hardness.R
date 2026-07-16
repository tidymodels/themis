#' @details
#' The instance hardness of each observation is estimated using the
#' k-Disagreeing Neighbors measure: the proportion of the nearest neighbors
#' that belong to a different class. Observations that are surrounded by points
#' of a different class are considered hard to classify. For each majority
#' class, the hardest observations are removed until the desired `under_ratio`
#' is reached.
