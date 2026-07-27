#' @details
#' This implements the NearMiss-1 algorithm. It retains the points from the
#' majority class which have the smallest mean distance to the nearest points
#' in the minority class.
#'
#' With more than two classes, the mean distance is computed to the nearest
#' points across all other classes, not only the minority class. This differs
#' from imbalanced-learn, which measures distance to the minority class only.
#' The binary case, the primary intended use, is unaffected.
