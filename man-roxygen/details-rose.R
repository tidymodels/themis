#' @details
#' The factor variable used to balance around must only have 2 levels.
#'
#' The ROSE algorithm works by selecting an observation belonging to class k
#' and generating new examples in its neighborhood, which is determined by a
#' smoothing matrix H_k. Smaller values of `minority_smoothness` and
#' `majority_smoothness` shrink the entries of H_k, producing tighter
#' neighborhoods. This is a cautious choice when there is a concern that
#' excessively large neighborhoods could blur the boundaries between classes.
