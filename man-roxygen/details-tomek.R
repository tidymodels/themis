#' @details
#' A Tomek link is a pair of points from different classes that are each
#' other's nearest neighbors. Such pairs sit on or very near the decision
#' boundary and are considered noise or borderline cases. The algorithm
#' identifies all Tomek links and removes the majority class instance from each
#' pair, cleaning the class boundary without discarding non-boundary majority
#' examples. Because only boundary points are removed, this typically discards
#' far fewer observations than other under-sampling methods.
