#' @details
#' Edited Nearest Neighbors (ENN) is a cleaning method. For each observation it
#' finds the `neighbors` nearest neighbors and, if the class of the observation
#' does not match the majority class among those neighbors, the observation is
#' removed. This tends to remove noisy and borderline observations, which can
#' lead to smoother decision boundaries.
#'
#' Setting `times` greater than 1 applies ENN repeatedly, removing more noisy
#' and borderline observations on each pass and stopping early once a pass
#' removes nothing. This corresponds to Repeated Edited Nearest Neighbors
#' (RENN).
#'
#' Setting `all_k = TRUE` applies ENN with increasing numbers of neighbors, from
#' `1` up to `neighbors`, cleaning the data at each step. This corresponds to
#' All k-Nearest Neighbors (AllKNN) and takes precedence over `times`.
