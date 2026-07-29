#' @details
#' The `version` argument selects between the three NearMiss variants:
#'
#' \describe{
#'   \item{`version = 1`}{Retains the points from the majority class which
#'   have the smallest mean distance to their nearest points in the minority
#'   class.}
#'   \item{`version = 2`}{Retains the points from the majority class which
#'   have the smallest mean distance to their farthest points in the minority
#'   class.}
#'   \item{`version = 3`}{Works in two stages. First, the `n_neighbors_ver3`
#'   nearest majority class neighbors of each minority class point form a
#'   candidate pool, and all other majority class points are removed. Then the
#'   points of that pool which have the largest mean distance to their nearest
#'   minority class points are retained.}
#' }
#'
#' Since the size of the NearMiss-3 candidate pool is governed by
#' `n_neighbors_ver3` rather than by `under_ratio`, the pool can be smaller
#' than the target set by `under_ratio`. The whole pool is then retained and
#' the target is not reached.
#'
#' With more than two classes, the mean distance is computed to the nearest
#' points across all other classes, not only the minority class. This differs
#' from imbalanced-learn, which measures distance to the minority class only.
#' The binary case, the primary intended use, is unaffected.
