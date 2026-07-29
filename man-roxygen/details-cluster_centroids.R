#' @details
#' Each class larger than the target count is summarized by running k-means on
#' the observations of that class, using as many clusters as the target count.
#' The class is then replaced by one representative per cluster:
#'
#' \describe{
#'   \item{`voting = "soft"`}{the cluster centroids themselves are used, so the
#'   returned observations are synthetic points that need not appear in the
#'   input.}
#'   \item{`voting = "hard"`}{the observation closest to each centroid is used,
#'   so all returned observations are real rows. The representative is always
#'   picked from the class being under-sampled.}
#' }
#'
#' This makes it the one *prototype generation* under-sampler in this package.
#' The other under-sampling methods perform *prototype selection*, keeping a
#' subset of the original rows.
#'
#' Because two clusters can share the same closest observation, `voting =
#' "hard"` can return slightly fewer observations than the target count.
