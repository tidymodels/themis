#' @details
#' SMOGN is a pre-processing approach for imbalanced regression. A relevance
#' function assigns each outcome value a relevance score, and values with a
#' relevance at or above `threshold` are treated as rare. The data is split
#' into contiguous bins of rare and common outcome values. Common bins are
#' under-sampled and rare bins are over-sampled toward a balanced size. New
#' rare examples are generated either by interpolating between an example and a
#' nearby neighbor (when they are close enough to be considered safe) or by
#' perturbing the example with Gaussian noise (when they are not), where the
#' amount of noise is controlled by `perturbation`.
#'
#' By default relevance is derived automatically from the boxplot extremes of
#' the outcome, giving the median a relevance of 0 and the extreme values a
#' relevance of 1. A matrix of relevance control points can instead be supplied
#' through `relevance`, with the first column giving outcome values and the
#' second column their relevance.
