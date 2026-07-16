#' @details
#' Condensed Nearest Neighbors (CNN) is an under-sampling method that reduces
#' the majority classes to a consistent subset: a subset that classifies the
#' original data correctly using a 1-nearest-neighbor rule. It starts with a
#' "store" containing all minority class observations and one randomly chosen
#' majority class observation. It then repeatedly scans the remaining majority
#' class observations and moves any that are misclassified by a 1-nearest
#' neighbor fit on the current store into the store. This continues until a
#' full pass adds no new observations. The observations left outside the store
#' are removed.
#'
#' The smallest class is treated as the minority class and is always kept. CNN
#' tends to keep observations near the decision boundary while discarding
#' redundant interior observations. Because the seed observation and the scan
#' order are random, results depend on the random seed.
